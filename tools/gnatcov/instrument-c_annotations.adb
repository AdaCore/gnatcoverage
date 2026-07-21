------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2026, AdaCore                     --
--                                                                          --
-- GNATcoverage is free software; you can redistribute it and/or modify it  --
-- under terms of the GNU General Public License as published by the  Free  --
-- Software  Foundation;  either version 3,  or (at your option) any later  --
-- version. This software is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Directories; use Ada.Directories;

with GNAT.Regpat; use GNAT.Regpat;

with Clang.Index;    use Clang.Index;
with GNATCOLL.Utils; use GNATCOLL.Utils;

with Diagnostics;        use Diagnostics;
with Instrument.C_Utils; use Instrument.C_Utils;
with Instrument.Common;  use Instrument.Common;
with Slocs;              use Slocs;
with SS_Annotations;     use SS_Annotations;
with Outputs;

package body Instrument.C_Annotations is

   Annotation_Pattern : constant Pattern_Matcher :=
     Compile ("\s*(GNATCOV_([a-zA-Z0-9_]*))\s*(?:(\(.*\))|""(.*)"")?\s*");
   --  Pattern to match a comment with a GNATCOV_* directive.
   --
   --  * Group 1: Full GNATCOV_* annotation kind.
   --  * Group 2: Annotation kind without the GNATCOV_ prefix.
   --  * Group 3: Arguments (call syntax, possibly empty).
   --  * Group 4: Single argument (legacy syntax).

   function Slice_Sloc
     (Buffer      : Unbounded_String;
      Buffer_Sloc : Source_Location;
      Match       : Match_Location) return Source_Location;
   --  Considering that the first byte of Buffer has the given source location,
   --  return the source location corresponding to Match (designating a a slice
   --  in Buffer).

   -------------------
   -- Token scanner --
   -------------------

   Token_Pattern : constant Pattern_Matcher :=
     Compile
       ("(\s+)"
        & "|(,)"
        & "|(=)"
        & "|(\()"
        & "|(\))"
        & "|(""[^""]*"")"
        & "|([0-9]+)"
        & "|(false|true)"
        & "|([a-zA-Z_][a-zA-Z0-9_]*)"
        & "|(.+)");
   --  Pattern to match a token in an annotation argument list.
   --
   --  This is designed to always match non-empty strings. Groups 1..(Last-1)
   --  materialize a valid token (which group matches determines the kind of
   --  token that was found), and group Last is for invalid tokens.
   --
   --  * Group 1:  Whitespace.
   --  * Group 2:  Comma token.
   --  * Group 3:  Equal token.
   --  * Group 4:  Opening paren.
   --  * Group 5:  Closing paren.
   --  * Group 6:  String literal.
   --  * Group 7:  Integer literal.
   --  * Group 8:  Boolean literal
   --  * Group 9:  Identifier.
   --  * Group 10: Invalid token.

   Token_Pattern_Last_Group : constant Positive := Paren_Count (Token_Pattern);
   Token_Pattern_Last_Valid : constant Positive :=
     Token_Pattern_Last_Group - 1;

   --  Whitespace tokens are silently discarded during the scan, and so they
   --  are not added to the token vector.

   type Any_Token_Kind is
     (Whitespace,
      Comma,
      Equal,
      Opening_Paren,
      Closing_Paren,
      String_Literal,
      Integer_Literal,
      Boolean_Literal,
      Identifier);
   subtype Token_Kind is Any_Token_Kind range Comma .. Any_Token_Kind'Last;
   type Token_Data is record
      Kind  : Token_Kind;
      Match : Match_Location;
   end record;
   type Token_Index is new Positive;
   package Token_Vectors is new
     Ada.Containers.Vectors (Token_Index, Token_Data);

   procedure Scan_Tokens
     (Buffer      : Unbounded_String;
      Match       : Match_Location;
      Tokens      : out Token_Vectors.Vector;
      Error_Index : out Natural);
   --  Compute the list of tokens found in Buffer in the slice designated by
   --  Match.
   --
   --  On success, store this list of tokens in Tokens and set Error_Index to
   --  0. On scanning error, set Error_Index to the index in Buffer for the
   --  first byte that could not be analyzed.

   function Exists
     (Tokens : Token_Vectors.Vector; Index : Token_Index) return Boolean
   is (Index <= Tokens.Last_Index);
   --  Return whether Index corresponds to a valid token in Tokens

   function Matches
     (Tokens : Token_Vectors.Vector; Index : Token_Index; Kind : Token_Kind)
      return Boolean
   is (Exists (Tokens, Index)
       and then Tokens.Constant_Reference (Index).Kind = Kind);
   --  Return whether there is a token at Index in Tokens that has the givne
   --  kind.

   -----------------------
   -- Expression parser --
   -----------------------

   --  Expression parsing creates a syntax tree from a sequence of tokens
   --  extract from a given source buffer.
   --
   --  The syntax tree is stored as a vector of syntactic elements
   --  (Syntax_Data), each element being identified by an index (Syntax_Index).

   type Any_Syntax_Index is new Natural;
   subtype Syntax_Index is Any_Syntax_Index range 1 .. Any_Syntax_Index'Last;
   No_Syntax : constant Any_Syntax_Index := 0;
   type Syntax_Kind is
     (String_Literal,
      Integer_Literal,
      Boolean_Literal,
      Identifier,
      Aggregate,
      Aggregate_Assoc);
   type Syntax_Data (Kind : Syntax_Kind := Syntax_Kind'First) is record
      Token : Token_Index;

      case Kind is
         when String_Literal
            | Boolean_Literal
            | Integer_Literal
            | Identifier
         =>
            null;

         when Aggregate =>
            Aggregate_First : Any_Syntax_Index;

         when Aggregate_Assoc =>
            Assoc_Expr : Syntax_Index;
            Assoc_Next : Any_Syntax_Index;
      end case;
   end record;
   package Syntax_Vectors is new
     Ada.Containers.Vectors (Syntax_Index, Syntax_Data);

   procedure Parse_Expression
     (Buffer : Unbounded_String;
      Sloc   : Source_Location;
      Match  : Match_Location;
      Tokens : out Token_Vectors.Vector;
      Syntax : out Syntax_Vectors.Vector;
      Root   : out Syntax_Index);
   --  Parse the expression found in Buffer, in the slice designated by Match.
   --
   --  Sloc is the source location of the first byte in Buffer: it is used to
   --  emit diagnostics with precise location inside this buffer.
   --
   --  On success, fill out Tokens and Syntax with tokens and syntactic
   --  elements, and set Root to the index of the analyzed expression in
   --  Syntax.
   --
   --  On failure, emit a warning and raise an
   --  Invalid_Annotation_Argument_Error.

   function Aggregate_Length
     (Syntax : Syntax_Vectors.Vector; Self : Syntax_Index) return Natural;
   --  Retun the number of associations in the aggregate Self

   Buffer_Command_Pattern : constant Pattern_Matcher :=
     Compile
       ("^[\t ]*\/\* (?:"
        & "(GNATCOV_DUMP_BUFFERS (?:\((.+)\))?)"
        & "|(GNATCOV_RESET_BUFFERS)"
        & ") ?\*\/[ \t]*",
        Flags => Multiple_Lines);
   --  Regexp to match a buffer control command in a C/C++ comment.
   --
   --  Group 1: whole "dump" command
   --    Group 2: prefix for the source file to create
   --  Group 3: whole "reset" command

   Buffer_Dump_Group        : constant := 1;
   Buffer_Dump_Prefix_Group : constant := 2;
   Buffer_Reset_Group       : constant := 3;

   Line_Directive_Regexp : constant Pattern_Matcher :=
     Compile ("# (\d+) ""(\S+)""(?: \d)?");
   --  Pattern matching GNU style line directives, used in Remap_Locations.

   package Sloc_To_Index_Maps is new
     Ada.Containers.Ordered_Maps
       (Key_Type     => Local_Source_Location,
        Element_Type => Natural);

   procedure Remap_Locations
     (Str : String; Filename : String; Slocs : in out Sloc_To_Index_Maps.Map);
   --  Modify Slocs in place to update the mapped indices to reflect the
   --  position of the corresponding key source location in Str.
   --
   --  This procedure interprets GNU style line directives, making it usable on
   --  a preprocessed file.
   --
   --  This is mainly used at the moment to correctly insert buffer annotations
   --  at the right position in a preprocessed file:
   --
   --  Assuming we have an annotation to dump buffers at location LINE:COL.
   --
   --  The Replace_Manual_Indication procedure only works on raw file indices,
   --  it has no concept of lines or columns. We thus need to translate the
   --  LINE:COL location to an actual character offset after the start of the
   --  file.
   --
   --  We cannot do this on the original file however, as the
   --  Replace_Manual_Indication procedure rewrites a C file that has already
   --  been preprocessed.
   --
   --  We thus need this procedure to find the offset in Str (the preprocessed
   --  file content) that corresponds to line LINE in Filename (the original
   --  file) leveraging line directives to do so, then add the column of the
   --  location to that offset to obtain the actual offset in Str
   --  that corresponds to LINE:COL in Filename.

   ----------------
   -- Slice_Sloc --
   ----------------

   function Slice_Sloc
     (Buffer      : Unbounded_String;
      Buffer_Sloc : Source_Location;
      Match       : Match_Location) return Source_Location
   is
      Result : Source_Location := Buffer_Sloc;
      Line   : Natural renames Result.L.Line;
      Column : Natural renames Result.L.Column;
   begin
      for I in 1 .. Match.First - 1 loop
         case US.Element (Buffer, I) is
            when ASCII.LF =>
               Line := Line + 1;
               Column := 1;

            when ASCII.HT =>
               Column := (Column + 8) / 8 * 8;

            when others   =>
               Column := Column + 1;
         end case;
      end loop;
      return Result;
   end Slice_Sloc;

   -----------------
   -- Scan_Tokens --
   -----------------

   procedure Scan_Tokens
     (Buffer      : Unbounded_String;
      Match       : Match_Location;
      Tokens      : out Token_Vectors.Vector;
      Error_Index : out Natural)
   is
      Index    : Positive := Match.First;
      Matches  : Match_Array (0 .. Token_Pattern_Last_Group);
      Is_Valid : Boolean;
   begin
      Tokens.Clear;
      Error_Index := 0;

      while Index <= Match.Last loop

         --  Try to match a token starting at the current Index

         Strings.Match (Token_Pattern, Buffer, Matches, Index, Match.Last);
         pragma Assert (Matches (0) /= No_Match);

         --  See if the pattern matched for an actual token group

         Is_Valid := False;
         for I in 1 .. Token_Pattern_Last_Valid loop
            declare
               Kind : constant Any_Token_Kind := Any_Token_Kind'Val (I - 1);
            begin
               if Matches (I) /= No_Match then
                  if Kind /= Whitespace then
                     Tokens.Append (Token_Data'(Kind, Matches (I)));
                  end if;
                  Index := Matches (I).Last + 1;
                  Is_Valid := True;
                  exit;
               end if;
            end;
         end loop;

         --  If not, we have an error

         if not Is_Valid then
            pragma Assert (Matches (Token_Pattern_Last_Group) /= No_Match);
            Error_Index := Index;
            return;
         end if;
      end loop;
   end Scan_Tokens;

   ----------------------
   -- Parse_Expression --
   ----------------------

   procedure Parse_Expression
     (Buffer : Unbounded_String;
      Sloc   : Source_Location;
      Match  : Match_Location;
      Tokens : out Token_Vectors.Vector;
      Syntax : out Syntax_Vectors.Vector;
      Root   : out Syntax_Index)
   is
      Index : Token_Index;

      function Recurse return Any_Syntax_Index;
      --  Parse a sub-expression starting at Index

      procedure Error
        (Index : Token_Index; Message : String := "Invalid syntax")
      with No_Return;
      --  Emit a warning located at the given token, then raise an
      --  Invalid_Annotation_Argument_Error exception.

      -----------
      -- Error --
      -----------

      procedure Error
        (Index : Token_Index; Message : String := "Invalid syntax")
      is
         Buffer_Index : constant Positive :=
           (if Index <= Tokens.Last_Index
            then Tokens (Index).Match.First
            else Match.Last);
      begin
         Report
           (Slice_Sloc (Buffer, Sloc, (Buffer_Index, Buffer_Index)),
            Message,
            Warning);
         raise Invalid_Annotation_Argument_Error;
      end Error;

      -------------
      -- Recurse --
      -------------

      function Recurse return Any_Syntax_Index is
         T : Token_Data;
      begin
         if Index > Tokens.Last_Index then
            Error (Index, "unexpected end of annotation");
         end if;

         T := Tokens (Index);
         case T.Kind is
            when Comma | Equal | Closing_Paren =>
               Error (Index);

            when Opening_Paren                 =>

               --  Opening parens start an aggregate

               Syntax.Append
                 (Syntax_Data'(Aggregate, Token => Index, others => <>));
               declare
                  Result  : constant Syntax_Index := Syntax.Last_Index;
                  First   : Any_Syntax_Index := No_Syntax;
                  Current : Any_Syntax_Index := No_Syntax;
               begin
                  --  If we have a closing paren just after, the aggregate is
                  --  empty.

                  Index := Index + 1;
                  if Matches (Tokens, Index, Closing_Paren) then
                     Syntax (Result).Aggregate_First := First;
                     Index := Index + 1;
                     return Result;
                  end if;

                  loop
                     --  Create a new association for this aggregate. Unless
                     --  this is the first aggregate, add the forward link to
                     --  it.

                     Syntax.Append
                       (Syntax_Data'
                          (Kind       => Aggregate_Assoc,
                           Token      => Index,
                           Assoc_Expr => <>,
                           Assoc_Next => No_Syntax));
                     if Current /= No_Syntax then
                        Syntax (Current).Assoc_Next := Syntax.Last_Index;
                     end if;
                     Current := Syntax.Last_Index;
                     if First = No_Syntax then
                        First := Current;
                        Syntax (Result).Aggregate_First := First;
                     end if;

                     --  Parse the association expression

                     declare
                        Expr : constant Syntax_Index := Recurse;
                     begin
                        Syntax (Current).Assoc_Expr := Expr;
                     end;

                     --  If we have a closing paren, then this was the last
                     --  association.

                     if Matches (Tokens, Index, Closing_Paren) then
                        Index := Index + 1;
                        return Result;

                     --  If we have a comma, then we expect another
                     --  association.

                     elsif Matches (Tokens, Index, Comma) then
                        Index := Index + 1;

                     --  Anything else is an error

                     elsif Exists (Tokens, Index) then
                        Error (Index);
                     end if;
                  end loop;
               end;

            when String_Literal                =>
               Syntax.Append (Syntax_Data'(String_Literal, Index));
               Index := Index + 1;
               return Syntax.Last_Index;

            when Integer_Literal               =>
               Syntax.Append (Syntax_Data'(Integer_Literal, Index));
               Index := Index + 1;
               return Syntax.Last_Index;

            when Boolean_Literal               =>
               Syntax.Append (Syntax_Data'(Boolean_Literal, Index));
               Index := Index + 1;
               return Syntax.Last_Index;

            when Identifier                    =>
               Syntax.Append (Syntax_Data'(Identifier, Index));
               Index := Index + 1;
               return Syntax.Last_Index;
         end case;
      end Recurse;

      --  Start of processing for Parse_Expression

   begin
      --  First, scan all tokens into Tokens

      declare
         Error_Index : Natural;
      begin
         Scan_Tokens (Buffer, Match, Tokens, Error_Index);
         if Error_Index /= 0 then
            Report
              (Slice_Sloc (Buffer, Sloc, (Error_Index, Error_Index)),
               "Invalid syntax",
               Warning);
            raise Invalid_Annotation_Argument_Error;
         end if;
      end;
      Index := Tokens.First_Index;

      --  Parse one expression, and expect nothing after it

      Root := Recurse;
      if Exists (Tokens, Index) then
         Error (Index);
      end if;
   end Parse_Expression;

   ----------------------
   -- Aggregate_Length --
   ----------------------

   function Aggregate_Length
     (Syntax : Syntax_Vectors.Vector; Self : Syntax_Index) return Natural
   is
      Cursor : Any_Syntax_Index := Syntax (Self).Aggregate_First;
   begin
      return Result : Natural := 0 do
         while Cursor /= No_Syntax loop
            Result := Result + 1;
            Cursor := Syntax (Cursor).Assoc_Next;
         end loop;
      end return;
   end Aggregate_Length;

   --------------------------
   -- Populate_Annotations --
   --------------------------

   procedure Populate_Annotations (UIC : in out C_Unit_Inst_Context) is

      --  When processing a comment (in Process_Token below), source text and
      --  source location for it. Indexes in matches (Match_Location values)
      --  are relative to the Comment string.

      Comment : Unbounded_String;
      Sloc    : Source_Location;

      --  Tokens and syntax tree for argument lists when processing an
      --  annotation.

      Tokens : Token_Vectors.Vector;
      Syntax : Syntax_Vectors.Vector;

      function Get (Self : Match_Location) return String
      is (if Self = No_Match
          then ""
          else US.Slice (Comment, Self.First, Self.Last));
      --  Return the Comment slice corresponding to Self, or the empty string
      --  if there is no match.

      function Get (Self : Any_Syntax_Index) return String;
      --  Assuming that Self designates a single-token syntax node, return the
      --  corresponding source slice.

      function Slice_Sloc (Self : Match_Location) return Source_Location
      is (Slice_Sloc (Comment, Sloc, Self));
      --  Return the source location corresponding to the given match

      function Syntax_Sloc (Self : Syntax_Index) return Source_Location
      is (Slice_Sloc (Tokens (Syntax (Self).Token).Match));
      --  Likewise, for a given syntax node

      procedure Process_Token (Token : Token_T);
      --  Try to parse an annotation in the given token

      Last_Cov_Off : Source_Location := Slocs.No_Location;
      --  Track the source location of the previous GNATCOV_COV_OFF annotation.
      --  Used to detect GNATCOV_COV_OFF/GNATCOV_COV_ON pairs.

      ---------
      -- Get --
      ---------

      function Get (Self : Any_Syntax_Index) return String is
         T : constant Token_Data := Tokens (Syntax (Self).Token);
      begin
         return Get ((T.Match.First, T.Match.Last));
      end Get;

      -------------------
      -- Process_Token --
      -------------------

      procedure Process_Token (Token : Token_T) is

         --  Skip this token if it's not a comment in which we can find a
         --  GNATCOV_* marker.

         First   : Positive;
         Last    : Natural;
         Matches : Match_Array (0 .. Paren_Count (Annotation_Pattern));
         Kind    : Src_Annotation_Kind;
         Result  : ALI_Annotation;
      begin
         if Get_Token_Kind (Token) /= Token_Comment then
            return;
         end if;

         Comment := +Get_Token_Spelling (UIC.TU, Token);

         --  Extract the comment "content": remove the "//" prefix or the "/*"
         --  and "*/" boundaries.

         First := 1;
         Last := Length (Comment);
         if US.Slice (Comment, 1, 2) = "//" then
            First := First + 2;
         else
            pragma Assert (US.Slice (Comment, 1, 2) = "/*");
            pragma Assert (US.Slice (Comment, Last - 1, Last) = "*/");
            First := First + 2;
            Last := Last - 2;
         end if;
         Match (Annotation_Pattern, Comment, Matches, First, Last);
         if Matches (0) = No_Match then
            return;
         end if;
         Sloc := Instrument.C_Utils.Sloc (Get_Token_Location (UIC.TU, Token));

         --  Now extract the annotation kind

         declare
            Kind_Str : constant String := Get (Matches (2));
         begin
            Kind := Src_Annotation_Kind'Value (Kind_Str);
         exception
            when Constraint_Error =>
               Report
                 (Slice_Sloc (Matches (1)),
                  "Invalid Xcov annotation kind: " & Get (Matches (1)),
                  Warning);
               return;
         end;
         declare
            A : ALI_Annotation (Kind);
         begin
            Result := A;
         end;

         --  Now that the annotation kind is known, validate the remaining
         --  arguments expected for that kind.
         --
         --  Still support the legacy syntax, but emit a warning when used.

         if Matches (4) /= No_Match then
            declare
               Justification : constant String := Get (Matches (4));
            begin
               Report
                 (Slice_Sloc (Matches (1)),
                  "Obsolete syntax, support will be removed in release 28."
                  & " Consider switching to: "
                  & Get (Matches (1))
                  & "("""
                  & Justification
                  & """)",
                  Warning);
               if Result.Kind in Exempt_On | Cov_Off then
                  Result.Justification := +Justification;
               end if;
            end;

         elsif Kind = Dump_Buffers then

            --  GNATCOV_DUMP_BUFFERS takes an arbitrary C expression as its
            --  argument. We cannot reasonably parse it here, so we have to
            --  resort to a special case to analyze this annotation.

            Result.Prefix := +Get (Matches (3));

         else
            declare
               Root : Any_Syntax_Index := No_Syntax;
            begin
               --  First build the syntax tree for the argument list

               if Matches (3) /= No_Match then
                  Parse_Expression
                    (Comment, Sloc, Matches (3), Tokens, Syntax, Root);

                  --  We parse the expression only if the regular expression
                  --  matched the comment. This regular expression matches the
                  --  parens that surround the argument list, so we know that
                  --  we get an aggregate at the root expression.

                  pragma Assert (Syntax (Root).Kind = Aggregate);
               end if;

               --  Then decode individual arguments

               declare
                  Args_Count : constant Natural :=
                    (if Root = No_Syntax
                     then 0
                     else Aggregate_Length (Syntax, Root));
                  Args       : Annotation_Value_Array (1 .. Args_Count);
                  Next       : Any_Syntax_Index :=
                    (if Root = No_Syntax
                     then No_Syntax
                     else Syntax (Root).Aggregate_First);
               begin
                  for I in Args'Range loop
                     declare
                        E : constant Syntax_Index := Syntax (Next).Assoc_Expr;
                        S : constant Source_Location := Syntax_Sloc (E);
                     begin
                        case Syntax (E).Kind is
                           when String_Literal  =>

                              --  Get the content of the string without the
                              --  surrounding double quotes.

                              declare
                                 Value : constant String := Get (E);
                              begin
                                 Args (I) :=
                                   (String_Value,
                                    S,
                                    +Value
                                       (Value'First + 1 .. Value'Last - 1));
                              end;

                           when Integer_Literal =>
                              declare
                                 Value_Str : constant String := Get (E);
                                 Value     : Integer;
                              begin
                                 Value := Integer'Value (Value_Str);
                                 Args (I) := (Integer_Value, S, Value);
                              exception
                                 when Constraint_Error =>
                                    Report (S, "Too large integer", Warning);
                                    return;
                              end;

                           when Boolean_Literal =>
                              Args (I) := (Boolean_Value, S, Get (E) = "true");

                           when others          =>
                              Report (S, "Expression expected", Warning);
                              return;
                        end case;
                     end;
                     Next := Syntax (Next).Assoc_Next;
                  end loop;

                  --  Finally, extract annotation data from them

                  Parse_Annotation
                    (Kind       => Kind,
                     Sloc       => Sloc,
                     Args       => Args,
                     Annotation => Result);
               end;
            exception
               when Invalid_Annotation_Argument_Error =>
                  return;
            end;
         end if;

         --  Add an entry into UIC.Disable_Cov_Regions when needed

         case Result.Kind is
            when Cov_Off                      =>
               Last_Cov_Off := Sloc;

            when Cov_On                       =>
               if Last_Cov_Off /= Slocs.No_Location then
                  UIC.Disable_Cov_Regions.Append
                    (Source_Location_Range'
                       (Source_File => Sloc.Source_File,
                        L           =>
                          (First_Sloc => Last_Cov_Off.L,
                           Last_Sloc  => Sloc.L)));
               else
                  Last_Cov_Off := Slocs.No_Location;
               end if;

            when Fine_Grained_Annotation_Kind =>

               --  Fine grained exemptions go to the dedicated map only, not
               --  the general purpose annotaions map.

               Insert_Fine_Grained_Exemption
                 (UIC.Fine_Grained_Exemptions,
                  Result.Exemption_Req,
                  Result.Justification);
               return;

            when others                       =>
               null;
         end case;

         UIC.Annotations.Append (Annotation_Couple'(Sloc, Result));
      end Process_Token;

      --  Start of processing for Populate_Annotations
   begin
      Iterate_Tokens
        (UIC.TU, Get_Translation_Unit_Cursor (UIC.TU), Process_Token'Access);
   end Populate_Annotations;

   ---------------------
   -- Remap_Locations --
   ---------------------

   procedure Remap_Locations
     (Str : String; Filename : String; Slocs : in out Sloc_To_Index_Maps.Map)
   is
      use Sloc_To_Index_Maps;
      Current_Line : Natural := 1;
      --  Current line in the file

      Current_Line_Index : Positive := Str'First;
      --  Corresponding index of the first character of the line Current_Line

      Next_Line : Natural := 1;
      --  Next line in the file. Not all lines from the non-preprocessed file
      --  may be present, as the preprocessor may remove empty lines.

      Next_Line_Index : Positive := Str'First;
      --  Index of the first character of the line Next_Line.

      Col_Var : Natural;
      --  Variable used to get the proper column from the line start index.

      Next_Sloc : Cursor := Slocs.First;

      procedure Get_Next_Line;
      --  Update Next_Line and Next_Line_Index to point to the next line of
      --  Filename in Str, based on the current position in Current_Line
      --  and Current_Line_Index.

      -------------------
      -- Get_Next_Line --
      -------------------

      procedure Get_Next_Line is
         In_File : Boolean := True;
         --  Whether the current line belongs to Filename. Assume this is True
         --  when entering this procedure.

         In_Directive : Boolean := False;
         --  Whether the current line is a line directive. If so, we need to
         --  check the next line.

         Line_Matches : Match_Array (0 .. 2);
      begin
         Next_Line_Index := Current_Line_Index;
         Next_Line := Current_Line;
         loop
            Next_Line_Index := GNATCOLL.Utils.Next_Line (Str, Next_Line_Index);

            --  Parse the line directive, if this line contains one. This
            --  matches GCC-style directives, for instance:
            --
            --     # 9 "foo.c" 2

            if Str (Next_Line_Index) = '#'
              and then Next_Line_Index < Str'Last
              and then Str (Next_Line_Index + 1) = ' '
            then
               Match
                 (Self       => Line_Directive_Regexp,
                  Data       => Str,
                  Matches    => Line_Matches,
                  Data_First => Next_Line_Index,
                  Data_Last  => Line_End (Str, Next_Line_Index));
            else
               Line_Matches := (others => No_Match);
            end if;

            if Line_Matches (0) /= No_Match then
               In_Directive := True;

               declare
                  Directive_Filename : constant String :=
                    Interpret_Escape_Sequence
                      (Str (Line_Matches (2).First .. Line_Matches (2).Last));
                  Directive_Line     : constant Natural :=
                    Natural'Value
                      (Str (Line_Matches (1).First .. Line_Matches (1).Last));
               begin
                  --  Ignore line directives which state a '0' line, as they
                  --  don't provide any meaningful insight.

                  if Directive_Filename = Filename and then Directive_Line /= 0
                  then
                     Next_Line := Directive_Line;
                     In_File := True;
                  else
                     In_File := False;
                  end if;
               end;

            --  Otherwise, we only need to bump the line number if in the right
            --  file and if the previous line was not a directive.

            else
               if In_File and then not In_Directive then
                  Next_Line := Next_Line + 1;
               end if;
               In_Directive := False;
            end if;

            --  Exit when we found the first line that is not a line
            --  directive and that belongs to Filename, or we've reach the end
            --  of the file.

            exit when
              (not In_Directive and then In_File)
              or else Next_Line_Index = Str'Last;
         end loop;
         --  If the line index is Str'Last and Next_Line hasn't been modified,
         --  this means there are no more lines. Signal this by setting
         --  Next_Line to 0.

         if Next_Line_Index = Str'Last and then Next_Line = Current_Line then
            Next_Line := 0;
         end if;
      end Get_Next_Line;

   begin
      if not Has_Element (Next_Sloc) then
         return;
      end if;

      loop
         --  If the next line in the file is the same as the one of Next_Sloc,
         --  Compute the index of the corresponding column.

         if Key (Next_Sloc).Line = Next_Line then
            Col_Var := Next_Line_Index;
            Skip_To_Column (Str, Key (Next_Sloc).Column, Col_Var);
            Slocs.Replace_Element (Next_Sloc, Col_Var);
            Next (Next_Sloc);

         --  If the next line in the file is past the next source location
         --  (e.g. the source location designates a whitespace-only line that
         --  got removed by the preprocessor), associate the sloc to the end of
         --  the last line we have for this file.
         --
         --  This could result in annotations being tied to an earlier location
         --  than actually specified, but with no differences in the
         --  obligations included in the range.
         --
         --  This has not yet been seen in practice.

         elsif Next_Line > Key (Next_Sloc).Line then
            Col_Var := Current_Line_Index;
            Slocs.Replace_Element (Next_Sloc, Line_End (Str, Col_Var));
            Next (Next_Sloc);

         else
            --  Otherwise just move to the next line
            Current_Line := Next_Line;
            Current_Line_Index := Next_Line_Index;
            Get_Next_Line;
         end if;
         exit when not Has_Element (Next_Sloc) or else Next_Line = 0;
      end loop;
      --  If there still are Slocs we have not remapped when reaching the end
      --  of the file, associate them with index 0.

      while Has_Element (Next_Sloc) loop
         Slocs.Replace_Element (Next_Sloc, 0);
         Next (Next_Sloc);
      end loop;
   end Remap_Locations;

   --------------------------
   -- Populate_Annotations --
   --------------------------

   procedure Populate_Annotations
     (Filename    : String;
      Buffer      : String;
      Annotations : out Index_To_Annotation_Maps.Map)
   is
      use Sloc_To_Index_Maps;

      --  Populate the Annotations map from external annotations

      Ext_Annotations : constant Instr_Annotation_Map :=
        Get_Buffer_Annotations (Filename);
      Slocs_To_Index  : Sloc_To_Index_Maps.Map;
   begin
      for Cur in Ext_Annotations.Iterate loop
         Slocs_To_Index.Insert (Instr_Annotation_Maps.Key (Cur), 0);
      end loop;
      Remap_Locations (Buffer, Filename, Slocs_To_Index);

      for Cur in Ext_Annotations.Iterate loop
         declare
            Sloc    : constant Local_Source_Location :=
              Instr_Annotation_Maps.Key (Cur);
            Instr_A : constant Instr_Annotation :=
              Instr_Annotation_Maps.Element (Cur);
            ALI_A   : ALI_Annotation (Instr_A.Kind);
            Index   : constant Natural := Slocs_To_Index (Sloc);
         begin
            if Index = 0 then
               Outputs.Warn
                 (Filename
                  & " has no "
                  & Image (Sloc)
                  & " location: ignoring"
                  & " the "
                  & Instr_A.Kind'Image
                  & " external annotation");
               goto Continue;
            end if;
            case Buffers_Annotation_Kind (Instr_A.Kind) is
               when Dump_Buffers  =>
                  ALI_A.Prefix :=
                    (if Instr_A.Trace_Prefix = ""
                     then Null_Unbounded_String
                     else Instr_A.Trace_Prefix);

               when Reset_Buffers =>
                  null;
            end case;
            Annotations.Insert
              ((Buffer_First => Index,
                Priority     => Natural (Annotations.Length),
                Buffer_Next  => Index),
               ALI_A);
         end;
         <<Continue>>
      end loop;

      --  Then populate it from directives found in the source code

      declare
         Index   : Positive := Buffer'First;
         Matches : Match_Array (0 .. 4);
         A       : ALI_Annotation;
      begin
         loop
            Match
              (Buffer_Command_Pattern, Buffer (Index .. Buffer'Last), Matches);
            exit when Matches (0) = No_Match;

            --  Move the index forward so that we are ready for the next
            --  match.

            Index := Matches (0).Last + 1;

            if Matches (Buffer_Dump_Group) /= No_Match then
               Switches.Misc_Trace.Trace
                 ("Found buffer dump indication in file "
                  & Simple_Name (Filename));

               --  If we had a prefix specified in the comment, include it
               --  in the dump procedure call. Use the project name as the
               --  prefix otherwise.

               declare
                  Prefix : constant Unbounded_String :=
                    (if Matches (Buffer_Dump_Prefix_Group) = No_Match
                     then Null_Unbounded_String
                     else
                       +Buffer
                          (Matches (Buffer_Dump_Prefix_Group).First
                           .. Matches (Buffer_Dump_Prefix_Group).Last));
               begin
                  A := (Kind => Dump_Buffers, Prefix => Prefix);
               end;

            else
               pragma Assert (Matches (Buffer_Reset_Group) /= No_Match);
               Switches.Misc_Trace.Trace
                 ("Found buffer reset indication in file " & Filename);

               A := (Kind => Reset_Buffers);
            end if;

            Annotations.Insert
              ((Buffer_First => Matches (0).First,
                Priority     => Natural (Annotations.Length),
                Buffer_Next  => Index),
               A);
         end loop;
      end;
   end Populate_Annotations;

end Instrument.C_Annotations;
