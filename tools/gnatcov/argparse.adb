------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2015, AdaCore                       --
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

with Ada.Command_Line;
with Ada.Containers;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Outputs;

package body Argparse is

   function "+" (S : String) return Unbounded_String
                 renames To_Unbounded_String;

   function "+" (US : Unbounded_String) return String
                 renames To_String;

   type Option_Info_Access is access constant Option_Info'Class;
   type Option_Info_Array is array (Natural range <>) of Option_Info_Access;

   package Option_Reference_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Option_Reference);

   type Option_Reference_Vector is access Option_Reference_Vectors.Vector;
   procedure Free is new Ada.Unchecked_Deallocation
     (Option_Reference_Vectors.Vector, Option_Reference_Vector);

   function Hash (C : Character) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (Character'Pos (C)));

   package Short_Options_Index is new Ada.Containers.Hashed_Maps
     (Key_Type        => Character,
      Hash            => Hash,
      Element_Type    => Option_Reference_Vector,
      Equivalent_Keys => "=");

   package Long_Options_Index is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Hash            => Ada.Strings.Unbounded.Hash,
      Element_Type    => Option_Reference_Vector,
      Equivalent_Keys => "=");

   type Parse_Table is record
      Short_Index : Short_Options_Index.Map;
      --  Options sorted by short name letter. Multiple options can have
      --  similar short names, so the map contains vectors of options.

      Long_Index  : Long_Options_Index.Map;
      --  Options sorted by long name. Multiple options can have similar long
      --  names, so the map contains vectors of options.
   end record;
   --  Helper data structure for parsing

   type Parser_Record is record
      Command_Infos        : Command_Info_Array;

      Bool_Info            : Bool_Option_Info_Array;
      String_Info          : String_Option_Info_Array;
      String_List_Info     : String_List_Option_Info_Array;

      Bool_Callback        : Bool_Callback_Type;
      String_Callback      : String_Callback_Type;
      String_List_Callback : String_List_Callback_Type;
      Arg_Callback         : Arg_Callback_Type;

      Table                : Parse_Table;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Parser_Record, Parser_Type);

   function Option_Name (Option : Option_Info'Class) return String;
   --  Shortcut for Option_Name (Ref_For_Option)

   function Get_Option
     (Parser : Parser_Type;
      Ref    : Option_Reference)
      return Option_Info_Access;
   --  Return an access to the Option_Info record corresponding to Ref in
   --  Parser.

   function Split_Option_Text (S : String) return String_Vectors.Vector;
   --  Split |-separated lists of option names, return a vector holding option
   --  names.

   procedure Build_Parse_Table (Parser : Parser_Type);
   --  Initialize Parser.Table from Parser.*_Info. Raise a Program_Error if
   --  option names are invalid/conflicting.

   function Sorted_Options
     (Parser        : Parser_Type;
      With_Internal : Boolean;
      For_Command   : Command_Type := No_Command)
      return Option_Info_Array;
   --  Return a list of options, sorted by long and then short names.
   --  With_Internal and For_Command are used to filter the set of options
   --  to return: they have the same meaning as in Print_Usage.

   function Wrap
     (Text              : String;
      First_Line_Indent : Natural;
      Next_Lines_Indent : Natural;
      Width             : Natural)
      return String;
   --  Wrap each line in Text on Width columns (0). Each line will be prefixed
   --  with First_Line_Indent spaces (1), plus Next_Line_Indent ones (2) for
   --  all but the first line and plus any existing spaces prefix for each
   --  line (3). All other contiguous spaces are merged otherwise (4).
   --
   --  To clarify, here are a few examples:
   --
   --   (0) Wrap ("Text text", 0, 0, 7) -> "Text" & ASCII.LF & "text"
   --   (1) Wrap ("Text", 2, 0, 80) -> "  Text"
   --   (2) Wrap ("Text text", 2, 3, 7) -> "  Text" & ASCII.LF & "     text"
   --   (3) Wrap ("  Text", 1, 0, 80) -> "   Text"
   --   (4) Wrap ("  Text      text", 0, 0, 80) -> "  Text text"

   ------------------
   -- Command_Name --
   ------------------

   function Command_Name
     (Parser  : Parser_Type;
      Command : Valid_Commands)
      return String
   is
     (To_String (Parser.Command_Infos (Command).Name));

   -----------------
   -- Option_Name --
   -----------------

   function Option_Name
     (Parser : Parser_Type;
      Ref    : Option_Reference)
      return String
   is
     (Option_Name (Get_Option (Parser, Ref).all));

   -----------------
   -- Option_Name --
   -----------------

   function Option_Name (Option : Option_Info'Class) return String is
      Result : Unbounded_String;
   begin
      if Length (Option.Long_Name) > 0 then
         Append (Result, Option.Long_Name);
      end if;
      if Length (Option.Short_Name) > 0 then
         if Length (Result) > 0 then
            Append (Result, '|');
         end if;
         Append (Result, Option.Short_Name);
      end if;
      return +Result;
   end Option_Name;

   ----------------
   -- Get_Option --
   ----------------

   function Get_Option
     (Parser : Parser_Type;
      Ref    : Option_Reference)
      return Option_Info_Access
   is
     (case Ref.Kind is
         when Bool_Opt => Parser.Bool_Info (Ref.Bool_Option)'Access,
         when String_Opt => Parser.String_Info (Ref.String_Option)'Access,
         when String_List_Opt =>
            Parser.String_List_Info (Ref.String_List_Option)'Access);

   ------------
   -- Create --
   ------------

   function Create
     (Name        : String;
      Pattern     : String := "";
      Description : String;
      Internal    : Boolean)
      return Command_Info
   is
     (+Name, +Pattern, +Description, Internal);

   ------------
   -- Create --
   ------------

   function Create
     (Long_Name, Short_Name, Help : String := "";
      Commands                    : Command_Set := All_Commands;
      Internal                    : Boolean)
      return Bool_Option_Info
   is
     ((+Long_Name, +Short_Name, +Help, Commands, Internal));

   ------------
   -- Create --
   ------------

   function Create
     (Long_Name, Short_Name, Help : String := "";
      Commands                    : Command_Set := All_Commands;
      Internal, At_Most_Once      : Boolean;
      Pattern                     : String := "")
      return String_Option_Info
   is
     (+Long_Name, +Short_Name, +Help, Commands, Internal, At_Most_Once,
      +Pattern);

   ------------
   -- Create --
   ------------

   function Create
     (Long_Name, Short_Name, Help : String := "";
      Commands                    : Command_Set := All_Commands;
      Internal                    : Boolean;
      Greedy                      : Boolean := False;
      Pattern                     : String := "")
      return String_List_Option_Info
   is
     (+Long_Name, +Short_Name, +Help, Commands, Internal, Greedy, +Pattern);

   ------------
   -- Create --
   ------------

   function Create
     (Command_Infos        : Command_Info_Array;
      Bool_Infos           : Bool_Option_Info_Array;
      String_Infos         : String_Option_Info_Array;
      String_List_Infos    : String_List_Option_Info_Array;

      Bool_Callback        : Bool_Callback_Type := null;
      String_Callback      : String_Callback_Type := null;
      String_List_Callback : String_List_Callback_Type := null;
      Arg_Callback         : Arg_Callback_Type := null)
      return Parser_Type
   is
      Result : constant Parser_Type := new Parser_Record'
        (Command_Infos,
         Bool_Infos, String_Infos, String_List_Infos,
         Bool_Callback, String_Callback, String_List_Callback, Arg_Callback,
         Table => <>);
   begin
      Build_Parse_Table (Result);
      return Result;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Parser : in out Parser_Type) is
   begin
      for Ref of Parser.Table.Short_Index loop
         Free (Ref);
      end loop;
      for Ref of Parser.Table.Long_Index loop
         Free (Ref);
      end loop;
      Free (Parser);
   end Destroy;

   -----------------
   -- Print_Usage --
   -----------------

   procedure Print_Usage
     (Parser        : Parser_Type;
      With_Internal : Boolean;
      Short         : Boolean;
      For_Command   : Command_Type := No_Command)
   is
      --  If we are printing the usage of an internal command, we obviously
      --  want to display internal options.

      With_Internal_Options : constant Boolean :=
        (For_Command /= No_Command
            and then Parser.Command_Infos (For_Command).Internal)
         or else With_Internal;

      Option_Infos : constant Option_Info_Array :=
        Sorted_Options (Parser, With_Internal_Options, For_Command);

      function Get_Pattern (Info : Option_Info'Class) return String;
      --  Return a pattern to be used in the help for the option corresponding
      --  to Info.
      --
      --    * For boolean options, return an empty string.
      --    * For single string options and multiple strings ones, return its
      --      provided pattern or " [...]" if the option has none.

      procedure Put_Wrapped
        (Text : String;
         First_Line_Indent : Natural;
         Next_Lines_Indent : Natural);
      --  Put on standard output Text wrapped on 80 columns according to
      --  First_Line_Indent and Next_Lines_Indent (see Wrap).

      -----------------
      -- Get_Pattern --
      -----------------

      function Get_Pattern (Info : Option_Info'Class) return String is
         Candidate : Unbounded_String;
      begin
         if Info in String_Option_Info'Class then
            Candidate := String_Option_Info (Info).Pattern;
         elsif Info in String_List_Option_Info'Class then
            Candidate := String_List_Option_Info (Info).Pattern;
         else
            return "";
         end if;

         return (if Length (Candidate) = 0
                 then " [...]"
                 else " " & (+Candidate));
      end Get_Pattern;

      -----------------
      -- Put_Wrapped --
      -----------------

      procedure Put_Wrapped
        (Text : String;
         First_Line_Indent : Natural;
         Next_Lines_Indent : Natural)
      is
      begin
         Put (Wrap (Text, First_Line_Indent, Next_Lines_Indent, 80));
      end Put_Wrapped;

      First : Boolean;
   begin
      if For_Command = No_Command then
         --  Describe commands first, or one command if asked so

         Put_Line
           ("Usage: " & Ada.Command_Line.Command_Name
            & " ACTION [OPTIONS ...]");

         if Short then
            Put_Line
              ("Run '" & Ada.Command_Line.Command_Name
               & " --help' for more information.");
            return;
         end if;

         New_Line;
         Put_Line ("ACTION is one of:");

         First := True;
         for Info of Parser.Command_Infos loop
            if not Info.Internal or else With_Internal then
               if First then
                  New_Line;
                  First := False;
               end if;
               Put_Wrapped (+Info.Name & " " & (+Info.Pattern), 2, 6);
               Put_Wrapped (+Info.Description, 4, 4);
               New_Line;
            end if;
         end loop;

      else
         declare
            Info : Command_Info renames Parser.Command_Infos (For_Command);
         begin
            Put_Wrapped ("Usage: " & Ada.Command_Line.Command_Name & " "
                         & (+Info.Name) & " " & (+Info.Pattern), 0, 4);

            if Short then
               Put_Line
                 ("Run '" & Ada.Command_Line.Command_Name & " " & (+Info.Name)
                  & " --help' for more information.");
               return;
            end if;

            New_Line;
            Put_Wrapped (+Info.Description, 2, 2);
            New_Line;
         end;
      end if;

      --  And then all options or only the ones that apply to For_Command

      Put_Line ("The following options are supported:");
      First := True;
      for Info of Option_Infos loop
         if First then
            New_Line;
            First := False;
         end if;
         declare
            Pattern : constant String := Get_Pattern (Info.all);
         begin
            --  Display all names for this option

            for Name of Split_Option_Text (+Info.Long_Name) loop
               Put_Wrapped (+Name & Pattern, 2, 6);
            end loop;
            for Name of Split_Option_Text (+Info.Short_Name) loop
               Put_Wrapped (+Name & Pattern, 2, 6);
            end loop;

            --  If this option is not available for all commands, display
            --  which commands accept it.

            if Info.Commands /= All_Commands then
               declare
                  Set_Image : Unbounded_String;
                  First     : Boolean := True;
               begin
                  Append (Set_Image, '[');
                  for Cmd in Valid_Commands'Range loop
                     if Info.Commands (Cmd)
                       and then
                         (not Parser.Command_Infos (Cmd).Internal
                          or else
                          With_Internal)
                     then
                        if not First then
                           Append (Set_Image, ", ");
                        end if;
                        Append (Set_Image, Parser.Command_Infos (Cmd).Name);
                        First := False;
                     end if;
                  end loop;
                  Append (Set_Image, ']');
                  Put_Wrapped (+Set_Image, 4, 5);
               end;
            end if;

            Put_Wrapped (+Info.Help, 4, 4);
            New_Line;
         end;
      end loop;
   end Print_Usage;

   -----------
   -- Parse --
   -----------

   function Parse
     (Parser       : Parser_Type;
      Args         : GNAT.Strings.String_List_Access;
      With_Command : Command_Type := No_Command;
      Callback     : access procedure (Result : in out Parsed_Arguments;
                                       Ref    : Option_Reference) := null)
      return Parsed_Arguments
   is
      I       : Positive := Args'First;
      Result  : Parsed_Arguments;

      function Error (S : String) return Parsed_Arguments is
        ((Error => +S, Command => Result.Command, others => <>));

      Arg_Error : exception;
      --  Exception to raise in order to abort the parsing process. The
      --  associated text is used to yield the error message in the result.

      type Slice is record
         First, Last : Natural;
      end record;
      --  Slice of the current argument (i.e. Args (I))

      procedure Split_Arg
        (Arg           : String;
         Option, Value : out Slice;
         Has_Value     : out Boolean);
      --  Try to split Arg into Option and Value. For instance, if Arg is
      --  "--foo=bar", Option will refer to "--foo" while Value will refer
      --  to "bar". Set Has_Value to whether there is a "=" substring.

      function Consume_Value
        (For_Arg : Slice;
         I       : in out Natural;
         J       : Natural)
         return String;
      --  Considering the For_Arg slice (which is the option part of the
      --  currently parsed argument Args(I)), try to fetch a value for the
      --  option wthat was just parsed. Look for any value starting at Args
      --  (I).all (J): if there is no value there, fetch the next argument and
      --  update I accordingly. In the latter case, raise an Arg_Error if there
      --  is no argument left.

      function Consume_Next_Arg
        (For_Arg : Slice;
         I       : in out Natural)
         return GNAT.Strings.String_Access;
      --  Fetch the next argument and return an access to it. Update I
      --  accordingly. Raise an Arg_Error if there is no argument left.

      function Select_Option
        (Opt_Name : String;
         Opt_Vec  : Option_Reference_Vector)
         return Option_Reference;
      --  Given Opt_Name (the currently parsed option) and Opt_Vec (a set of
      --  candidates for a match), return the option that is accepted by the
      --  matched subcommand (Result.Command).

      function Invoke_Callback (Ref : Option_Reference) return Boolean;
      --  If callback is provided, call it for Ref. Return whether it yielded
      --  an error (in which case, parsing must be stopped).

      procedure Handle_Bool (Opt : Bool_Options);
      --  Set the boolean for Opt to True in Result. Previously invoke the
      --  boolean option callback if provided.

      function Handle_String_List
        (Opt : String_List_Options;
         Str : Unbounded_String)
         return Boolean;
      --  Invoke the multiple strings option callback if provided. Then, if Opt
      --  is greedy, consume all the arguments, store them in Result and return
      --  True. Consume only one Str and return False otherwise.

      ---------------
      -- Split_Arg --
      ---------------

      procedure Split_Arg
        (Arg           : String;
         Option, Value : out Slice;
         Has_Value     : out Boolean)
      is
      begin
         for I in Arg'Range loop
            if Arg (I) = '=' then
               Option := (Arg'First, I - 1);
               Value := (I + 1, Arg'Last);
               Has_Value := True;
               return;
            end if;
         end loop;
         Option := (Arg'First, Arg'Last);
         Value := (1, 0);
         Has_Value := False;
      end Split_Arg;

      -------------------
      -- Consume_Value --
      -------------------

      function Consume_Value
        (For_Arg : Slice;
         I       : in out Natural;
         J       : Natural)
        return String
      is
         Arg : String renames Args (I).all;
      begin
         --  The current short option requires a value:

         if J > Arg'Last then

            --  If this is the last option in this argument, fetch the next
            --  argument.

            return Consume_Next_Arg (For_Arg, I).all;

         else
            --  Otherwise, take the rest of the current argument (stripping the
            --  heading '=' if present).

            return Arg ((if Arg (J) = '=' then J + 1 else J) .. Arg'Last);
         end if;

      end Consume_Value;

      ----------------------
      -- Consume_Next_Arg --
      ----------------------

      function Consume_Next_Arg
        (For_Arg : Slice;
         I       : in out Natural)
         return GNAT.Strings.String_Access
      is
      begin
         if I < Args'Last then
            I := I + 1;
            return Args (I);
         else
            raise Arg_Error with
              (Args (I).all (For_Arg.First .. For_Arg.Last)
               & " requires a value");
         end if;
      end Consume_Next_Arg;

      -------------------
      -- Select_Option --
      -------------------

      function Select_Option
        (Opt_Name : String;
         Opt_Vec  : Option_Reference_Vector)
         return Option_Reference
      is
      begin
         for Ref of Opt_Vec.all loop
            if Get_Option (Parser, Ref).Commands (Result.Command) then
               return Ref;
            end if;
         end loop;
         raise Arg_Error with
           (Opt_Name & " is not valid with the """
            & Command_Name (Parser, Result.Command) & """ command.");
      end Select_Option;

      ---------------------
      -- Invoke_Callback --
      ---------------------

      function Invoke_Callback (Ref : Option_Reference) return Boolean is
      begin
         if Callback /= null then
            Callback (Result, Ref);
            return Length (Result.Error) > 0;
         end if;
         return False;
      end Invoke_Callback;

      -----------------
      -- Handle_Bool --
      -----------------

      procedure Handle_Bool (Opt : Bool_Options)
      is
      begin
         if Parser.Bool_Callback /= null then
            Parser.Bool_Callback (Result, Opt);
         end if;

         Result.Bool_Args (Opt) := True;
      end Handle_Bool;

      ------------------------
      -- Handle_String_List --
      ------------------------

      function Handle_String_List
        (Opt : String_List_Options;
         Str : Unbounded_String)
         return Boolean
      is
         Str_Vec : String_Vectors.Vector renames
           Result.String_List_Args (Opt);
      begin
         if Parser.String_List_Callback /= null then
            Parser.String_List_Callback (Result, Opt, +Str);
         end if;

         if Parser.String_List_Info (Opt).Greedy then
            for J in I .. Args'Last loop
               Str_Vec.Append (+Args (J).all);
            end loop;
            return True;
         else
            Str_Vec.Append (Str);
            return False;
         end if;
      end Handle_String_List;

   begin
      --  Get the command from With_Command or from Args

      if With_Command /= No_Command then
         Result.Command := With_Command;
      else
         if Args'Length = 0 then
            return Error ("No command specified.");
         end if;

         declare
            Command : constant GNAT.Strings.String_Access := Args (Args'First);
         begin
            for Cmd in Parser.Command_Infos'Range loop
               if +Parser.Command_Infos (Cmd).Name = Command.all then
                  Result.Command := Cmd;
                  exit;

               elsif Cmd = Command_Type'Last then
                  return Error
                    ("Bad command: " & Command.all & ". Try option --help");
               end if;
            end loop;
         end;
         I := I + 1;
      end if;

      --  And the parse the arguments that follow

      while I <= Args'Last loop
         declare
            Arg           : String renames Args (I).all;
            Option, Value : Slice;
            J             : Natural;
            Has_Value     : Boolean;

            Long_Cur      : Long_Options_Index.Cursor;
            Short_Cur     : Short_Options_Index.Cursor;

            Opt           : Option_Reference;
         begin
            --  The following allow users to query help messages for a specific
            --  subcommand.

            if Arg = "--help" then
               Print_Usage
                 (Parser        => Parser,
                  With_Internal => False,
                  Short         => False,
                  For_Command   => Result.Command);
               raise Outputs.Xcov_Exit_Exc;
            elsif Arg = "--help-internal" then
               Print_Usage
                 (Parser        => Parser,
                  With_Internal => True,
                  Short         => False,
                  For_Command   => Result.Command);
               raise Outputs.Xcov_Exit_Exc;
            end if;

            if Arg'Length >= 2 and then Arg (Arg'First) = '-' then
               --  We have an option! Determine if this is a short or a long
               --  option first. Since we allow long options to start with a
               --  single dash ('-'), let's first match a long option.

               Split_Arg (Arg, Option, Value, Has_Value);
               Long_Cur := Parser.Table.Long_Index.Find
                 (+Arg (Option.First .. Option.Last));

               if Long_Options_Index.Has_Element (Long_Cur) then

                  --  We found a match! Interpret it, now.

                  Opt := Select_Option
                    (Arg (Option.First .. Option.Last),
                     Long_Options_Index.Element (Long_Cur));
                  if Invoke_Callback (Opt) then
                     return Result;
                  end if;
                  case Opt.Kind is
                     when Bool_Opt =>
                        if Has_Value then
                           return Error
                             (Option_Name (Parser, Opt)
                              & " does not accept a value.");
                        end if;
                        Handle_Bool (Opt.Bool_Option);

                     when String_Opt | String_List_Opt =>
                        declare
                           Str : constant Unbounded_String :=
                             (if Has_Value
                              then +Arg (Value.First .. Value.Last)
                              else +Consume_Next_Arg (Option, I).all);
                        begin
                           if Opt.Kind = String_Opt then
                              if Parser.String_Callback /= null then
                                 Parser.String_Callback
                                   (Result, Opt.String_Option, +Str);
                              end if;

                              Result.String_Args (Opt.String_Option) :=
                                (Present => True,
                                 Value   => Str);

                           elsif Handle_String_List
                             (Opt.String_List_Option, Str)
                           then
                              --  If this is a greedy option, it consumes the
                              --  rest of the arguments, so we only have to
                              --  return afterwards.

                              return Result;
                           end if;
                        end;
                  end case;

               elsif Arg (Arg'First + 1) = '-' then
                  return Error
                    ("Unknown option: " & Arg (Option.First .. Option.Last));

               --  Past this point, this is either a short option or an unknown
               --  one.

               else
                  J := Arg'First + 1;
                  while J <= Arg'Last loop
                     Short_Cur :=
                       Parser.Table.Short_Index.Find (Arg (J));

                     if not Short_Options_Index.Has_Element (Short_Cur) then
                        return Error
                          ("Unknown option: -" & Arg (J) & " (from "
                           & Arg (Option.First .. Option.Last) & ").");
                     end if;

                     Opt := Select_Option
                       (Arg (Option.First .. Option.Last),
                        Short_Options_Index.Element (Short_Cur));
                     if Invoke_Callback (Opt) then
                        return Result;
                     end if;
                     case Opt.Kind is
                        when Bool_Opt =>
                           Handle_Bool (Opt.Bool_Option);
                           J := J + 1;

                        when String_Opt | String_List_Opt =>
                           --  This options requires a value:
                           --    * if this is the last option in this argument,
                           --      get the rest of this argument as a value (if
                           --      any) or fetch the next argument;
                           --    * otherwise take the rest of the current
                           --      argument.

                           declare
                              Str : constant Unbounded_String :=
                                +Consume_Value (Option, I, J + 1);
                           begin
                              if Opt.Kind = String_Opt then
                                 if Parser.String_Callback /= null then
                                    Parser.String_Callback
                                      (Result, Opt.String_Option, +Str);
                                 end if;
                                 Result.String_Args (Opt.String_Option) :=
                                   (Present => True,
                                    Value   => Str);

                              elsif Handle_String_List
                                (Opt.String_List_Option, Str)
                              then

                                 --  If this is a greedy option, it consumes
                                 --  the rest of the arguments, so we only
                                 --  have to return afterwards.

                                 return Result;
                              end if;
                           end;

                           --  We are done with this argument: move on to the
                           --  next one.

                           exit;
                     end case;
                  end loop;

               end if;

            else
               --  This is not an option: add it to remaining arguments

               if Parser.Arg_Callback /= null then
                  Parser.Arg_Callback (Result, Arg);
               end if;
               Result.Remaining_Args.Append (+Arg);
            end if;
         end;
         I := I + 1;
      end loop;

      return Result;
   exception
      when E : Arg_Error =>
         return Error (Ada.Exceptions.Exception_Message (E));
   end Parse;

   -----------
   -- Merge --
   -----------

   procedure Merge
     (Args       : in out Parsed_Arguments;
      Other_Args : Parsed_Arguments)
   is
   begin
      Args.Command := Other_Args.Command;

      --  Boolean arguments are active iff at least one of the arguments
      --  enabled it.

      for Opt in Bool_Options'Range loop
         Args.Bool_Args (Opt) :=
           Args.Bool_Args (Opt) or else Other_Args.Bool_Args (Opt);
      end loop;

      --  String arguments present in Other_Args override previous ones in Args

      for Opt in String_Options'Range loop
         if Other_Args.String_Args (Opt).Present then
            Args.String_Args (Opt) := Other_Args.String_Args (Opt);
         end if;
      end loop;

      --  Items present in Other_Args are just added to the previous ones in
      --  Arg. Likewise for remaining arguments.

      for Opt in String_List_Options'Range loop
         Args.String_List_Args (Opt).Append
           (Other_Args.String_List_Args (Opt));
      end loop;

      for Opt of Other_Args.Remaining_Args loop
         Args.Remaining_Args.Append (Opt);
      end loop;
   end Merge;

   -----------------------
   -- Split_Option_Text --
   -----------------------

   function Split_Option_Text (S : String) return String_Vectors.Vector is
      Result : String_Vectors.Vector;
      First  : Positive := S'First;
      I      : Positive := First;
   begin
      if S'Length = 0 then
         return Result;
      end if;

      while I <= S'Last loop
         if S (I) = '|' then
            Result.Append (+S (First .. I - 1));
            First := I + 1;
         end if;
         I := I + 1;
      end loop;
      Result.Append (+S (First .. I - 1));
      return Result;
   end Split_Option_Text;

   -----------------------
   -- Build_Parse_Table --
   -----------------------

   procedure Build_Parse_Table (Parser : Parser_Type) is

      procedure Process_Option (Ref : Option_Reference);
      --  Add Ref to Parser.Table. Raise a Program_Error if Ref has at least
      --  one invalid name.

      procedure Check_Homonym (Opt_Vec : Option_Reference_Vector);
      --  Raise a Program_Error if multiple options in Opt_Vec have similar
      --  names and have a common subset of accepted subcommands (i.e. if
      --  parsing them would be ambiguous).

      --------------------
      -- Process_Option --
      --------------------

      procedure Process_Option (Ref : Option_Reference) is
         Opt : constant Option_Info_Access := Get_Option (Parser, Ref);
      begin
         for Name of Split_Option_Text (+Opt.Long_Name) loop
            if Length (Name) < 2
              or else Element (Name, 1) /= '-'
            then
               raise Program_Error
                 with "Invalid option long name: " & (+Name);
            end if;

            --  TODO??? Also check that the option name matches [-a-zA-Z]+

            declare
               use Long_Options_Index;

               Index   : Long_Options_Index.Map
               renames Parser.Table.Long_Index;
               Opt_Vec : Option_Reference_Vector;

               Cur     : constant Cursor := Index.Find (Name);
            begin
               if Cur = No_Element then
                  Opt_Vec := new Option_Reference_Vectors.Vector;
                  Index.Insert (Name, Opt_Vec);
               else
                  Opt_Vec := Element (Cur);
               end if;
               Opt_Vec.Append (Ref);
            end;
         end loop;

         for Name of Split_Option_Text (+Opt.Short_Name) loop
            if Length (Name) /= 2
              or else Element (Name, 1) /= '-'
              or else Element (Name, 2) not in 'a' .. 'z' | 'A' .. 'Z'
            then
               raise Program_Error
                 with "Invalid option short name: " & (+Name);
            end if;

            declare
               use Short_Options_Index;

               Index   : Short_Options_Index.Map renames
                 Parser.Table.Short_Index;
               Key     : constant Character := Element (Name, 2);
               Opt_Vec : Option_Reference_Vector;

               Cur     : constant Cursor := Index.Find (Key);
            begin
               if Cur = No_Element then
                  Opt_Vec := new Option_Reference_Vectors.Vector;
                  Index.Insert (Key, Opt_Vec);
               else
                  Opt_Vec := Element (Cur);
               end if;
               Opt_Vec.Append (Ref);
            end;
         end loop;
      end Process_Option;

      -------------------
      -- Check_Homonym --
      -------------------

      procedure Check_Homonym (Opt_Vec : Option_Reference_Vector) is
      begin
         declare
            Busy_Commands : array (Valid_Commands) of Option_Info_Access :=
              (others => null);
            --  For each command, this array remembers what option reserved the
            --  current name.
         begin
            for Ref of Opt_Vec.all loop
               declare
                  Opt         : constant Option_Info_Access :=
                    Get_Option (Parser, Ref);
                  Opt_Cmd_Set : Command_Set renames
                    Opt.Commands;
               begin
                  for Cmd in Command_Set'Range loop
                     if Opt_Cmd_Set (Cmd) then
                        if Busy_Commands (Cmd) /= null then
                           raise Program_Error with
                             ("The following options conflict on the """
                              & (+Parser.Command_Infos (Cmd).Name)
                              & """ command: "
                              & Option_Name (Busy_Commands (Cmd).all)
                              & " and " & Option_Name (Opt.all));
                        end if;
                        Busy_Commands (Cmd) := Opt;
                     end if;
                  end loop;
               end;
            end loop;
         end;
      end Check_Homonym;

   begin
      --  First, add all options to the table

      for Opt in Bool_Options loop
         Process_Option ((Bool_Opt, Opt));
      end loop;
      for Opt in String_Options loop
         Process_Option ((String_Opt, Opt));
      end loop;
      for Opt in String_List_Options loop
         Process_Option ((String_List_Opt, Opt));
      end loop;

      --  Then, perform consistency checks:
      --
      --  1. For each name, check that there are not two different options that
      --  are valid for the same command.
      --
      --  2. No long name option can start like a short option (we want to
      --  support odd long names, like -eargs).

      for Opt_Vec of Parser.Table.Short_Index loop
         Check_Homonym (Opt_Vec);
      end loop;

      for Cur in Parser.Table.Long_Index.Iterate loop
         declare
            use Long_Options_Index;

            Name    : constant Unbounded_String := Key (Cur);
            C       : constant Character := Element (Name, 2);
            Opt_Vec : constant Option_Reference_Vector := Element (Cur);
         begin
            Check_Homonym (Opt_Vec);

            if Parser.Table.Short_Index.Contains (C) then
               raise Program_Error with
                 ("Long option name " & (+Name)
                  & " conflicts with short name -" & C);
            end if;
         end;
      end loop;

   end Build_Parse_Table;

   --------------------
   -- Sorted_Options --
   --------------------

   function Sorted_Options
     (Parser        : Parser_Type;
      With_Internal : Boolean;
      For_Command   : Command_Type := No_Command)
      return Option_Info_Array
   is
      function "<" (Left, Right : Option_Info_Access) return Boolean;

      procedure Sort is new Ada.Containers.Generic_Array_Sort
        (Index_Type   => Natural,
         Element_Type => Option_Info_Access,
         Array_Type   => Option_Info_Array);

      procedure Append (O : Option_Info_Access);
      --  Append O to Result and increment Last

      Count  : constant Natural :=
        (Parser.Bool_Info'Length
         + Parser.String_Info'Length
         + Parser.String_Info'Length);
      Result : Option_Info_Array (1 .. Count);
      Last   : Natural := 0;

      ---------
      -- "<" --
      ---------

      function "<" (Left, Right : Option_Info_Access) return Boolean is
         Left_Str : constant Unbounded_String :=
           (if Length (Left.Long_Name) = 0
            then Left.Short_Name
            else Left.Long_Name);
         Right_Str : constant Unbounded_String :=
           (if Length (Right.Long_Name) = 0
            then Right.Short_Name
            else Right.Long_Name);
      begin
         return Left_Str < Right_Str;
      end "<";

      ------------
      -- Append --
      ------------

      procedure Append (O : Option_Info_Access) is
      begin
         if For_Command /= No_Command then

            --  Filter options that are invalid for the given command

            if not O.Commands (For_Command) then
               return;
            end if;

            --  Also filter out internal ones if the command itself is not
            --  internal and if not asked for internal options.

            if not Parser.Command_Infos (For_Command).Internal
              and then
                O.Internal and then not With_Internal
            then
               return;
            end if;
         elsif O.Internal and then not With_Internal then
            return;
         end if;

         Last := Last + 1;
         Result (Last) := O;
      end Append;

   begin
      --  Fill Result with accesse to all options

      for Opt of Parser.Bool_Info loop
         Append (Opt'Unchecked_Access);
      end loop;
      for Opt of Parser.String_Info loop
         Append (Opt'Unchecked_Access);
      end loop;
      for Opt of Parser.String_List_Info loop
         Append (Opt'Unchecked_Access);
      end loop;

      --  Sort result by long option name and then short option name

      Sort (Result (1 .. Last));
      return Result (1 .. Last);
   end Sorted_Options;

   ----------
   -- Wrap --
   ----------

   function Wrap
     (Text              : String;
      First_Line_Indent : Natural;
      Next_Lines_Indent : Natural;
      Width             : Natural)
      return String
   is
      type Word_Type is record
         First, Last : Natural;
      end record;

      Result          : Unbounded_String;
      First_Line      : Boolean := True;
      I               : Natural := Text'First;
      Relative_Indent : Natural := 0;
      Column          : Natural := 0;

      procedure Scan_Relative_Indent;
      procedure Put_New_Line;
      procedure Put_Indent;
      function Next_Word return Word_Type;
      procedure Put_Word (Word : Word_Type);

      --------------------------
      -- Scan_Relative_Indent --
      --------------------------

      procedure Scan_Relative_Indent is
      begin
         Relative_Indent := 0;
         while I <= Text'Last and then Text (I) = ' ' loop
            I := I + 1;
            Relative_Indent := Relative_Indent + 1;
         end loop;
      end Scan_Relative_Indent;

      ------------------
      -- Put_New_Line --
      ------------------

      procedure Put_New_Line is
      begin
         Append (Result, ASCII.LF);
         First_Line := False;
         Column := 0;
      end Put_New_Line;

      ----------------
      -- Put_Indent --
      ----------------

      procedure Put_Indent is
         Size : constant Natural :=
           (if First_Line
            then First_Line_Indent
            else Next_Lines_Indent) + Relative_Indent;
      begin
         Append (Result, (1 .. Size => ' '));
         Column := Column + Size;
      end Put_Indent;

      ---------------
      -- Next_Word --
      ---------------

      function Next_Word return Word_Type is
         Start : Natural;
      begin
         --  First, look for the start of the next word. If we find a new line
         --  character, return it as if it was a word on its own.

         while I <= Text'Last loop
            case Text (I) is
               when ASCII.LF =>
                  Start := I;
                  I := I + 1;
                  return (Start, Start);
               when ' ' =>
                  null;
               when others =>
                  exit;
            end case;
            I := I + 1;
         end loop;

         if I > Text'Last then
            return (1, 0);
         end if;

         Start := I;

         --  Then, look for the end of this word

         while I <= Text'Last and then Text (I) not in ASCII.LF | ' ' loop
            I := I + 1;
         end loop;

         return (Start, I - 1);
      end Next_Word;

      --------------
      -- Put_Word --
      --------------

      procedure Put_Word (Word : Word_Type) is
         S : String renames Text (Word.First .. Word.Last);
      begin
         Append (Result, S);
         Column := Column + S'Length;
      end Put_Word;

   begin
      Scan_Relative_Indent;
      loop
         declare
            Word   : constant Word_Type := Next_Word;
            Length : constant Natural :=
              (if Word.First > Word.Last
               then 0
               else Word.Last - Word.First + 1);
         begin
            exit when Length = 0;
            if Text (Word.First) = ASCII.LF then
               Put_New_Line;
               Scan_Relative_Indent;

            else
               --  At this point, we know we have to output a word

               if Column = 0 then
                  Put_Indent;
               elsif Column + Length > Width then
                  Put_New_Line;
                  Put_Indent;
               else
                  Append (Result, ' ');
                  Column := Column + 1;
               end if;

               Put_Word (Word);
            end if;
         end;
      end loop;
      if Column /= 0 then
         Append (Result, ASCII.LF);
      end if;
      return To_String (Result);
   end Wrap;

end Argparse;
