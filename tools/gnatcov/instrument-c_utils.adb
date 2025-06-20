------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

with Interfaces.C; use Interfaces.C;
with System;       use System;

with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Clang.CX_String;  use Clang.CX_String;
with Clang.Extensions; use Clang.Extensions;

with Files_Table; use Files_Table;

package body Instrument.C_Utils is

   use Cursor_Vectors;

   function Visitor_Wrapper
     (Node        : Cursor_T;
      Parent      : Cursor_T;
      Client_Data : Client_Data_T) return Child_Visit_Result_T
     with Convention => C;
   --  Helper for Visit_Children and Visit procedures.
   --
   --  Interpret Client_Data as the Visitor anonymous function for these
   --  procedures and call it on Node.
   --
   --  Note: this has convention C in order to be a callback for the
   --  Clang.Index.Visit_Children and Clang.Index.Visit functions.

   --------------------------------
   -- Presumed_Spelling_Location --
   --------------------------------

   function Presumed_Spelling_Location
     (TU             : Translation_Unit_T;
      Loc            : Source_Location_T;
      Macro_Name     : Unbounded_String;
      Builtin_Macros : Macro_Set) return Source_Location
   is
      C_Filename   : aliased String_T;
      Line, Column : aliased unsigned;
      Filename     : Unbounded_String;
      Sloc         : Local_Source_Location;
   begin
      Get_Presumed_Location
        (Get_Spelling_Loc (TU, Loc),
         C_Filename'Access,
         Line'Access,
         Column'Access);
      Filename := +Get_C_String (C_Filename);

      --  If this is a command-line or built-in source location, ignore the
      --  line and column indices.

      if Clang_Predefined_File (+Filename) then

         --  As built-in macros are passed through clang's command line, their
         --  location refers to the "<command line>" rather than "<built-in>".
         --  Fix this for macros that are actually built-in.

         if Builtin_Macros.Contains
           (Macro_Definition'
              (Define => True, Name => Macro_Name, others => <>))
         then
            Filename := +"<built-in>";
         end if;
         Sloc := To_Sloc (0, 0);
      else
         Sloc := To_Sloc (Line, Column);
         Filename := +Full_Name (+Filename);
      end if;

      Dispose_String (C_Filename);
      return
        (Source_File => Get_Index_From_Generic_Name
           (Name => +Filename,
            Kind => Source_File),
         L           =>  Sloc);
   end Presumed_Spelling_Location;

   ----------
   -- Sloc --
   ----------

   function Sloc (Loc : Source_Location_T) return Source_Location is
      Line, Column : aliased Interfaces.C.unsigned;
      File         : aliased String_T;
   begin
      if Loc = Get_Null_Location then
         return Slocs.No_Location;
      end if;
      Get_Presumed_Location (Location => Loc,
                             Filename => File'Access,
                             Line     => Line'Access,
                             Column   => Column'Access);
      return
        (Source_File =>
           Get_Index_From_Generic_Name
             (Full_Name (Get_C_String (File)), Kind => Source_File),
         L           =>
           (Line   => Natural (Line),
            Column => Natural (Column)));
   end Sloc;

   ----------------
   -- Start_Sloc --
   ----------------

   function Start_Sloc (N : Cursor_T) return Source_Location_T is
   begin
      return Get_Range_Start (Get_Cursor_Extent (N));
   end Start_Sloc;

   function Start_Sloc (N : Cursor_T) return Source_Location is
   begin
      return Sloc (Start_Sloc (N));
   end Start_Sloc;

   --------------
   -- End_Sloc --
   --------------

   function End_Sloc (N : Cursor_T) return Source_Location_T is
   begin
      return Get_Range_End (Get_Cursor_Extent (N));
   end End_Sloc;

   function End_Sloc (N : Cursor_T) return Source_Location is
      Loc : Source_Location_T := End_Sloc (N);
   begin
      if Is_Macro_Location (Loc) then
         Loc := Get_Expansion_End (Get_Cursor_TU (N), Loc);
      end if;
      return Sloc (Loc);
   end End_Sloc;

   ----------
   -- Kind --
   ----------

   function Kind (N : Cursor_T) return Cursor_Kind_T is
   begin
      return Get_Cursor_Kind (N);
   end Kind;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (N : Cursor_T) return Boolean is
   begin
      return Cursor_Is_Null (N);
   end Is_Null;

   ---------------------
   -- Visitor_Wrapper --
   ---------------------

   function Visitor_Wrapper
     (Node        : Cursor_T;
      Parent      : Cursor_T;
      Client_Data : Client_Data_T) return Child_Visit_Result_T
   is
      pragma Unreferenced (Parent);
      Callback : access function
                   (Node : Cursor_T) return Child_Visit_Result_T
      with Import, Address => System.Address (Client_Data);
   begin
      return Callback.all (Node);
   end Visitor_Wrapper;

   --------------------
   -- Visit_Children --
   --------------------

   procedure Visit_Children
     (Parent  : Cursor_T;
      Visitor : not null access function
                  (Node : Cursor_T) return Child_Visit_Result_T)
   is
      Dummy : constant unsigned := Visit_Children
        (Parent, Visitor_Wrapper'Access, Client_Data_T (Visitor'Address));
   begin
      null;
   end Visit_Children;

   -----------
   -- Visit --
   -----------

   procedure Visit
     (Parent  : Cursor_T;
      Visitor : not null access function
                  (Node : Cursor_T) return Child_Visit_Result_T)
   is
      Dummy : constant unsigned := Visit
        (Parent, Visitor_Wrapper'Access, Client_Data_T (Visitor'Address));
   begin
      null;
   end Visit;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children (N : Cursor_T) return Cursor_Vectors.Vector is
      Res : Vector;

      function Append_Child (Cursor : Cursor_T) return Child_Visit_Result_T;
      --  Callback for Visit_Children. Append Cursor to Res and continue the
      --  traversal.

      ------------------
      -- Append_Child --
      ------------------

      function Append_Child (Cursor : Cursor_T) return Child_Visit_Result_T is
      begin
         Res.Append (Cursor);
         return Child_Visit_Continue;
      end Append_Child;

   --  Start of processing for Get_Children

   begin
      Visit_Children (N, Get_Children.Append_Child'Access);
      return Res;
   end Get_Children;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (N : Cursor_T) return Cursor_Vectors.Vector is
      Res : Cursor_Vectors.Vector;
   begin
      Res.Append (N);
      return Res;
   end To_Vector;

   --------------
   -- Get_Main --
   --------------

   function Get_Main (TU : Translation_Unit_T) return Cursor_T is
      Result : Cursor_T := Get_Null_Cursor;

      function Visit_Decl (Cursor : Cursor_T) return Child_Visit_Result_T;
      --  Callback for Visit_Children. Set Result and break the iteration if
      --  Cursor is the "main" function definition, continue the iteration to
      --  find the main otherwise.

      ----------------
      -- Visit_Decl --
      ----------------

      function Visit_Decl (Cursor : Cursor_T) return Child_Visit_Result_T is
      begin
         if Kind (Cursor) in Cursor_Translation_Unit | Cursor_Linkage_Spec
         then
            return Child_Visit_Recurse;
         end if;

         --  Clang will append an underscore to the mangled names of entities
         --  when -m32 is passed, so we also need to check for this case.

         if Cursor_Get_Mangling (Cursor) = "main"
           or else Cursor_Get_Mangling (Cursor) = "_main"
         then
            Result := Cursor;
            return Child_Visit_Break;
         end if;
         return Child_Visit_Continue;
      end Visit_Decl;

   --  Start of processing for Get_Main

   begin
      Visit_Children (Parent  => Get_Translation_Unit_Cursor (TU),
                      Visitor => Visit_Decl'Access);
      return Result;
   end Get_Main;

   ------------------------
   -- Is_Atexit_Declared --
   ------------------------

   function Is_Atexit_Declared (TU : Translation_Unit_T) return Boolean is
      Has_Atexit_Declaration : Boolean := False;

      function Is_Atexit (Cursor : Cursor_T) return Child_Visit_Result_T;
      --  Set Has_Atexit_Declaration to True if Cursor is the declaration of
      --  the atexit function. Otherwise, recurse into the node if it is a
      --  list of C declarations, or continue the traversal of the current
      --  declaration list.

      ---------------
      -- Is_Atexit --
      ---------------

      function Is_Atexit (Cursor : Cursor_T) return Child_Visit_Result_T is
      begin
         if Kind (Cursor) in Cursor_Translation_Unit | Cursor_Linkage_Spec
         then
            return Child_Visit_Recurse;
         elsif Cursor_Get_Mangling (Cursor) = "atexit" then
            Has_Atexit_Declaration := True;
            return Child_Visit_Break;
         else
            return Child_Visit_Continue;
         end if;
      end Is_Atexit;

      --  Start of processing for Is_Atexit_Declared

   begin
      Visit_Children (Parent  => Get_Translation_Unit_Cursor (TU),
                      Visitor => Is_Atexit'Access);
      return Has_Atexit_Declaration;
   end Is_Atexit_Declared;

   --  Rewriting utilities

   --------------------------------
   -- Insert_Text_After_Start_Of --
   --------------------------------

   procedure Insert_Text_After_Start_Of
     (N    : Cursor_T;
      Text : String;
      Rew  : Rewriter_T)
   is
      Location : constant Source_Location_T :=
        Get_Range_Start (Get_Cursor_Extent (N));
   begin
      CX_Rewriter_Insert_Text_After
        (Rew    => Rew,
         Loc    => Location,
         Insert => Text);
   end Insert_Text_After_Start_Of;

   -----------------------------
   -- Insert_Text_In_Brackets --
   -----------------------------

   procedure Insert_Text_In_Brackets
     (CmpdStmt : Cursor_T;
      Text     : String;
      Rew      : Rewriter_T)
   is
      Location : Source_Location_T;
   begin

      --  If the Cursor is not of the right kind, the call to
      --  Get_LBrac_Loc_Plus_One will return a null cursor.

      pragma Assert (Kind (CmpdStmt) = Cursor_Compound_Stmt);

      Location := Get_LBrac_Loc_Plus_One (CmpdStmt);
      CX_Rewriter_Insert_Text_After
        (Rew    => Rew,
         Loc    => Location,
         Insert => Text);
   end Insert_Text_In_Brackets;

   ---------------------------------
   -- Insert_Text_Before_Start_Of --
   ---------------------------------

   procedure Insert_Text_Before_Start_Of
     (N    : Cursor_T;
      Text : String;
      Rew  : Rewriter_T)
   is
      Location : constant Source_Location_T :=
        Get_Range_Start (Get_Cursor_Extent (N));
   begin
      CX_Rewriter_Insert_Text_Before
        (Rew    => Rew,
         Loc    => Location,
         Insert => Text);
   end Insert_Text_Before_Start_Of;

   -------------------------------
   -- Insert_Text_After_End_Of --
   -------------------------------

   procedure Insert_Text_After_End_Of
     (N    : Cursor_T;
      Text : String;
      Rew  : Rewriter_T)
   is
      Location : constant Source_Location_T :=
        Get_Range_End (Get_Cursor_Extent (N));
   begin
      CX_Rewriter_Insert_Text_After
        (Rew    => Rew,
         Loc    => Location,
         Insert => Text);
   end Insert_Text_After_End_Of;

   -------------------------------
   -- Insert_Text_Before_End_Of --
   -------------------------------

   procedure Insert_Text_Before_End_Of
     (N    : Cursor_T;
      Text : String;
      Rew  : Rewriter_T)
   is
      Location : constant Source_Location_T :=
        Get_Range_End (Get_Cursor_Extent (N));
   begin
      CX_Rewriter_Insert_Text_Before
        (Rew    => Rew,
         Loc    => Location,
         Insert => Text);
   end Insert_Text_Before_End_Of;

   --------------------
   -- Iterate_Tokens --
   --------------------

   procedure Iterate_Tokens
     (TU      : Translation_Unit_T;
      N       : Cursor_T;
      Process : not null access procedure (Tok : Token_T))
   is
      type Tokens_Array is array (Natural range <>) of aliased Token_T;
      Source_Range : constant Source_Range_T := Get_Cursor_Extent (N);
      Num_Tokens   : aliased Interfaces.C.unsigned;
      Toks_Ptr     : System.Address;
      type Token_Acc is access Token_T;
      function To_Access is new Ada.Unchecked_Conversion
        (System.Address, Token_Acc);
   begin
      Tokenize (TU, Source_Range, Toks_Ptr'Address, Num_Tokens'Access);
      if Toks_Ptr = Null_Address then
         return;
      end if;
      declare
         Tokens  : Tokens_Array (1 .. Natural (Num_Tokens))
           with Import, Address => Toks_Ptr;
      begin
         for J in Tokens'Range loop
            Process.all (Tokens (J));
         end loop;
         Dispose_Tokens (TU, To_Access (Toks_Ptr), Num_Tokens);
      end;
   end Iterate_Tokens;

   --  Debugging utilities

   ------------------
   -- Print_Tokens --
   ------------------

   procedure Print_Tokens (TU : Translation_Unit_T; N : Cursor_T) is
      procedure Put_Token (Tok : Token_T);
      --  Callback for Iterate_Tokens. Put the spelling of Tok on the standard
      --  output.

      ---------------
      -- Put_Token --
      ---------------

      procedure Put_Token (Tok : Token_T) is
      begin
         Put (Get_Token_Spelling (TU, Tok));
      end Put_Token;

   --  Start of processing for Print_Tokens

   begin
      Iterate_Tokens (TU, N, Put_Token'Access);
      New_Line;
   end Print_Tokens;

end Instrument.C_Utils;
