------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2008-2022, AdaCore                     --
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Clang.CX_String;  use Clang.CX_String;
with Clang.Extensions; use Clang.Extensions;

package body Instrument.C_Utils is

   use Cursor_Vectors;

   ----------
   -- Sloc --
   ----------

   function Sloc (Loc : Source_Location_T) return Source_Location is
      Line : aliased Interfaces.C.unsigned;
      Column : aliased Interfaces.C.unsigned;
   begin
      Get_Presumed_Location (Location => Loc,
                             Filename => null,
                             Line     => Line'Access,
                             Column   => Column'Access);
      return (Line => Langkit_Support.Slocs.Line_Number (Line),
              Column => Langkit_Support.Slocs.Column_Number (Column));
   end Sloc;

   ----------------
   -- Start_Sloc --
   ----------------

   function Start_Sloc (N : Cursor_T) return Source_Location is
      Line   : aliased Interfaces.C.unsigned;
      Column : aliased Interfaces.C.unsigned;
      Loc    : constant Source_Location_T :=
        Get_Range_Start (Get_Cursor_Extent (N));
   begin
      Get_Presumed_Location (Location => Loc,
                             Filename => null,
                             Line     => Line'Access,
                             Column   => Column'Access);
      return (Line => Langkit_Support.Slocs.Line_Number (Line),
              Column => Langkit_Support.Slocs.Column_Number (Column));
   end Start_Sloc;

   --------------
   -- End_Sloc --
   --------------

   function End_Sloc (N : Cursor_T) return Source_Location is
      Line   : aliased Interfaces.C.unsigned;
      Column : aliased Interfaces.C.unsigned;
      Loc    : Source_Location_T :=
        Get_Range_End (Get_Cursor_Extent (N));
   begin
      if Is_Macro_Location (Loc) then
         Loc := Get_Expansion_End (Get_Cursor_TU (N), Loc);
      end if;
      Get_Presumed_Location
        (Location => Loc,
         Filename => null,
         Line     => Line'Access,
         Column   => Column'Access);
      return (Line   => Langkit_Support.Slocs.Line_Number (Line),
              Column => Langkit_Support.Slocs.Column_Number (Column));
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

   pragma Warnings (Off);

   --------------------
   -- Visit_Children --
   --------------------

   procedure Visit_Children
     (Parent  : Cursor_T;
      Visitor : Cursor_Visitor_Function) is

      function Visitor_Wrapper
        (Node        : Cursor_T;
         Parent      : Cursor_T;
         Client_Data : Client_Data_T) return Child_Visit_Result_T
        with Convention => C;

      function Visitor_Wrapper
        (Node        : Cursor_T;
         Parent      : Cursor_T;
         Client_Data : Client_Data_T) return Child_Visit_Result_T
      is
         pragma Unreferenced (Parent, Client_Data);
      begin
         return Visitor (Node);
      end Visitor_Wrapper;

      Data   : Client_Data_T;
      Result : unsigned;
   begin
      Result :=
        Visit_Children (Parent, Visitor_Wrapper'Unrestricted_Access, Data);
   end Visit_Children;

   -----------
   -- Visit --
   -----------

   procedure Visit (Parent : Cursor_T; Visitor : Cursor_Visitor_Function) is

      function Visitor_Wrapper
        (Node        : Cursor_T;
         Parent      : Cursor_T;
         Client_Data : Client_Data_T) return Child_Visit_Result_T
      with Convention => C;

      function Visitor_Wrapper
        (Node        : Cursor_T;
         Parent      : Cursor_T;
         Client_Data : Client_Data_T) return Child_Visit_Result_T
      is
         pragma Unreferenced (Parent, Client_Data);
      begin
         return Visitor (Node);
      end Visitor_Wrapper;

      Data   : Client_Data_T;
      Result : unsigned;
   begin
      Result := Visit (Parent, Visitor_Wrapper'Unrestricted_Access, Data);
   end Visit;
   pragma Warnings (On);

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children (N : Cursor_T) return Cursor_Vectors.Vector is

      Res : Vector;

      --  Append the children to the result vector and continue the traversal
      --  until all children have been appended.

      function Append_Child (Cursor : Cursor_T) return Child_Visit_Result_T
        with Convention => C;

      function Append_Child (Cursor : Cursor_T) return Child_Visit_Result_T
      is
      begin
         Res.Append (Cursor);
         return Child_Visit_Continue;
      end Append_Child;

   begin
      Visit_Children (N, Get_Children.Append_Child'Unrestricted_Access);
      return Res;
   end Get_Children;

   -------------------------
   -- Is_Unit_Of_Interest --
   -------------------------

   function Is_Unit_Of_Interest
     (N        : Cursor_T;
      Filename : String) return Boolean
   is
      File   : aliased String_T;
      Line   : aliased unsigned;
      Column : aliased unsigned;
      Loc    : constant Source_Location_T := Get_Cursor_Location (N);
   begin
      Get_Presumed_Location (Location => Loc,
                             Filename => File'Access,
                             Line     => Line'Access,
                             Column   => Column'Access);
      declare
         Cursor_Filename : constant String := Get_C_String (File);
      begin
         --  For now, just retrieve SCO for the file being instrumented.
         --  TODO: determine what should be done for included code (inlines
         --  function, expanded macro coming from another file etc.)

         return Cursor_Filename = Filename;
      end;
   end Is_Unit_Of_Interest;

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

      Main_Decl_Cursor : Cursor_T := Get_Null_Cursor;

      function Visit_Decl (Cursor : Cursor_T) return Child_Visit_Result_T
        with Convention => C;

      function Visit_Decl (Cursor : Cursor_T) return Child_Visit_Result_T
      is
      begin
         if Kind (Cursor) = Cursor_Translation_Unit then
            return Child_Visit_Recurse;
         end if;
         if Cursor_Get_Mangling (Cursor) = "main" then
            Main_Decl_Cursor := Cursor;
            return Child_Visit_Break;
         end if;
         return Child_Visit_Continue;
      end Visit_Decl;
   begin
      Visit_Children (Parent  => Get_Translation_Unit_Cursor (TU),
                      Visitor => Visit_Decl'Unrestricted_Access);
      return Main_Decl_Cursor;
   end Get_Main;

   --  Rewriting utilities

   ---------------------------
   -- Add_Statement_In_Main --
   ---------------------------

   procedure Add_Statement_In_Main
     (TU        : Translation_Unit_T;
      Rew       : Rewriter_T;
      Statement : String) is

      function Visit_Decl (Cursor : Cursor_T) return Child_Visit_Result_T
        with Convention => C;
      --  Traverse the tree until the main function is found, and insert a
      --  statement.

      function Visit_Decl (Cursor : Cursor_T) return Child_Visit_Result_T
      is
      begin
         if Kind (Cursor) = Cursor_Translation_Unit then
            return Child_Visit_Recurse;
         end if;
         if Cursor_Get_Mangling (Cursor) = "main" then
            declare
               Fun_Children : constant Vector := Get_Children (Cursor);
               Body_Cursor  : constant Cursor_T :=
                 Fun_Children (Last_Index (Fun_Children));

               --  Body of a function is a compound statement, so rewrite
               --  before its first children node will do.

               Body_Stmts : constant Vector := Get_Children (Body_Cursor);
               First_Stmt : constant Cursor_T :=
                 Body_Stmts (First_Index (Body_Stmts));

               Location : constant Source_Location_T :=
                 Get_Cursor_Location (First_Stmt);
            begin
               CX_Rewriter_Insert_Text_Before
                 (Rew    => Rew,
                  Loc    => Location,
                  Insert => Statement);
            end;
            return Child_Visit_Break;
         end if;
         return Child_Visit_Continue;
      end Visit_Decl;
   begin
      Visit_Children (Parent  => Get_Translation_Unit_Cursor (TU),
                      Visitor => Visit_Decl'Unrestricted_Access);
   end Add_Statement_In_Main;

   ---------------------------------
   -- Add_Statement_Before_Return --
   ---------------------------------

   procedure Add_Statement_Before_Return
     (Fun_Decl  : Cursor_T;
      Rew       : Rewriter_T;
      Statement : String) is

      function Visit_Decl (Cursor : Cursor_T)
         return Child_Visit_Result_T with Convention => C;

      function Visit_Decl (Cursor : Cursor_T) return Child_Visit_Result_T
      is
      begin
         if Is_Statement (Kind (Cursor)) then
            if Kind (Cursor) = Cursor_Return_Stmt then
               Insert_Text_After_Start_Of (N    => Cursor,
                                           Text => Statement,
                                           Rew  => Rew);
            else
               return Child_Visit_Recurse;
            end if;
         end if;
         return Child_Visit_Continue;
      end Visit_Decl;

   begin
      Visit_Children (Parent  => Fun_Decl,
                      Visitor => Visit_Decl'Unrestricted_Access);
   end Add_Statement_Before_Return;

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

   -------------
   -- Curlify --
   -------------

   procedure Curlify (N : Cursor_T; Rew : Rewriter_T) is
   begin
      case Kind (N) is
         when Cursor_Compound_Stmt =>
            null;
         when others =>
            declare
               Location_After : constant Source_Location_T :=
                 Get_Range_End (Get_Cursor_Extent (N));
            begin
               Insert_Text_After_Start_Of (N, "{", Rew);
               CX_Rewriter_Insert_Text_After_Token (Rew, Location_After, "}");
            end;
      end case;
   end Curlify;

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

      ---------------
      -- Put_Token --
      ---------------

      procedure Put_Token (Tok : Token_T) is
      begin
         Put (Get_Token_Spelling (TU, Tok));
      end Put_Token;
   begin
      Iterate_Tokens (TU, N, Put_Token'Access);
      New_Line;
   end Print_Tokens;

end Instrument.C_Utils;
