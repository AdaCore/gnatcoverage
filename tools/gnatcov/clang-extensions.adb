------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2021-2024, AdaCore                     --
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

--  Wrap the clang extension functions into thicker bindings, similarly to what
--  is done for the libclang Ada bindings themselves (to avoid having to use
--  the String_T type that must be memory managed by the user for instance).

with Clang.CX_File;   use Clang.CX_File;
with Clang.CX_String; use Clang.CX_String;

with Files_Table; use Files_Table;

package body Clang.Extensions is

   function To_Sloc
     (File : File_T; Line, Column : unsigned) return Source_Location
   is
     ((Source_File => Get_Index_From_Generic_Name
                        (Name => Get_File_Name (File),
                         Kind => Source_File),
       L           =>  To_Sloc (Line, Column)));
   --  Convert a Clang source location to gnatcov's own format

   -----------------------
   -- Get_Decl_Name_Str --
   -----------------------

   function Get_Decl_Name_Str (C : Cursor_T) return String is

      function Get_Decl_Name_Str_C (C : Cursor_T) return String_T
        with
          Import, Convention => C,
          External_Name => "clang_getDeclName";

      DeclName_Str_C : constant String_T := Get_Decl_Name_Str_C (C);
      DeclName       : constant String   := Get_C_String (DeclName_Str_C);
   begin
      Dispose_String (DeclName_Str_C);
      return DeclName;
   end Get_Decl_Name_Str;

   -------------------------
   -- Get_Callee_Name_Str --
   -------------------------

   function Get_Callee_Name_Str (C : Cursor_T) return String is

      function Get_Callee_Name_Str_C (C : Cursor_T) return String_T
        with
          Import, Convention => C,
          External_Name => "clang_getCalleeName";

      CalleeName_Str_C : constant String_T := Get_Callee_Name_Str_C (C);
      CalleeName       : constant String   := Get_C_String (CalleeName_Str_C);
   begin
      Dispose_String (CalleeName_Str_C);
      return CalleeName;
   end Get_Callee_Name_Str;

   ---------------------------------
   -- Is_Instrumentable_Call_Expr --
   ---------------------------------

   function Is_Instrumentable_Call_Expr (C : Cursor_T) return Boolean is
      function Is_Instrumentable_Call_Expr_C (C : Cursor_T) return unsigned
        with
          Import, Convention => C,
          External_Name => "clang_isInstrumentableCallExpr";
   begin
      return Is_Instrumentable_Call_Expr_C (C) /= 0;
   end Is_Instrumentable_Call_Expr;

   -------------------------------
   --  Is_CXX_Member_Call_Expr  --
   -------------------------------

   function Is_Prefixed_CXX_Member_Call_Expr (C : Cursor_T) return Boolean is
      function Is_CXX_Member_Call_Expr_C (C : Cursor_T) return unsigned
        with
          Import, Convention => C,
          External_Name => "clang_isPrefixedCXXMemberCallExpr";
   begin
      return Is_CXX_Member_Call_Expr_C (C) /= 0;
   end Is_Prefixed_CXX_Member_Call_Expr;

   --------------------
   -- Get_Opcode_Str --
   --------------------

   function Get_Opcode_Str (C : Cursor_T) return String is

      function Get_Opcode_Str_C (C : Cursor_T) return String_T
        with
          Import, Convention => C,
          External_Name => "clang_getOpcodeStr";

      Opcode_Str_C : constant String_T := Get_Opcode_Str_C (C);
      Opcode_Str   : constant String := Get_C_String (Opcode_Str_C);
   begin
      Dispose_String (Opcode_Str_C);
      return Opcode_Str;
   end Get_Opcode_Str;

   --------------------------------------
   -- Is_This_Declaration_A_Definition --
   --------------------------------------

   function Is_This_Declaration_A_Definition (C : Cursor_T) return Boolean
   is
      function Is_This_Declaration_A_Definition_C
        (C : Cursor_T) return unsigned
        with
          Import, Convention => C,
          External_Name => "clang_isThisDeclarationADefinition";
   begin
      return Is_This_Declaration_A_Definition_C (C) /= 0;
   end Is_This_Declaration_A_Definition;

   ------------------
   -- Is_Constexpr --
   ------------------

   function Is_Constexpr (C : Cursor_T) return Boolean
   is
      function Is_Constexpr_C (C : Cursor_T) return unsigned
        with
          Import, Convention => C,
          External_Name => "clang_isConstexpr";
   begin
      return Is_Constexpr_C (C) /= 0;
   end Is_Constexpr;

   -----------------------------------
   -- CX_Rewriter_Insert_Text_After --
   -----------------------------------

   procedure CX_Rewriter_Insert_Text_After
     (Rew    : Rewriter_T;
      Loc    : Source_Location_T;
      Insert : String)
   is
      procedure CX_Rewriter_Insert_Text_After_C
        (Rew    : Rewriter_T;
         Loc    : Source_Location_T;
         Insert : String)
        with
          Import, Convention => C,
          External_Name => "clang_CXRewriter_insertTextAfter";
   begin
      CX_Rewriter_Insert_Text_After_C (Rew, Loc, Insert & ASCII.NUL);
   end CX_Rewriter_Insert_Text_After;

   -----------------------------------------
   -- CX_Rewriter_Insert_Text_After_Token --
   -----------------------------------------

   procedure CX_Rewriter_Insert_Text_After_Token
     (Rew    : Rewriter_T;
      Loc    : Source_Location_T;
      Insert : String)
   is
      procedure CX_Rewriter_Insert_Text_After_Token_C
        (Rew    : Rewriter_T;
         Loc    : Source_Location_T;
         Insert : String)
        with
          Import, Convention => C,
          External_Name => "clang_CXRewriter_insertTextAfterToken";
   begin
      CX_Rewriter_Insert_Text_After_Token_C (Rew, Loc, Insert & ASCII.NUL);
   end CX_Rewriter_Insert_Text_After_Token;

   ------------------------------------------
   -- CX_Rewriter_Insert_Text_Before_Token --
   ------------------------------------------

   procedure CX_Rewriter_Insert_Text_Before_Token
     (Rew    : Rewriter_T;
      Loc    : Source_Location_T;
      Insert : String)
   is
      procedure CX_Rewriter_Insert_Text_Before_Token_C
        (Rew    : Rewriter_T;
         Loc    : Source_Location_T;
         Insert : String)
        with
          Import, Convention => C,
          External_Name => "clang_CXRewriter_insertTextBeforeToken";
   begin
      CX_Rewriter_Insert_Text_Before_Token_C (Rew, Loc, Insert & ASCII.NUL);
   end CX_Rewriter_Insert_Text_Before_Token;

   ------------------------------------
   -- CX_Rewriter_Get_Rewritten_Text --
   ------------------------------------

   function CX_Rewriter_Get_Rewritten_Text
     (Rew : Rewriter_T;
      R   : Source_Range_T) return String
   is
      function CX_Rewriter_Get_Rewritten_Text
        (Rew : Rewriter_T;
         R   : Source_Range_T) return String_T
        with
          Import, Convention => C,
          External_Name => "clang_CXRewriter_getRewrittenText";

      Rewritten_Text_C : constant String_T :=
        CX_Rewriter_Get_Rewritten_Text (Rew, R);
      Rewritten_Text   : constant String := Get_C_String (Rewritten_Text_C);
   begin
      Dispose_String (Rewritten_Text_C);
      return Rewritten_Text;
   end CX_Rewriter_Get_Rewritten_Text;

   -----------------------
   -- Spelling_Location --
   -----------------------

   function Spelling_Location (Loc : Source_Location_T) return Source_Location
   is
      File                 : File_T;
      Line, Column, Offset : aliased unsigned;
   begin
      Get_Spelling_Location
        (Loc, File'Address, Line'Access, Column'Access, Offset'Access);

      return To_Sloc (File, Line, Column);
   end Spelling_Location;

   -------------------
   -- File_Location --
   -------------------

   function File_Location
     (Loc : Source_Location_T) return Local_Source_Location
   is
      File                 : File_T;
      Line, Column, Offset : aliased unsigned;
   begin
      Get_File_Location
        (Loc, File'Address, Line'Access, Column'Access, Offset'Access);
      return To_Sloc (Line, Column);
   end File_Location;

   -----------------------
   -- Presumed_Location --
   -----------------------

   function Presumed_Location
     (Loc : Source_Location_T) return Local_Source_Location
   is
      Filename     : aliased String_T;
      Line, Column : aliased unsigned;
   begin
      Get_Presumed_Location (Loc, Filename'Access, Line'Access, Column'Access);
      Dispose_String (Filename);
      return To_Sloc (Line, Column);
   end Presumed_Location;

   -----------------------
   -- Is_Macro_Location --
   -----------------------

   function Is_Macro_Location (Loc : Source_Location_T) return Boolean
   is
      function Is_Macro_Location_C (Loc : Source_Location_T) return unsigned
        with
          Import, Convention => C,
          External_Name => "clang_isMacroLocation";
   begin
      return Is_Macro_Location_C (Loc) /= 0;
   end Is_Macro_Location;

   ----------------------------
   -- Is_Macro_Arg_Expansion --
   ----------------------------

   function Is_Macro_Arg_Expansion
     (Loc       : Source_Location_T;
      Start_Loc : out Source_Location_T;
      TU        : Translation_Unit_T) return Boolean
   is
      function Is_Macro_Arg_Expansion
        (Loc       : Source_Location_T;
         Start_Loc : access Source_Location_T;
         TU        : Translation_Unit_T) return unsigned
        with
          Import, Convention => C,
          External_Name      => "clang_isMacroArgExpansion";

      C_Start_Loc : aliased Source_Location_T;
   begin
      return Result : constant Boolean :=
        Is_Macro_Arg_Expansion (Loc, C_Start_Loc'Access, TU) /= 0
      do
         Start_Loc := C_Start_Loc;
      end return;
   end Is_Macro_Arg_Expansion;

   ----------------------------------------------
   -- Get_Immediate_Macro_Name_For_Diagnostics --
   ----------------------------------------------

   function Get_Immediate_Macro_Name_For_Diagnostics
     (Loc : Source_Location_T;
      TU  : Translation_Unit_T) return String
   is
      function Get_Immediate_Macro_Name_For_Diagnostics_C
        (Loc : Source_Location_T;
         TU  : Translation_Unit_T) return String_T
        with
          Import, Convention => C,
          External_Name      => "clang_getImmediateMacroNameForDiagnostics";

      Macro_Name_C : constant String_T :=
        Get_Immediate_Macro_Name_For_Diagnostics_C (Loc, TU);
      Macro_Name   : constant String :=
        Get_C_String (Macro_Name_C);
   begin
      Dispose_String (Macro_Name_C);
      return Macro_Name;
   end Get_Immediate_Macro_Name_For_Diagnostics;

end Clang.Extensions;
