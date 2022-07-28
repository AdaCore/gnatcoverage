------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
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

with Clang.CX_String; use Clang.CX_String;

package body Clang.Extensions is

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

   ----------------------------
   -- Is_Macro_Arg_Expansion --
   ----------------------------

   function Is_Macro_Arg_Expansion
     (Loc       : Source_Location_T;
      Start_Loc : access Source_Location_T := null;
      TU        : Translation_Unit_T) return Boolean
   is
      function Is_Macro_Arg_Expansion
        (Loc       : Source_Location_T;
         Start_Loc : access Source_Location_T;
         TU        : Translation_Unit_T) return unsigned
        with
          Import, Convention => C,
          External_Name      => "clang_isMacroArgExpansion";
   begin
      return Is_Macro_Arg_Expansion (Loc, Start_Loc, TU) /= 0;
   end Is_Macro_Arg_Expansion;

end Clang.Extensions;
