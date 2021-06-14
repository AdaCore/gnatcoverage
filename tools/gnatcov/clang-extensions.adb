------------------------------------------------------------------------------
--                                                                          --
--                               GNATcoverage                               --
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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

with Interfaces.C.Strings; use Interfaces.C.Strings;

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

end Clang.Extensions;
