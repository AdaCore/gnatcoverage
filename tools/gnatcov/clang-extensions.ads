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

--  This package binds the clang extensions that are defined in
--  clang-wrapper.cc. We need to extend the libclang library as it does not
--  provide enough utilities for gnatcov usage.

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

with Clang.Index;   use Clang.Index;
with Clang.Rewrite; use Clang.Rewrite;

package Clang.Extensions is

   function Get_Body (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getBody";

   function Get_Cond (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getCond";

   function Get_For_Init (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getForInit";

   function Get_For_Inc (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getForInc";

   function Get_Then (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getThen";

   function Get_Else (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getElse";

   function Get_Sub_Expr (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getSubExpr";

   function Get_Sub_Stmt (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getSubStmt";

   function Get_LHS (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getLHS";

   function Get_RHS (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getRHS";

   function Get_Operator_Loc (C : Cursor_T) return Source_Location_T
     with Import, Convention => C, External_Name => "clang_getOperatorLoc";

   function Get_Opcode_Str (C : Cursor_T) return String with Inline;

   function Unwrap (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_unwrap";

   function Get_Parent (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getParent";

   function Visit
     (Parent      : Cursor_T;
      Visitor     : Cursor_Visitor_T;
      Client_Data : Client_Data_T) return unsigned
     with Import, Convention => C, External_Name => "clang_visit";
   --  Same as Visit_Children, but starts with visiting the node before
   --  visiting its children.

   procedure CX_Rewriter_Insert_Text_After
     (Rew    : Rewriter_T;
      Loc    : Source_Location_T;
      Insert : String)
     with Inline;
   --  Insert the text Insert before the given location, and after any
   --  previously inserted string (at the same location).

   procedure CX_Rewriter_Insert_Text_After_Token
     (Rew : Rewriter_T;
      Loc : Source_Location_T;
      Insert : String)
     with Inline;
   --  Insert the text Insert after the token at the given location, and after
   --  any previously inserted string (at the same location).

end Clang.Extensions;