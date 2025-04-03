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

--  This package binds the clang extensions that are defined in
--  clang-wrapper.cc. We need to extend the libclang library as it does not
--  provide enough utilities for gnatcov usage.

with Interfaces.C; use Interfaces.C;

with Clang.CX_Source_Location; use Clang.CX_Source_Location;
with Clang.Index;              use Clang.Index;
with Clang.Rewrite;            use Clang.Rewrite;

with Slocs; use Slocs;

package Clang.Extensions is

   function Get_Body (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getBody";

   function Get_Function_Signature_Sloc (C : Cursor_T) return Source_Range_T
      with
         Import,
         Convention => C,
         External_Name => "clang_getFunctionSignatureSloc";
   --  Given a FunctionDecl or LambdaExpr, returns the Source Range of the
   --  signature, thus excluding the body if there is one.

   function Get_Cond (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getCond";

   function Get_For_Init (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getForInit";

   function Get_For_Inc (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getForInc";

   function Get_Cond_Var (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getCondVar";

   function Get_Var_Init_Expr (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getVarInitExpr";

   function Get_For_Range_Expr (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getForRangeExpr";

   function Get_Then (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getThen";

   function Get_Else (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getElse";

   function Get_Else_Loc (C : Cursor_T) return Source_Location_T
     with Import, Convention => C, External_Name => "clang_getElseLoc";
   --  If this cursor is an if statement with an else part, return the
   --  else location.Otherwise, return a null location.

   function Get_While_Loc (C : Cursor_T) return Source_Location_T
     with Import, Convention => C, External_Name => "clang_getWhileLoc";
   --  If this cursor is a do / while statement, return the while location.
   --  Otherwise return a null location.

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

   function Get_First_Decl (C : Cursor_T) return Cursor_T
     with Import, Convention => C, External_Name => "clang_getFirstDecl";
   --  Given a Decl_Stmt, return the only declaration if it is a single decl,
   --  and the first of the declaration list otherwise.

   function Get_Opcode_Str (C : Cursor_T) return String with Inline;

   function Get_Decl_Name_Str (C : Cursor_T) return String with Inline;

   function Get_Callee_Name_Str (C : Cursor_T) return String with Inline;

   function Is_This_Declaration_A_Definition (C : Cursor_T) return Boolean
     with Inline;
   --  Given a function or function template declaration, return true if
   --  the declaration is indeed a definition (i.e. it has a body).
   --  Providing another kind of node may return False;

   function Get_LBrac_Loc_Plus_One (C : Cursor_T) return Source_Location_T
     with Import, Convention => C,
          External_Name => "clang_getLBracLocPlusOne";
   --  Given a clang::CompoundStmt (which is a possibly empty list of
   --  statements surrounded by brackets), return the Location just after its
   --  opening bracket. Giving another kind of cursor will return a null
   --  location.

   function Is_Instrumentable_Call_Expr (C : Cursor_T) return Boolean
     with Inline;
   --  Given a cursor C, return True if the cursor kind is CallExpr AND if
   --  the underlying C++ statement class is one of:
   --  - Stmt::CallExprClass
   --  - Stmt::CXXOperatorCallExprClass
   --  - Stmt::CXXMemberCallExprClass
   --
   --  CXCursor_CallExpr is also used for other kinds of nodes that we do not
   --  wish to instrument for function coverage, and that we need to use the
   --  C++ API to detect.
   --
   --  TODO??? Actually decide what to do for the rest, so Ctor/Dtor call
   --          coverage makes sense.

   function Is_Prefixed_CXX_Member_Call_Expr (C : Cursor_T) return Boolean
      with Inline;
   --  Return True if the given cursor is a statement with type
   --  Stmt::CXXMemberCallExprClass, and if it is a prefixed method call
   --  (meaning not a method that is called from the body of another method
   --  in which it is possible to simply omit `this->`).

   function Is_Struct_Field_Call_Expr (C : Cursor_T) return Boolean;
   --  Return true if the cursor points to a CallExpr, whose callee is a
   --  MemberExpr.

   function Get_CXX_Member_Call_Expr_SCO_Sloc_Range
     (C : Cursor_T) return Source_Range_T
      with
         Import,
         Convention => C,
         External_Name => "clang_getCXXMemberCallExprSCOSlocRange";
   --  Assume the given cursor is a Stmt::CXXMemberCallExprClass.
   --  Given the expression is `Foo.Bar(Baz)`, it will return a source range
   --  containing `.Bar`.

   function Get_CXX_Member_Call_Expr_Base_Sloc_Range
     (C : Cursor_T) return Source_Range_T
      with
         Import,
         Convention => C,
         External_Name => "clang_getCXXMemberCallExprBaseSlocRange";
   --  Assume the given cursor is a Stmt::CXXMemberCallExprClass.
   --  Given the expression is `Foo.Bar(Baz)`, it will return the source
   --  range of `Foo`.

   function Get_Struct_Field_Call_Expr_SCO_Sloc_Range
     (C : Cursor_T) return Source_Range_T
      with
         Import,
         Convention => C,
         External_Name => "clang_getStructFieldCallExprSCOSlocRange";
   --  Given the expression is `Foo.Bar(Baz)`, it will return the source range
   --  for `.Bar(Baz)`.

   function Get_Struct_Field_Call_Expr_Base_Sloc_Range
     (C : Cursor_T) return Source_Range_T
      with
         Import,
         Convention => C,
         External_Name => "clang_getStructFieldCallExprBaseSlocRange";
   --  Given the expression is `Foo.Bar(Baz)`, it will return the source range
   --  for `Foo`.

   function Is_Constexpr (C : Cursor_T) return Boolean with Inline;

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
     (Rew    : Rewriter_T;
      Loc    : Source_Location_T;
      Insert : String)
     with Inline;
   --  Insert the text Insert after the token at the given location, and after
   --  any previously inserted string (at the same location).

   procedure CX_Rewriter_Insert_Text_Before_Token
     (Rew    : Rewriter_T;
      Loc    : Source_Location_T;
      Insert : String)
     with Inline;
   --  Insert the text Insert before the token at the given location, and after
   --  any previously inserted string (at the same location).

   function CX_Rewriter_Get_Rewritten_Text
     (Rew : Rewriter_T;
      R   : Source_Range_T) return String
     with Inline;
   --  Return the rewritten text for the given source range.

   function Get_Cursor_TU (C : Cursor_T) return Translation_Unit_T
   with
     Import, Convention => C,
     External_Name => "clang_getCursorTU";
   --  Return the translation unit to which C belongs.

   --------------------
   -- Clang wrappers --
   --------------------

   --  The functions below are simply bindings around clang functions, which
   --  are exhaustively documented in the clang sources.

   function Spelling_Location (Loc : Source_Location_T) return Source_Location
     with Inline;
   --  Thick binding for Clang.Index.Get_Spelling_Location

   function File_Location
     (Loc : Source_Location_T) return Local_Source_Location
     with Inline;
   --  Thick binding for Clang.Index.Get_File_Location

   function Presumed_Location
     (Loc : Source_Location_T) return Local_Source_Location;
   --  Thick binding for Clang.Index.Get Presumed_Location

   function Is_Macro_Location (Loc : Source_Location_T) return Boolean
     with Inline;
   --  See isMacroID in clang/Basic/SourceLocation.h.

   function Is_Macro_Arg_Expansion
     (Loc       : Source_Location_T;
      Start_Loc : out Source_Location_T;
      TU        : Translation_Unit_T) return Boolean
      with Inline;
   --  See isMacroArgExpansion in clang/Basic/SourceManager.h.

   function Get_Immediate_Macro_Name_For_Diagnostics
     (Loc : Source_Location_T;
      TU  : Translation_Unit_T) return String
     with Inline;
   --  See getImmediateMacroNameForDiagnostics in clang/Lex/Lexer.h

   function Get_Immediate_Macro_Caller_Loc
     (Loc : Source_Location_T;
      TU  : Translation_Unit_T) return Source_Location_T
     with
       Import, Convention => C,
       External_Name => "clang_getImmediateMacroCallerLoc";
   --  See getImmediateMacroCallerLoc in clang/Basic/SourceManager.h.

   function Get_Immediate_Expansion_Loc
     (Loc : Source_Location_T;
      TU  : Translation_Unit_T) return Source_Location_T
     with
       Import, Convention => C,
       External_Name => "clang_getImmediateExpansionLoc";
   --  See getImmediateExpansionRange in clang/Basic/SourceManager.h.

   function Get_Expansion_End
     (TU  : Translation_Unit_T;
      Loc : Source_Location_T) return Source_Location_T
     with
       Import, Convention => C,
       External_Name => "clang_getExpansionEnd";
   --  See getExpansionRange in clang/Basic/SourceManager.h.

   function Get_Spelling_Loc
     (TU  : Translation_Unit_T;
      Loc : Source_Location_T) return Source_Location_T
     with Import, Convention => C, External_Name => "clang_getSpellingLoc";
   --  See getSpellingLoc in clang/Basic/SourceManager.h

   procedure Print_Location (TU : Translation_Unit_T; Loc : Source_Location_T)
     with Import, Convention => C, External_Name => "clang_printLocation";

   -----------
   -- Utils --
   -----------

   function To_Sloc (Line, Column : unsigned) return Local_Source_Location is
     ((Natural (Line), Natural (Column)));
   --  Convert a Clang local source location to gnatcov's own format

end Clang.Extensions;
