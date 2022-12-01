/****************************************************************************
 *                                                                          *
 *                              GNATcoverage                                *
 *                                                                          *
 *                        Copyright (C) 2021, AdaCore                       *
 *                                                                          *
 * GNATcoverage is free software; you can redistribute it and/or modify it  *
 * under terms of the GNU General Public License as published by the  Free  *
 * Software  Foundation;  either version 3,  or (at your option) any later  *
 * version. This software is distributed in the hope that it will be useful *
 * but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- *
 * TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public *
 * License for  more details.  You should have  received  a copy of the GNU *
 * General  Public  License  distributed  with  this  software;   see  file *
 * COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy *
 * of the license.                                                          *
 *                                                                          *
 ****************************************************************************/

/* Make sure we refer to the static version of symbols on Windows, not to DLL
   importers.  */

#define CINDEX_NO_EXPORTS

#include "libclang/CXCursor.h"
#include "libclang/CXSourceLocation.h"
#include "libclang/CXString.h"
#include "libclang/CXTranslationUnit.h"
#include "libclang/CursorVisitor.h"
#include "clang-c/Index.h"
#include "clang-c/Rewrite.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/ParentMapContext.h"
#include "clang/AST/StmtCXX.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/Lex/Lexer.h"
#include "clang/Lex/Token.h"
#include "clang/Rewrite/Core/Rewriter.h"

using namespace clang;
using namespace clang::cxcursor;
using namespace clang::cxloc;
using namespace clang::cxstring;
using namespace clang::cxtu;

/* Return the AST context corresponding to the given translation unit TU.  */

static ASTContext &
getContext (CXTranslationUnit TU)
{
  return getASTUnit (TU)->getASTContext ();
}

/* Likewise, but starting from a cursor.  */

static ASTContext &
getContext (CXCursor C)
{
  return getContext (getCursorTU (C));
}

/* Return the SourceManager corresponding to the given translation unit TU.  */

static const SourceManager &
getSourceManager (CXTranslationUnit TU)
{
  return getASTUnit (TU)->getSourceManager ();
}

/* Translate a source location to a cursor source location in the given
   translation unit TU.  */

static CXSourceLocation
translateSLoc (CXTranslationUnit TU, SourceLocation Loc)
{
  return translateSourceLocation (getContext (TU), Loc);
}

/* Convert a clang Stmt type to a libclang CXCursor structure.  The CXCursor C
   is simply used to get a relevant declaration and translation unit to tie
   the returned cursor to.  */

CXCursor
MakeCXCursorWithNull (const Stmt *S, CXCursor C)
{
  if (S)
    return MakeCXCursor (S, getCursorDecl (C), getCursorTU (C));
  else
    return clang_getNullCursor ();
}

CXCursor
MakeCXCursorWithNull (const Decl *decl, CXCursor C)
{
  if (decl)
    return MakeCXCursor (decl, getCursorTU (C));
  else
    return clang_getNullCursor ();
}

/* Most of the functions are getters around existing clang functions of a
   similar name, for the accepted type(s).  For instance, clang_getCond will
   simply call getCond on the given node, if this is a statement or an
   expression that has this getter (an IfStmt, WhileStmt etc.).  Some checks
   are done, to make sure this is a supported type, before actually calling the
   wrapped function on the casted node.  */

extern "C" CXCursor
clang_getCond (CXCursor C)
{
  if (clang_isStatement (C.kind) || clang_isExpression (C.kind))
    if (const Stmt *S = getCursorStmt (C))
      switch (S->getStmtClass ())
	{
	case Stmt::IfStmtClass:
	  return MakeCXCursorWithNull (cast<IfStmt> (S)->getCond (), C);
	case Stmt::WhileStmtClass:
	  return MakeCXCursorWithNull (cast<WhileStmt> (S)->getCond (), C);
	case Stmt::ForStmtClass:
	  return MakeCXCursorWithNull (cast<ForStmt> (S)->getCond (), C);
	case Stmt::SwitchStmtClass:
	  return MakeCXCursorWithNull (cast<SwitchStmt> (S)->getCond (), C);
	case Stmt::DoStmtClass:
	  return MakeCXCursorWithNull (cast<DoStmt> (S)->getCond (), C);
	case Stmt::ConditionalOperatorClass:
	  return MakeCXCursorWithNull (
	    cast<ConditionalOperator> (S)->getCond (), C);
	default:
	  return clang_getNullCursor ();
	}
  return clang_getNullCursor ();
}

extern "C" CXCursor
clang_getBody (CXCursor C)
{
  if (clang_isDeclaration (C.kind))
    {
      if (const Decl *D = getCursorDecl (C))
	switch (D->getKind ())
	  {
	  case Decl::FunctionTemplate:
	    return MakeCXCursorWithNull (
	      cast<FunctionTemplateDecl> (D)->getTemplatedDecl ()->getBody (),
	      C);
	  case Decl::Function:
	  case Decl::CXXMethod:
	  case Decl::CXXConstructor:
	  case Decl::CXXDestructor:
	  case Decl::CXXConversion:
	    {
	      const FunctionDecl *FD = cast<FunctionDecl> (D);
	      if (FD->doesThisDeclarationHaveABody ())
		return MakeCXCursorWithNull (FD->getBody (), C);
	      break;
	    }
	  default:
	    return MakeCXCursorWithNull (D->getBody (), C);
	  }
    }
  else if (clang_isStatement (C.kind))
    {
      if (const Stmt *S = getCursorStmt (C))
	switch (S->getStmtClass ())
	  {
	  case Stmt::WhileStmtClass:
	    return MakeCXCursorWithNull (cast<WhileStmt> (S)->getBody (), C);
	  case Stmt::ForStmtClass:
	    return MakeCXCursorWithNull (cast<ForStmt> (S)->getBody (), C);
	  case Stmt::CXXForRangeStmtClass:
	    return MakeCXCursorWithNull (cast<CXXForRangeStmt> (S)->getBody (),
					 C);
	  case Stmt::DoStmtClass:
	    return MakeCXCursorWithNull (cast<DoStmt> (S)->getBody (), C);
	  case Stmt::SwitchStmtClass:
	    return MakeCXCursorWithNull (cast<SwitchStmt> (S)->getBody (), C);
	  default:
	    return clang_getNullCursor ();
	  }
    }
  else if (clang_isExpression (C.kind))
    if (const Expr *E = getCursorExpr (C))
      switch (E->getStmtClass ())
	{
	case Expr::LambdaExprClass:
	  return MakeCXCursorWithNull (cast<LambdaExpr> (E)->getBody (), C);
	default:
	  return clang_getNullCursor ();
	}
  return clang_getNullCursor ();
}

extern "C" CXCursor
clang_getForInit (CXCursor C)
{
  if (clang_isStatement (C.kind))
    if (const Stmt *S = getCursorStmt (C))
      switch (S->getStmtClass ())
	{
	case Stmt::ForStmtClass:
	  return MakeCXCursorWithNull (cast<ForStmt> (S)->getInit (), C);
	case Stmt::CXXForRangeStmtClass:
	  return MakeCXCursorWithNull (cast<CXXForRangeStmt> (S)->getInit (),
				       C);
	default:
	  return clang_getNullCursor ();
	}
  return clang_getNullCursor ();
}

extern "C" CXCursor
clang_getForRangeExpr (CXCursor C)
{
  const Stmt *stmt;
  const CXXForRangeStmt *for_stmt;

  if (clang_isStatement (C.kind) && (stmt = cxcursor::getCursorStmt (C))
      && stmt->getStmtClass () == Stmt::CXXForRangeStmtClass)
    {
      for_stmt = cast<CXXForRangeStmt> (stmt);
      return MakeCXCursorWithNull (for_stmt->getRangeInit (), C);
    }
  return clang_getNullCursor ();
}

extern "C" CXCursor
clang_getForInc (CXCursor C)
{
  if (clang_isStatement (C.kind))
    if (const Stmt *S = getCursorStmt (C))
      switch (S->getStmtClass ())
	{
	case Stmt::ForStmtClass:
	  return MakeCXCursorWithNull (cast<ForStmt> (S)->getInc (), C);
	default:
	  return clang_getNullCursor ();
	}
  return clang_getNullCursor ();
}

extern "C" CXCursor
clang_getCondVar (CXCursor C)
{
  const Stmt *stmt;
  const WhileStmt *while_stmt;
  const VarDecl *decl;

  if (clang_isStatement (C.kind) && (stmt = getCursorStmt (C))
      && stmt->getStmtClass () == Stmt::WhileStmtClass)
    {
      while_stmt = cast<WhileStmt> (stmt);
      if (decl = while_stmt->getConditionVariable ())
	return MakeCXCursorWithNull (decl, C);
    }
  return clang_getNullCursor ();
}

extern "C" CXCursor
clang_getVarInitExpr (CXCursor C)
{
  const Decl *decl;
  const VarDecl *var_decl;

  if (clang_isDeclaration (C.kind) && (decl = getCursorDecl (C))
      && decl->getKind () == Decl::Kind::Var)
    {
      var_decl = cast<VarDecl> (decl);
      return MakeCXCursorWithNull (var_decl->getInit (), C);
    }
  return clang_getNullCursor ();
}

extern "C" CXCursor
clang_getThen (CXCursor C)
{
  if (clang_isStatement (C.kind))
    if (const Stmt *S = getCursorStmt (C))
      switch (S->getStmtClass ())
	{
	case Stmt::IfStmtClass:
	  return MakeCXCursorWithNull (cast<IfStmt> (S)->getThen (), C);
	default:
	  return clang_getNullCursor ();
	}
  return clang_getNullCursor ();
}

extern "C" CXCursor
clang_getElse (CXCursor C)
{
  if (clang_isStatement (C.kind))
    if (const Stmt *S = getCursorStmt (C))
      switch (S->getStmtClass ())
	{
	case Stmt::IfStmtClass:
	  return MakeCXCursorWithNull (cast<IfStmt> (S)->getElse (), C);
	default:
	  return clang_getNullCursor ();
	}
  return clang_getNullCursor ();
}

extern "C" CXSourceLocation
clang_getElseLoc (CXCursor C)
{
  if (clang_isStatement (C.kind))
    if (const Stmt *S = getCursorStmt (C))
      switch (S->getStmtClass ())
	{
	case Stmt::IfStmtClass:
	  return translateSLoc (getCursorTU (C), cast<IfStmt> (S)->getElseLoc ());
	default:
	  return clang_getNullLocation ();
	}
  return clang_getNullLocation ();
}

extern "C" CXSourceLocation
clang_getWhileLoc (CXCursor C)
{
  if (clang_isStatement (C.kind))
    if (const Stmt *S = getCursorStmt (C))
      switch (S->getStmtClass ())
	{
	case Stmt::DoStmtClass:
	  return translateSLoc (getCursorTU (C), cast<DoStmt> (S)->getWhileLoc ());
	default:
	  return clang_getNullLocation ();
	}
  return clang_getNullLocation ();
}


extern "C" CXCursor
clang_getSubExpr (CXCursor C)
{
  if (clang_isExpression (C.kind))
    if (const Stmt *S = getCursorStmt (C))
      switch (S->getStmtClass ())
	{
	case Stmt::UnaryOperatorClass:
	  return MakeCXCursorWithNull (cast<UnaryOperator> (S)->getSubExpr (),
				       C);
	default:
	  return clang_getNullCursor ();
	}
  return clang_getNullCursor ();
}

extern "C" CXCursor
clang_getSubStmt (CXCursor C)
{
  if (clang_isStatement (C.kind))
    if (const Stmt *S = getCursorStmt (C))
      switch (S->getStmtClass ())
	{
	case Stmt::CaseStmtClass:
	  return MakeCXCursorWithNull (cast<CaseStmt> (S)->getSubStmt (), C);
	case Stmt::DefaultStmtClass:
	  return MakeCXCursorWithNull (cast<DefaultStmt> (S)->getSubStmt (),
				       C);
	default:
	  return clang_getNullCursor ();
	}
  return clang_getNullCursor ();
}

extern "C" CXCursor
clang_getRHS (CXCursor C)
{
  if (clang_isExpression (C.kind))
    if (const Stmt *S = getCursorStmt (C))
      switch (S->getStmtClass ())
	{
	case Stmt::BinaryOperatorClass:
	  return MakeCXCursorWithNull (cast<BinaryOperator> (S)->getRHS (), C);
	case Stmt::ConditionalOperatorClass:
	  return MakeCXCursorWithNull (
	    cast<ConditionalOperator> (S)->getRHS (), C);
	default:
	  return clang_getNullCursor ();
	}
  return clang_getNullCursor ();
}

extern "C" CXCursor
clang_getLHS (CXCursor C)
{
  if (clang_isExpression (C.kind))
    if (const Stmt *S = getCursorStmt (C))
      switch (S->getStmtClass ())
	{
	case Stmt::BinaryOperatorClass:
	  return MakeCXCursorWithNull (cast<BinaryOperator> (S)->getLHS (), C);
	case Stmt::ConditionalOperatorClass:
	  return MakeCXCursorWithNull (
	    cast<ConditionalOperator> (S)->getLHS (), C);
	default:
	  return clang_getNullCursor ();
	}
  return clang_getNullCursor ();
}

/* Return the name of the declared object. */

extern "C" const CXString
clang_getDeclName (CXCursor C)
{
  if (clang_isDeclaration (C.kind))
    {
      auto getFunctionDeclName = [] (const FunctionDecl *FD) {
	const DeclarationName FunctionName = FD->getNameInfo ().getName ();
	return createDup (FunctionName.getAsString ().c_str ());
      };

      const Decl *D = getCursorDecl (C);

      switch (D->getKind ())
	{
	case Decl::Function:
	case Decl::CXXMethod:
	case Decl::CXXConstructor:
	case Decl::CXXDestructor:
	  {
	    const clang::FunctionDecl *FD = cast<clang::FunctionDecl> (D);
	    return getFunctionDeclName (FD);
	  }
	case Decl::FunctionTemplate:
	  {
	    const clang::FunctionDecl *FD
	      = (cast<clang::FunctionTemplateDecl> (D))->getTemplatedDecl ();
	    return getFunctionDeclName (FD);
	  }

	case Decl::Namespace:
	  {
	    const NamespaceDecl *ND
	      = (cast<clang::NamespaceDecl> (D))->getCanonicalDecl ();

	    if (ND->isAnonymousNamespace ())
	      return createDup ("Anonymous namespace");

	    return createDup (ND->getName ());
	  }
	case Decl::ClassTemplate:

	  {
	    const clang::ClassTemplateDecl *CD
	      = (cast<clang::ClassTemplateDecl> (D))->getCanonicalDecl ();
	    return createDup (CD->getName ());
	  }
	case Decl::CXXRecord:
	  {
	    const clang::CXXRecordDecl *RD
	      = (cast<clang::CXXRecordDecl> (D))->getCanonicalDecl ();
	    return createDup (RD->getName ());
	  }

	default:
	  return createEmpty ();
	}
    }

  return createEmpty ();
}

/* Return the string representative of the operator for a binary or unary
   operator node.  */

extern "C" CXString
clang_getOpcodeStr (CXCursor C)
{
  if (clang_isExpression (C.kind))
    if (const Stmt *S = getCursorStmt (C))
      switch (S->getStmtClass ())
	{
	case Stmt::BinaryOperatorClass:
	  return createRef (BinaryOperator::getOpcodeStr (
	    cast<BinaryOperator> (S)->getOpcode ()));
	case Stmt::UnaryOperatorClass:
	  return createRef (UnaryOperator::getOpcodeStr (
	    cast<UnaryOperator> (S)->getOpcode ()));
	default:
	  return createEmpty ();
	}
  return createEmpty ();
}

/* Return the location of the operator for a binary / unary operator node.  */

extern "C" CXSourceLocation
clang_getOperatorLoc (CXCursor C)
{
  CXTranslationUnit TU = getCursorTU (C);
  SourceLocation sloc;
  if (clang_isExpression (C.kind))
    if (const Stmt *S = getCursorStmt (C))
      switch (S->getStmtClass ())
	{
	case Stmt::BinaryOperatorClass:
	  sloc = cast<BinaryOperator> (S)->getOperatorLoc ();
	  break;
	case Stmt::UnaryOperatorClass:
	  sloc = cast<UnaryOperator> (S)->getOperatorLoc ();
	  break;
	default:
	  return clang_getNullLocation ();
	}
  return translateSLoc (TU, sloc);
}

/* If the given expression is a wrapping expression (i.e. a parenthesized
   expression, a cast expression etc.), return the outermost expression inside
   that is not a wrapping expression.  */

extern "C" CXCursor
clang_unwrap (CXCursor C)
{
  if (clang_isExpression (C.kind))
    if (const Stmt *S = getCursorStmt (C))
      switch (S->getStmtClass ())
	{
	case Stmt::ParenExprClass:
	  return clang_unwrap (
	    MakeCXCursorWithNull (cast<ParenExpr> (S)->getSubExpr (), C));
	case Expr::ConstantExprClass:
	case Expr::ExprWithCleanupsClass:
	  return clang_unwrap (
	    MakeCXCursorWithNull (cast<FullExpr> (S)->getSubExpr (), C));
	case Expr::ImplicitCastExprClass:
	case Expr::CStyleCastExprClass:
	case Expr::CXXFunctionalCastExprClass:
	case Expr::CXXStaticCastExprClass:
	case Expr::CXXDynamicCastExprClass:
	case Expr::CXXReinterpretCastExprClass:
	case Expr::CXXConstCastExprClass:
	case Expr::CXXAddrspaceCastExprClass:
	  return clang_unwrap (
	    MakeCXCursorWithNull (cast<CastExpr> (S)->getSubExpr (), C));
	}
  return C;
}

/*  Visit a node, starting by itself (in contrary to clang_visitChildren that
    immediately starts with the children of the node).  */

extern "C" unsigned
clang_visit (CXCursor parent, CXCursorVisitor visitor,
	     CXClientData client_data)
{
  CursorVisitor CursorVis (getCursorTU (parent), visitor, client_data,
			   /*VisitPreprocessorLast=*/false);
  return CursorVis.Visit (parent);
}

/* Returns the parent node of the given node.  This accepts statements and
   expressions, unlike clang_getLexicalParent and clang_getCursorParent.  */

extern "C" CXCursor
clang_getParent (CXCursor C)
{
  assert (clang_isStatement (C.kind) || clang_isExpression (C.kind));
  if (const Stmt *S = getCursorStmt (C))
    {
      const auto Parents = getContext (C).getParents (*S);
      if (Parents.empty ())
	return clang_getNullCursor ();
      const auto &SParent = Parents[0];
      if (const auto *Res = SParent.get<Stmt> ())
	return MakeCXCursorWithNull (Res, C);
    }
  return clang_getNullCursor ();
}

/* Wrappers around rewriting functions.  */

extern "C" void
clang_CXRewriter_insertTextAfter (CXRewriter Rew, CXSourceLocation Loc,
				  const char *Insert)
{
  assert (Rew);
  Rewriter &R = *reinterpret_cast<Rewriter *> (Rew);
  R.InsertTextAfter (translateSourceLocation (Loc), Insert);
}

extern "C" void
clang_CXRewriter_insertTextAfterToken (CXRewriter Rew, CXSourceLocation Loc,
				       const char *Insert)
{
  assert (Rew);
  Rewriter &R = *reinterpret_cast<Rewriter *> (Rew);
  R.InsertTextAfterToken (translateSourceLocation (Loc), Insert);
}

/* Wrappers around source location analysis functions.  */

extern "C" unsigned
clang_isMacroLocation (CXSourceLocation Loc)
{
  const SourceLocation SLoc = translateSourceLocation (Loc);
  return SLoc.isMacroID () ? 1 : 0;
}

extern "C" unsigned
clang_isMacroArgExpansion (CXSourceLocation Loc, CXSourceLocation *StartLoc,
			   CXTranslationUnit TU)
{
  const SourceManager &SM = getSourceManager (TU);
  const SourceLocation SLoc = translateSourceLocation (Loc);
  SourceLocation Result;
  if (SM.isMacroArgExpansion (SLoc, &Result))
    {
      *StartLoc = translateSLoc (TU, Result);
      return 1;
    }
  return 0;
}

extern "C" CXSourceLocation
clang_getImmediateMacroCallerLoc (CXSourceLocation Loc, CXTranslationUnit TU)
{
  const SourceManager &SM = getSourceManager (TU);
  SourceLocation SLoc = translateSourceLocation (Loc);
  if (SLoc.isMacroID ())
    return translateSLoc (TU, SM.getImmediateMacroCallerLoc (SLoc));
  return Loc;
}

extern "C" CXSourceLocation
clang_getImmediateExpansionLoc (CXSourceLocation Loc, CXTranslationUnit TU)
{
  const SourceManager &SM = getSourceManager (TU);
  SourceLocation SLoc = translateSourceLocation (Loc);
  return translateSLoc (TU, SM.getImmediateExpansionRange (SLoc).getBegin ());
}

extern "C" CXString
clang_getImmediateMacroNameForDiagnostics (CXSourceLocation Loc,
					   CXTranslationUnit TU)
{
  SourceLocation SLoc = translateSourceLocation (Loc);
  const SourceManager &SM = getSourceManager (TU);
  return createDup (Lexer::getImmediateMacroNameForDiagnostics (
    SLoc, SM, getContext (TU).getLangOpts ()));
}

extern "C" CXSourceLocation
clang_getExpansionEnd (CXTranslationUnit TU, CXSourceLocation Loc)
{
  SourceLocation SLoc = translateSourceLocation (Loc);
  const SourceManager &SM = getSourceManager (TU);
  return translateSLoc (TU, SM.getExpansionRange (SLoc).getEnd ());
}

extern "C" CXTranslationUnit
clang_getCursorTU (CXCursor C)
{
  return getCursorTU (C);
}

/* Debug helpers.  */

extern "C" void
clang_printLocation (CXTranslationUnit TU, CXSourceLocation Loc)
{
  const SourceManager &SM = getSourceManager (TU);
  translateSourceLocation (Loc).dump (SM);
}
