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

#include "libclang/CursorVisitor.h"
#include "libclang/CXCursor.h"
#include "libclang/CXSourceLocation.h"
#include "libclang/CXString.h"
#include "libclang/CXTranslationUnit.h"
#include "clang-c/Index.h"
#include "clang-c/Rewrite.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/ParentMapContext.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/Lex/Lexer.h"
#include "clang/Lex/Token.h"
#include "clang/Rewrite/Core/Rewriter.h"

using namespace clang;
using namespace clang::cxcursor;
using namespace clang::cxloc;
using namespace clang::cxstring;
using namespace clang::cxtu;

/* Convert a clang Stmt type to a libclang CXCursor structure.  The CXCursor C
   is simply used to get a relevant declaration and translation unit to tie
   the returned cursor to.  */

CXCursor
MakeCXCursorWithNull(const Stmt *S, CXCursor C)
{
  if(S)
    return MakeCXCursor(S, getCursorDecl(C), getCursorTU(C));
  else
    return clang_getNullCursor();
}

/* Most of the functions are getters around existing clang functions of a
   similar name, for the accepted type(s).  For instance, clang_getCond will
   simply call getCond on the given node, if this is a statement or an
   expression that has this getter (an IfStmt, WhileStmt etc.).  Some checks
   are done, to make sure this is a supported type, before actually calling the
   wrapped function on the casted node.  */

extern "C" CXCursor
clang_getCond(CXCursor C)
{
  if (clang_isStatement(C.kind) || clang_isExpression(C.kind))
    if (const Stmt *S = cxcursor::getCursorStmt(C))
      switch (S->getStmtClass())
        {
          default:
            return clang_getNullCursor();
          case Stmt::IfStmtClass:
            return MakeCXCursorWithNull(cast<IfStmt>(S)->getCond(), C);
          case Stmt::WhileStmtClass:
            return MakeCXCursorWithNull(cast<WhileStmt>(S)->getCond(), C);
          case Stmt::ForStmtClass:
            return MakeCXCursorWithNull(cast<ForStmt>(S)->getCond(), C);
          case Stmt::SwitchStmtClass:
            return MakeCXCursorWithNull(cast<SwitchStmt>(S)->getCond(), C);
          case Stmt::DoStmtClass:
            return MakeCXCursorWithNull(cast<DoStmt>(S)->getCond(), C);
          case Stmt::ConditionalOperatorClass:
            return MakeCXCursorWithNull
              (cast<ConditionalOperator>(S)->getCond(), C);
        }
  return clang_getNullCursor();
}

extern "C" CXCursor
clang_getBody(CXCursor C)
{
  if (clang_isDeclaration(C.kind))
    {
      if (const Decl *D = cxcursor::getCursorDecl(C))
        return MakeCXCursorWithNull(D->getBody(),C);
    }
  else if (clang_isStatement(C.kind))
    if (const Stmt *S = cxcursor::getCursorStmt(C))
      switch (S->getStmtClass())
        {
          default:
            return clang_getNullCursor();
          case Stmt::WhileStmtClass:
            return MakeCXCursorWithNull(cast<WhileStmt>(S)->getBody(), C);
          case Stmt::ForStmtClass:
            return MakeCXCursorWithNull(cast<ForStmt>(S)->getBody(), C);
          case Stmt::DoStmtClass:
            return MakeCXCursorWithNull(cast<DoStmt>(S)->getBody(), C);
          case Stmt::SwitchStmtClass:
            return MakeCXCursorWithNull(cast<SwitchStmt>(S)->getBody(), C);
        }
  return clang_getNullCursor();
}

extern "C" CXCursor
clang_getForInit(CXCursor C)
{
  if (clang_isStatement(C.kind))
    if (const Stmt *S = cxcursor::getCursorStmt(C))
      switch (S->getStmtClass())
        {
          default:
            return clang_getNullCursor();
          case Stmt::ForStmtClass:
            return MakeCXCursorWithNull(cast<ForStmt>(S)->getInit(), C);
        }
  return clang_getNullCursor();
}

extern "C" CXCursor
clang_getForInc(CXCursor C)
{
  if (clang_isStatement(C.kind))
    if (const Stmt *S = cxcursor::getCursorStmt(C))
      switch (S->getStmtClass())
        {
          default:
            return clang_getNullCursor();
          case Stmt::ForStmtClass:
            return MakeCXCursorWithNull(cast<ForStmt>(S)->getInc(), C);
        }
  return clang_getNullCursor();
}

extern "C" CXCursor
clang_getThen(CXCursor C)
{
  if (clang_isStatement(C.kind))
    if (const Stmt *S = cxcursor::getCursorStmt(C))
      switch (S->getStmtClass())
        {
          default:
            return clang_getNullCursor();
          case Stmt::IfStmtClass:
            return MakeCXCursorWithNull(cast<IfStmt>(S)->getThen(), C);
        }
  return clang_getNullCursor();
}

extern "C" CXCursor
clang_getElse(CXCursor C)
{
  if (clang_isStatement(C.kind))
    if (const Stmt *S = cxcursor::getCursorStmt(C))
      switch (S->getStmtClass())
        {
          default:
            return clang_getNullCursor();
          case Stmt::IfStmtClass:
            return MakeCXCursorWithNull(cast<IfStmt>(S)->getElse(), C);
        }
  return clang_getNullCursor();
}

extern "C" CXCursor
clang_getSubExpr(CXCursor C)
{
  if (clang_isExpression (C.kind))
    if (const Stmt *S = cxcursor::getCursorStmt(C))
      switch (S->getStmtClass())
        {
          default:
            return clang_getNullCursor();
          case Stmt::UnaryOperatorClass:
            return MakeCXCursorWithNull
              (cast<UnaryOperator>(S)->getSubExpr(), C);
        }
  return clang_getNullCursor();
}

extern "C" CXCursor
clang_getSubStmt(CXCursor C)
{
  if (clang_isStatement(C.kind))
    if (const Stmt *S = cxcursor::getCursorStmt(C))
      switch (S->getStmtClass())
        {
          default:
            return clang_getNullCursor();
          case Stmt::CaseStmtClass:
            return MakeCXCursorWithNull(cast<CaseStmt>(S)->getSubStmt(), C);
          case Stmt::DefaultStmtClass:
            return MakeCXCursorWithNull(cast<DefaultStmt>(S)->getSubStmt(), C);
        }
  return clang_getNullCursor();
}

extern "C" CXCursor
clang_getRHS(CXCursor C)
{
  if (clang_isExpression (C.kind))
    if (const Stmt *S = cxcursor::getCursorStmt(C))
      switch (S->getStmtClass())
        {
          default:
            return clang_getNullCursor();
          case Stmt::BinaryOperatorClass:
            return MakeCXCursorWithNull(cast<BinaryOperator>(S)->getRHS(), C);
          case Stmt::ConditionalOperatorClass:
            return MakeCXCursorWithNull
              (cast<ConditionalOperator>(S)->getRHS(), C);
        }
  return clang_getNullCursor();
}

extern "C" CXCursor
clang_getLHS(CXCursor C)
{
  if (clang_isExpression(C.kind))
    if (const Stmt *S = cxcursor::getCursorStmt(C))
      switch (S->getStmtClass())
        {
          default:
            return clang_getNullCursor();
          case Stmt::BinaryOperatorClass:
            return MakeCXCursorWithNull(cast<BinaryOperator>(S)->getLHS(), C);
          case Stmt::ConditionalOperatorClass:
            return MakeCXCursorWithNull
              (cast<ConditionalOperator>(S)->getRHS(), C);
        }
  return clang_getNullCursor();
}

/* Return the string representative of the operator for a binary or unary
   operator node.  */

extern "C" CXString
clang_getOpcodeStr(CXCursor C)
{
  if (clang_isExpression(C.kind))
    if (const Stmt *S = cxcursor::getCursorStmt(C))
      switch (S->getStmtClass())
        {
          default:
            return createEmpty();
          case Stmt::BinaryOperatorClass:
            return createRef(BinaryOperator::getOpcodeStr
                             (cast<BinaryOperator>(S)->getOpcode()));
          case Stmt::UnaryOperatorClass:
            return createRef(UnaryOperator::getOpcodeStr
                             (cast<UnaryOperator>(S)->getOpcode()));
        }
  return createEmpty();
}

/* Return the location of the operator for a binary / unary operator node.  */

extern "C" CXSourceLocation
clang_getOperatorLoc(CXCursor C)
{
  ASTUnit *CXXUnit = cxtu::getASTUnit(getCursorTU(C));
  if (!CXXUnit)
    return clang_getNullLocation();
  SourceLocation sloc;
  if (clang_isExpression(C.kind))
    if (const Stmt *S = cxcursor::getCursorStmt(C))
      switch (S->getStmtClass())
        {
          default:
            return clang_getNullLocation();
          case Stmt::BinaryOperatorClass:
            sloc = cast<BinaryOperator>(S)->getOperatorLoc();
            break;
          case Stmt::UnaryOperatorClass:
            sloc = cast<UnaryOperator>(S)->getOperatorLoc();
            break;
        }
  return cxloc::translateSourceLocation(CXXUnit->getASTContext(), sloc);
}

/* If the given expression is a paren expression, return the outermost
   expression inside that is not a paren expression.  */

extern "C" CXCursor
clang_unwrap(CXCursor C)
{
  if (clang_isExpression(C.kind))
    if (const Stmt *S = cxcursor::getCursorStmt(C))
      if (S->getStmtClass() == Stmt::ParenExprClass)
        return clang_unwrap(MakeCXCursorWithNull
                            (cast<ParenExpr>(S)->getSubExpr(), C));
  return C;
}

/*  Visit a node, starting by itself (in contrary to clang_visitChildren that
    immediately starts with the children of the node).  */

extern "C" unsigned
clang_visit(CXCursor parent, CXCursorVisitor visitor,
            CXClientData client_data)
{
  cxcursor::CursorVisitor CursorVis(getCursorTU(parent), visitor, client_data,
                                    /*VisitPreprocessorLast=*/false);
  return CursorVis.Visit(parent);
}

/* Returns the parent node of the given node.  This accepts statements and
   expressions, unlike clang_getLexicalParent and clang_getCursorParent.  */

extern "C" CXCursor
clang_getParent(CXCursor C)
{
  assert(clang_isStatement(C.kind) || clang_isExpression(C.kind));
  ASTUnit *astUnit = cxtu::getASTUnit(getCursorTU(C));
  ASTContext &astContext = astUnit->getASTContext();
  if (const Stmt *S = cxcursor::getCursorStmt(C))
    {
      const auto Parents = astContext.getParents(*S);
      if (Parents.empty())
        return clang_getNullCursor();
      const auto &SParent = Parents[0];
      if (const auto *Res = SParent.get<Stmt>())
        return MakeCXCursorWithNull(Res, C);
    }
  return clang_getNullCursor();
}

/* Wrappers around rewriting functions.  */

extern "C" void
clang_CXRewriter_insertTextAfter(CXRewriter Rew, CXSourceLocation Loc,
                                 const char *Insert)
{
  assert(Rew);
  clang::Rewriter &R = *reinterpret_cast<clang::Rewriter *>(Rew);
  R.InsertTextAfter(clang::cxloc::translateSourceLocation(Loc), Insert);
}

extern "C" void
clang_CXRewriter_insertTextAfterToken(CXRewriter Rew, CXSourceLocation Loc,
                                      const char *Insert)
{
  assert(Rew);
  clang::Rewriter &R = *reinterpret_cast<clang::Rewriter *>(Rew);
  R.InsertTextAfterToken(clang::cxloc::translateSourceLocation(Loc), Insert);
}

extern "C" unsigned
clang_CXRewriter_isRewritable(CXSourceLocation Loc)
{
  return clang::Rewriter::isRewritable
    (clang::cxloc::translateSourceLocation(Loc)) ? 1 : 0;
}
