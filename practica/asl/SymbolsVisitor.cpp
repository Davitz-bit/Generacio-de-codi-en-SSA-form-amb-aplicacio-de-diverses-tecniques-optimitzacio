//////////////////////////////////////////////////////////////////////
//
//    SymbolsVisitor - Walk the parser tree to register symbols
//                     for the Asl programming language
//
//    Copyright (C) 2020-2030  Universitat Politecnica de Catalunya
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU General Public License
//    as published by the Free Software Foundation; either version 3
//    of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Affero General Public License for more details.
//
//    You should have received a copy of the GNU Affero General Public
//    License along with this library; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
//
//    contact: José Miguel Rivero (rivero@cs.upc.edu)
//             Computer Science Department
//             Universitat Politecnica de Catalunya
//             despatx Omega.110 - Campus Nord UPC
//             08034 Barcelona.  SPAIN
//
//////////////////////////////////////////////////////////////////////

#include "SymbolsVisitor.h"
#include "antlr4-runtime.h"

#include "../common/TypesMgr.h"
#include "../common/SymTable.h"
#include "../common/TreeDecoration.h"
#include "../common/SemErrors.h"

#include <iostream>
#include <string>
#include <vector>

#include <cstddef>    // std::size_t

// uncomment the following line to enable debugging messages with DEBUG*
// #define DEBUG_BUILD
#include "../common/debug.h"

// using namespace std;


// Constructor
SymbolsVisitor::SymbolsVisitor(TypesMgr       & Types,
                               SymTable       & Symbols,
                               TreeDecoration & Decorations,
                               SemErrors      & Errors) :
  Types{Types},
  Symbols{Symbols},
  Decorations{Decorations},
  Errors{Errors} {
}

// Methods to visit each kind of node:
std::any SymbolsVisitor::visitProgram(AslParser::ProgramContext *ctx) {
  DEBUG_ENTER();
  SymTable::ScopeId sc = Symbols.pushNewScope(SymTable::GLOBAL_SCOPE_NAME);
  putScopeDecor(ctx, sc);
  for (auto ctxFunc : ctx->function()) { 
    visit(ctxFunc);
  }
  Symbols.popScope();
  DEBUG_EXIT();
  return 0;
}

std::any SymbolsVisitor::visitFunction(AslParser::FunctionContext *ctx) {
  DEBUG_ENTER();
  std::string funcName = ctx->ID()->getText();
  SymTable::ScopeId sc = Symbols.pushNewScope(funcName);
  putScopeDecor(ctx, sc);

  // visit(ctx->declarations(0)); /* parameters declaration */
  // visit(ctx->declarations(1)); /* variables declaration */
  if (ctx->parameter_decl())
    visit(ctx->parameter_decl());
  for (auto ctxVariable_decl : ctx->variable_decl())
    visit(ctxVariable_decl);

  Symbols.popScope();

  if (Symbols.findInCurrentScope(funcName)) {
    Errors.declaredIdent(ctx->ID());
    TypesMgr::TypeId errorTy = Types.createErrorTy();
    putTypeDecor(ctx, errorTy);
  }
  else {
    std::vector<TypesMgr::TypeId> lParamsTy;
    // if (ctx->declarations(0)->parameter_decl()) {
    if (ctx->parameter_decl()) {
      // for (auto ctxParameter_declType : ctx->declarations(0)->parameter_decl()->type())
      for (auto ctxParameter_declType : ctx->parameter_decl()->type())
        lParamsTy.push_back(getTypeDecor(ctxParameter_declType));
    }
    TypesMgr::TypeId retTy = Types.createVoidTy();
    if (ctx->basic_type()) {
      visit(ctx->basic_type());
      retTy = getTypeDecor(ctx->basic_type());
    }
    TypesMgr::TypeId funcTy = Types.createFunctionTy(lParamsTy, retTy);
    Symbols.addFunction(funcName, funcTy);
    putTypeDecor(ctx, funcTy);
  }
  DEBUG_EXIT();
  return 0;
}

// std::any SymbolsVisitor::visitDeclarations(AslParser::DeclarationsContext *ctx) {
//   DEBUG_ENTER();
//   visitChildren(ctx);
//   DEBUG_EXIT();
//   return 0;
// }

std::any SymbolsVisitor::visitVariable_decl(AslParser::Variable_declContext *ctx) {
  DEBUG_ENTER();
  for (auto ctxId : ctx->ID()) {
    std::string varName = ctxId->getText();
    if (Symbols.findInCurrentScope(varName)) {
      Errors.declaredIdent(ctxId);
    }
    else {
      visit(ctx->type());
      TypesMgr::TypeId varTy= getTypeDecor(ctx->type());
      Symbols.addLocalVar(varName, varTy);
    }
  }
  DEBUG_EXIT();
  return 0;
}

/* Meu */
std::any SymbolsVisitor::visitParameter_decl(AslParser::Parameter_declContext *ctx) {
  DEBUG_ENTER();
  for (long unsigned int i=0; i<ctx->ID().size(); ++i) {
    std::string paramName = ctx->ID(i)->getText();
    if (Symbols.findInCurrentScope(paramName)) {
      Errors.declaredIdent(ctx->ID(i));
    }
    else {
      visit(ctx->type(i));
      TypesMgr::TypeId paramTy = getTypeDecor(ctx->type(i));
      Symbols.addParameter(paramName, paramTy);
    }
  }
  DEBUG_EXIT();
  return 0;
}

std::any SymbolsVisitor::visitType(AslParser::TypeContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->basic_type());
  TypesMgr::TypeId basicTy = getTypeDecor(ctx->basic_type());
  if (ctx->ARRAY()) {
    unsigned int size = stoi(ctx->INTVAL()->getText());
    TypesMgr::TypeId arrayType = Types.createArrayTy(size, basicTy);
    putTypeDecor(ctx, arrayType);
  }
  else {
    putTypeDecor(ctx, basicTy);
  }
  DEBUG_EXIT();
  return 0;
}

std::any SymbolsVisitor::visitBasic_type(AslParser::Basic_typeContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId t;
  if (ctx->INT()) t = Types.createIntegerTy();
  else if (ctx->FLOAT()) t = Types.createFloatTy();
  else if (ctx->BOOL()) t = Types.createBooleanTy();
  else if (ctx->CHAR()) t = Types.createCharacterTy();
  else t = Types.createErrorTy();
  putTypeDecor(ctx, t);
  DEBUG_EXIT();
  return 0;
}



// std::any SymbolsVisitor::visitStatements(AslParser::StatementsContext *ctx) {
//   DEBUG_ENTER();
//   std::any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// std::any SymbolsVisitor::visitAssignStmt(AslParser::AssignStmtContext *ctx) {
//   DEBUG_ENTER();
//   std::any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// std::any SymbolsVisitor::visitIfStmt(AslParser::IfStmtContext *ctx) {
//   DEBUG_ENTER();
//   std::any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// std::any SymbolsVisitor::visitProcCall(AslParser::ProcCallContext *ctx) {
//   DEBUG_ENTER();
//   std::any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// std::any SymbolsVisitor::visitReadStmt(AslParser::ReadStmtContext *ctx) {
//   DEBUG_ENTER();
//   std::any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// std::any SymbolsVisitor::visitWriteExpr(AslParser::WriteExprContext *ctx) {
//   DEBUG_ENTER();
//   std::any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// std::any SymbolsVisitor::visitWriteString(AslParser::WriteStringContext *ctx) {
//   DEBUG_ENTER();
//   std::any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// std::any SymbolsVisitor::visitLeft_expr(AslParser::Left_exprContext *ctx) {
//   DEBUG_ENTER();
//   std::any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// std::any SymbolsVisitor::visitExprIdent(AslParser::ExprIdentContext *ctx) {
//   DEBUG_ENTER();
//   std::any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// std::any SymbolsVisitor::visitArithmetic(AslParser::ArithmeticContext *ctx) {
//   DEBUG_ENTER();
//   std::any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// std::any SymbolsVisitor::visitRelational(AslParser::RelationalContext *ctx) {
//   DEBUG_ENTER();
//   std::any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// std::any SymbolsVisitor::visitValue(AslParser::ValueContext *ctx) {
//   DEBUG_ENTER();
//   std::any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// std::any SymbolsVisitor::visitIdent(AslParser::IdentContext *ctx) {
//   DEBUG_ENTER();
//   std::any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }


// Getters for the necessary tree node atributes:
//   Scope and Type
SymTable::ScopeId SymbolsVisitor::getScopeDecor(antlr4::ParserRuleContext *ctx) {
  return Decorations.getScope(ctx);
}
TypesMgr::TypeId SymbolsVisitor::getTypeDecor(antlr4::ParserRuleContext *ctx) {
  return Decorations.getType(ctx);
}

// Setters for the necessary tree node attributes:
//   Scope and Type
void SymbolsVisitor::putScopeDecor(antlr4::ParserRuleContext *ctx, SymTable::ScopeId s) {
  Decorations.putScope(ctx, s);
}
void SymbolsVisitor::putTypeDecor(antlr4::ParserRuleContext *ctx, TypesMgr::TypeId t) {
  Decorations.putType(ctx, t);
}
