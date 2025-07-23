//////////////////////////////////////////////////////////////////////
//
//    TypeCheckVisitor - Walk the parser tree to do the semantic
//                       typecheck for the Asl programming language
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

#include "TypeCheckVisitor.h"
#include "antlr4-runtime.h"

#include "../common/TypesMgr.h"
#include "../common/SymTable.h"
#include "../common/TreeDecoration.h"
#include "../common/SemErrors.h"

#include <iostream>
#include <string>

// uncomment the following line to enable debugging messages with DEBUG*
// #define DEBUG_BUILD
#include "../common/debug.h"

// using namespace std;


// Constructor
TypeCheckVisitor::TypeCheckVisitor(TypesMgr       & Types,
                                   SymTable       & Symbols,
                                   TreeDecoration & Decorations,
                                   SemErrors      & Errors) :
  Types{Types},
  Symbols{Symbols},
  Decorations{Decorations},
  Errors{Errors} {
}

// Accessor/Mutator to the attribute currFunctionType
TypesMgr::TypeId TypeCheckVisitor::getCurrentFunctionTy() const {
  return currFunctionType;
}

void TypeCheckVisitor::setCurrentFunctionTy(TypesMgr::TypeId type) {
  currFunctionType = type;
}

// Methods to visit each kind of node:
//
std::any TypeCheckVisitor::visitProgram(AslParser::ProgramContext *ctx) {
  DEBUG_ENTER();
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  for (auto ctxFunc : ctx->function()) { 
    visit(ctxFunc);
  }
  if (Symbols.noMainProperlyDeclared())
    Errors.noMainProperlyDeclared(ctx);
  Symbols.popScope();
  Errors.print();
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitFunction(AslParser::FunctionContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId ctxTy = getTypeDecor(ctx);
  if (Types.isErrorTy(ctxTy)) {
    ;
  }
  else {
    std::string funcName = ctx->ID()->getText();
    TypesMgr::TypeId funcTy = Symbols.getGlobalFunctionType(funcName);
    setCurrentFunctionTy(funcTy);
    SymTable::ScopeId sc = getScopeDecor(ctx);
    Symbols.pushThisScope(sc);
    visit(ctx->statements());
    Symbols.popScope();
  }
  DEBUG_EXIT();
  return 0;
}

// std::any TypeCheckVisitor::visitDeclarations(AslParser::DeclarationsContext *ctx) {
//   DEBUG_ENTER();
//   std::any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// std::any TypeCheckVisitor::visitVariable_decl(AslParser::Variable_declContext *ctx) {
//   DEBUG_ENTER();
//   std::any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// std::any TypeCheckVisitor::visitType(AslParser::TypeContext *ctx) {
//   DEBUG_ENTER();
//   std::any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

std::any TypeCheckVisitor::visitStatements(AslParser::StatementsContext *ctx) {
  DEBUG_ENTER();
  visitChildren(ctx);
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitAssignStmt(AslParser::AssignStmtContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->left_expr());
  visit(ctx->expr());
  TypesMgr::TypeId left_exprTy = getTypeDecor(ctx->left_expr());
  TypesMgr::TypeId exprTy = getTypeDecor(ctx->expr());
  if ((not Types.isErrorTy(left_exprTy)) and (not Types.isErrorTy(exprTy)) and
      (not Types.copyableTypes(left_exprTy, exprTy)))
    Errors.incompatibleAssignment(ctx->ASSIGN());
  if ((not Types.isErrorTy(left_exprTy)) and (not getIsLValueDecor(ctx->left_expr())))
    Errors.nonReferenceableLeftExpr(ctx->left_expr());
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitIfStmt(AslParser::IfStmtContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId exprTy = getTypeDecor(ctx->expr());
  if ((not Types.isErrorTy(exprTy)) and (not Types.isBooleanTy(exprTy)))
    Errors.booleanRequired(ctx);
  for (auto ctxStatements : ctx->statements())
    visit(ctxStatements);
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitProcCall(AslParser::ProcCallContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->ident());
  visit(ctx->parameters());
  TypesMgr::TypeId identTy = getTypeDecor(ctx->ident());
  if (Types.isErrorTy(identTy)) {
    ;
  } else if (not Types.isFunctionTy(identTy)) {
    Errors.isNotCallable(ctx->ident());
  }
  else {
    /*std::string funcName = ctx->ident()->ID()->getText();*/
    /*TypesMgr::TypeId funcType = Symbols.getGlobalFunctionType(funcName);*/
    TypesMgr::TypeId funcTy = identTy;
    int funcNumParams = Types.getNumOfParameters(funcTy);
    int callNumParams = ctx->parameters()->expr().size();
    int numParamsMin = std::min(funcNumParams, callNumParams);
    if (callNumParams != funcNumParams) {
      Errors.numberOfParameters(ctx->ident());
    }
    if (funcNumParams > 0) {
      std::vector<TypesMgr::TypeId> funcParamsTy = Types.getFuncParamsTypes(funcTy);
      for (int i=0; i<numParamsMin; ++i) {
        TypesMgr::TypeId paramTy = getTypeDecor(ctx->parameters()->expr(i));
        TypesMgr::TypeId funcParamTy = funcParamsTy[i];
        if (Types.isErrorTy(paramTy)) {
          ;
        }
        else if (Types.isIntegerTy(paramTy) and Types.isFloatTy(funcParamTy)) {
          ;
        }
        else if (not Types.equalTypes(paramTy, funcParamTy)) {
          Errors.incompatibleParameter(ctx->parameters()->expr(i), i+1, ctx);
        }
      }
    }
  }
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitReadStmt(AslParser::ReadStmtContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->left_expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->left_expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isPrimitiveTy(t1)) and
      (not Types.isFunctionTy(t1)))
    Errors.readWriteRequireBasic(ctx);
  if ((not Types.isErrorTy(t1)) and (not getIsLValueDecor(ctx->left_expr())))
    Errors.nonReferenceableExpression(ctx);
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitWriteExpr(AslParser::WriteExprContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isPrimitiveTy(t1)))
    Errors.readWriteRequireBasic(ctx);
  DEBUG_EXIT();
  return 0;
}

// std::any TypeCheckVisitor::visitWriteString(AslParser::WriteStringContext *ctx) {
//   DEBUG_ENTER();
//   std::any r = visitChildren(ctx);
//   DEBUG_EXIT();
//   return r;
// }

// std::any TypeCheckVisitor::visitLeft_expr(AslParser::Left_exprContext *ctx) {
//   DEBUG_ENTER();
//   visit(ctx->ident());
//   TypesMgr::TypeId t1 = getTypeDecor(ctx->ident());
//   putTypeDecor(ctx, t1);
//   bool b = getIsLValueDecor(ctx->ident());
//   putIsLValueDecor(ctx, b);
//   DEBUG_EXIT();
//   return 0;
// }

std::any TypeCheckVisitor::visitArithmetic(AslParser::ArithmeticContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr(0));
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  visit(ctx->expr(1));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  if ((((not Types.isErrorTy(t1)) and (not Types.isNumericTy(t1))) or
       ((not Types.isErrorTy(t2)) and (not Types.isNumericTy(t2)))) or
      ((not Types.isErrorTy(t1)) and (not Types.isErrorTy(t2)) and ctx->MOD() and
       ((not Types.isIntegerTy(t1)) or (not Types.isIntegerTy(t2))))) {
    Errors.incompatibleOperator(ctx->op);
  }
  TypesMgr::TypeId t = Types.createErrorTy();
  if (Types.isIntegerTy(t1) and Types.isIntegerTy(t2))
    t = Types.createIntegerTy();
  else if (Types.isFloatTy(t1) or Types.isFloatTy(t2)) /* Almenys un dels dos és Float. L'Integer es transforma a Float. */
    t = Types.createFloatTy();
  else /* Passem al node pare que es correcte, per a que faci els seus errors. */
    t = Types.createIntegerTy();
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, false);
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitRelational(AslParser::RelationalContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr(0));
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  visit(ctx->expr(1));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  std::string oper = ctx->op->getText();
  if ((not Types.isErrorTy(t1)) and (not Types.isErrorTy(t2)) and
      ((not Types.comparableTypes(t1, t2, oper)) or
      ((Types.isBooleanTy(t1) and Types.isBooleanTy(t2)) and (not (ctx->EQ() or ctx->NE())))))
    Errors.incompatibleOperator(ctx->op);
  TypesMgr::TypeId t = Types.createBooleanTy();
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, false);
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitExprValue(AslParser::ExprValueContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->value());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->value());
  putTypeDecor(ctx, t1);
  bool b = getIsLValueDecor(ctx->value());
  putIsLValueDecor(ctx, b);
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitExprIdent(AslParser::ExprIdentContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->ident());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->ident());
  putTypeDecor(ctx, t1);
  bool b = getIsLValueDecor(ctx->ident());
  putIsLValueDecor(ctx, b);
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitIdent(AslParser::IdentContext *ctx) {
  DEBUG_ENTER();
  std::string ident = ctx->getText();
  if (Symbols.findInStack(ident) == -1) {
    Errors.undeclaredIdent(ctx->ID());
    TypesMgr::TypeId te = Types.createErrorTy();
    putTypeDecor(ctx, te);
    putIsLValueDecor(ctx, true);
  }
  else {
    TypesMgr::TypeId t1 = Symbols.getType(ident);
    putTypeDecor(ctx, t1);
    if (Symbols.isFunctionClass(ident))
      putIsLValueDecor(ctx, false);
    else
      putIsLValueDecor(ctx, true);
  }
  DEBUG_EXIT();
  return 0;
}

/* Meu */
std::any TypeCheckVisitor::visitParenthesis(AslParser::ParenthesisContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());
  putTypeDecor(ctx, t1);
  bool b = getIsLValueDecor(ctx->expr());
  putIsLValueDecor(ctx, b);
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitSign(AslParser::SignContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isNumericTy(t1)))
    Errors.incompatibleOperator(ctx->op);
  else {
    putTypeDecor(ctx, t1);
    putIsLValueDecor(ctx, false);
  }
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitNot(AslParser::NotContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isBooleanTy(t1)))
    Errors.incompatibleOperator(ctx->op);
  TypesMgr::TypeId t = Types.createBooleanTy();
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, false);
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitBoolean(AslParser::BooleanContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr(0));
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  visit(ctx->expr(1));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  if (((not Types.isErrorTy(t1)) and (not Types.isBooleanTy(t1))) or
      ((not Types.isErrorTy(t2)) and (not Types.isBooleanTy(t2))))
    Errors.incompatibleOperator(ctx->op);
  TypesMgr::TypeId t = Types.createBooleanTy();
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, false);
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitValue(AslParser::ValueContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId t;
  if (ctx->INTVAL()) t = Types.createIntegerTy();
  else if (ctx->FLOATVAL()) t = Types.createFloatTy();
  else if (ctx->BOOLVAL()) t = Types.createBooleanTy();
  else if (ctx->CHARVAL()) t = Types.createCharacterTy();
  putTypeDecor(ctx, t);
  putIsLValueDecor(ctx, false);
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitWhileStmt(AslParser::WhileStmtContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->expr());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());
  if ((not Types.isErrorTy(t1)) and (not Types.isBooleanTy(t1))) {
    Errors.booleanRequired(ctx);
    TypesMgr::TypeId te = Types.createErrorTy();
    putTypeDecor(ctx, te);
  }
  visit(ctx->statements());
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitReturnStmt(AslParser::ReturnStmtContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId tRet = Types.createVoidTy();
  if (ctx->expr()) {
    visit(ctx->expr());
    tRet = getTypeDecor(ctx->expr());
  }
  if (not Types.isErrorTy(tRet)) {
    TypesMgr::TypeId funcType = getCurrentFunctionTy();
    TypesMgr::TypeId funcRetType = Types.getFuncReturnType(funcType);
    if ((not (Types.isIntegerTy(tRet) and Types.isFloatTy(funcRetType))) and
        (tRet != funcRetType)) {
      Errors.incompatibleReturn(ctx->RETURN());
      tRet = Types.createErrorTy();
    }
  }
  putTypeDecor(ctx, tRet);
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitParameters(AslParser::ParametersContext *ctx) {
  DEBUG_ENTER();
  visitChildren(ctx);
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitLeft_exprIdent(AslParser::Left_exprIdentContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->ident());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->ident());
  putTypeDecor(ctx, t1);
  bool b = getIsLValueDecor(ctx->ident());
  putIsLValueDecor(ctx, b);
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitLeft_exprArray(AslParser::Left_exprArrayContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->ident());
  visit(ctx->expr());
  TypesMgr::TypeId arrayType = getTypeDecor(ctx->ident());
  bool b = getIsLValueDecor(ctx->ident());
  putIsLValueDecor(ctx, b);
  
  if (Types.isErrorTy(arrayType)) {
    ;
  }
  else {
    bool b = getIsLValueDecor(ctx->ident());
    putIsLValueDecor(ctx, b);
    if (ctx->expr()) {
      if (not Types.isArrayTy(arrayType)) {
        Errors.nonArrayInArrayAccess(ctx);
      }
      else {
        TypesMgr::TypeId arrayElemType = Types.getArrayElemType(arrayType);
        putTypeDecor(ctx, arrayElemType);
      }
      TypesMgr::TypeId exprType = getTypeDecor(ctx->expr());
      if (not Types.isIntegerTy(exprType)) {
        Errors.nonIntegerIndexInArrayAccess(ctx->expr());
      }
    }
  }
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitExprArray(AslParser::ExprArrayContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->ident());
  visit(ctx->expr());
  TypesMgr::TypeId arrayType = getTypeDecor(ctx->ident());
  if (Types.isErrorTy(arrayType)) {
    ;
  }
  else {
    bool b = getIsLValueDecor(ctx->ident());
    putIsLValueDecor(ctx, b);
    if (not Types.isArrayTy(arrayType)) {
      Errors.nonArrayInArrayAccess(ctx);
    }
    else {
      TypesMgr::TypeId arrayElemType = Types.getArrayElemType(arrayType);
      putTypeDecor(ctx, arrayElemType);
    }
    TypesMgr::TypeId exprType = getTypeDecor(ctx->expr());
    if (not Types.isIntegerTy(exprType)) {
      Errors.nonIntegerIndexInArrayAccess(ctx->expr());
    }
  }
  DEBUG_EXIT();
  return 0;
}

std::any TypeCheckVisitor::visitExprFunc(AslParser::ExprFuncContext *ctx) {
  DEBUG_ENTER();
  visit(ctx->ident());
  visit(ctx->parameters());
  TypesMgr::TypeId t1 = getTypeDecor(ctx->ident());
  if (Types.isErrorTy(t1)) {
    ;
  }
  else if (not Types.isFunctionTy(t1)) {
    Errors.isNotCallable(ctx->ident());
  }
  else {
    std::string funcName = ctx->ident()->ID()->getText();
    TypesMgr::TypeId funcType = Symbols.getGlobalFunctionType(funcName);
    int numParamsFunc = Types.getNumOfParameters(funcType);
    int numParamsPassed = ctx->parameters()->expr().size();
    int numParamsMin = std::min(numParamsFunc, numParamsPassed);
    if (numParamsPassed != numParamsFunc) {
      Errors.numberOfParameters(ctx->ident());
    }
    if (numParamsFunc > 0) {
      std::vector<TypesMgr::TypeId> funcParamsType = Types.getFuncParamsTypes(funcType);
      for (int i=0; i<numParamsMin; ++i) {
        TypesMgr::TypeId paramType = getTypeDecor(ctx->parameters()->expr(i));
        if (Types.isErrorTy(paramType)) {
          ;
        }
        else if (Types.isIntegerTy(paramType) and Types.isFloatTy(funcParamsType[i])) {
          ;
        }
        else if (not Types.equalTypes(paramType, funcParamsType[i])) {
          Errors.incompatibleParameter(ctx->parameters()->expr(i), i+1, ctx);
        }
      }
    }
    TypesMgr::TypeId funcRetType = Types.getFuncReturnType(funcType);
    if (Types.isVoidTy(funcRetType)) {
      Errors.isNotFunction(ctx->ident());
    }
    else
      putTypeDecor(ctx, funcRetType);
  }
  DEBUG_EXIT();
  return 0;
}





/*  Comentaris per debugging
 *  std::cerr << "visitExprFunc" << " | Type " << ctx->parameters()->expr(i)->getText() << " : ( ERROR | INTEGER | FLOAT | BOOLEAN | CHARACTER | VOID | FUNCTION | ARRAY)" << " ->" << " ( " << Types.isErrorTy(paramType) << " | " << Types.isIntegerTy(paramType) << " | " << Types.isFloatTy(paramType) << " | " << Types.isBooleanTy(paramType) << " | " << Types.isCharacterTy(paramType) << " | " << Types.isVoidTy(paramType) << " | " << Types.isFunctionTy(paramType) << " | " << Types.isArrayTy(paramType) << " ) " << std::endl;
 *  std::cerr << "visitExprFunc" << " | numParamsFunc " << numParamsFunc << std::endl;
 */

// Getters for the necessary tree node atributes:
//   Scope, Type ans IsLValue
SymTable::ScopeId TypeCheckVisitor::getScopeDecor(antlr4::ParserRuleContext *ctx) {
  return Decorations.getScope(ctx);
}
TypesMgr::TypeId TypeCheckVisitor::getTypeDecor(antlr4::ParserRuleContext *ctx) {
  return Decorations.getType(ctx);
}
bool TypeCheckVisitor::getIsLValueDecor(antlr4::ParserRuleContext *ctx) {
  return Decorations.getIsLValue(ctx);
}

// Setters for the necessary tree node attributes:
//   Scope, Type ans IsLValue
void TypeCheckVisitor::putScopeDecor(antlr4::ParserRuleContext *ctx, SymTable::ScopeId s) {
  Decorations.putScope(ctx, s);
}
void TypeCheckVisitor::putTypeDecor(antlr4::ParserRuleContext *ctx, TypesMgr::TypeId t) {
  Decorations.putType(ctx, t);
}
void TypeCheckVisitor::putIsLValueDecor(antlr4::ParserRuleContext *ctx, bool b) {
  Decorations.putIsLValue(ctx, b);
}
