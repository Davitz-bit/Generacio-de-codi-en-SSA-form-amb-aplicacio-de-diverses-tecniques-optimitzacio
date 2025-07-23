//////////////////////////////////////////////////////////////////////
//
//    CodeGenVisitor - Walk the parser tree to do
//                     the generation of code
//
//    Copyright (C) 2020-2030  Universitat Politecnica de Catalunya
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU General Public License
//    as published by the Free Software Foundation; either version 3
//    of the License,or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Affero General Public License for more details.
//
//    You should have received a copy of the GNU Affero General Public
//    License along with this library; if not,write to the Free Software
//    Foundation,Inc.,59 Temple Place,Suite 330,Boston,MA 02111-1307 USA
//
//    contact: José Miguel Rivero (rivero@cs.upc.edu)
//             Computer Science Department
//             Universitat Politecnica de Catalunya
//             despatx Omega.110 - Campus Nord UPC
//             08034 Barcelona.  SPAIN
//
//////////////////////////////////////////////////////////////////////

#include "CodeGenVisitor.h"
#include "antlr4-runtime.h"

#include "../common/TypesMgr.h"
#include "../common/SymTable.h"
#include "../common/TreeDecoration.h"
#include "../common/code.h"

#include <string>
#include <cstddef>    // std::size_t

// uncomment the following line to enable debugging messages with DEBUG*
// #define DEBUG_BUILD
#include "../common/debug.h"

// using namespace std;


// Constructor
CodeGenVisitor::CodeGenVisitor(TypesMgr       & Types,
                               SymTable       & Symbols,
                               TreeDecoration & Decorations) :
  Types{Types},
  Symbols{Symbols},
  Decorations{Decorations} {
}

// Accessor/Mutator to the attribute currFunctionType
TypesMgr::TypeId CodeGenVisitor::getCurrentFunctionTy() const {
  return currFunctionType;
}

void CodeGenVisitor::setCurrentFunctionTy(TypesMgr::TypeId type) {
  currFunctionType = type;
}

// Methods to visit each kind of node:
//
std::any CodeGenVisitor::visitProgram(AslParser::ProgramContext *ctx) {
  DEBUG_ENTER();
  code my_code;
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  for (auto ctxFunc : ctx->function()) {
    subroutine subr = std::any_cast<subroutine>(visit(ctxFunc));
    my_code.add_subroutine(subr);
  }
  Symbols.popScope();
  DEBUG_EXIT();
  return my_code;
}

std::any CodeGenVisitor::visitFunction(AslParser::FunctionContext *ctx) {
  DEBUG_ENTER();
  SymTable::ScopeId sc = getScopeDecor(ctx);
  Symbols.pushThisScope(sc);
  subroutine subr(ctx->ID()->getText());
  codeCounters.reset();

  // std::vector<var> && lvars = std::any_cast<std::vector<var>>(visit(ctx->declarations(1)));
  // for (auto & onevar : lvars) {
  //   subr.add_var(onevar);
  // }
  // std::vector<var> lvars;
  for (auto & varDeclCtx : ctx->variable_decl()) {
    std::vector<var> vars = std::any_cast<std::vector<var>>(visit(varDeclCtx));
    for (var onevar : vars)
      // lvars.push_back(onevar);
      subr.add_var(onevar);
  }

  TypesMgr::TypeId funcTy = getTypeDecor(ctx);
  TypesMgr::TypeId funcRetTy = Types.getFuncReturnType(funcTy);
  if (not Types.isVoidTy(funcRetTy)) {
    std::string paramNameFuncRet = "_result";
    std::string paramTypeFuncRet = Types.to_string(funcRetTy);
    bool paramIsArrayFuncRet = Types.isArrayTy(funcRetTy);
    subr.add_param(paramNameFuncRet,paramTypeFuncRet,paramIsArrayFuncRet);
  }

  if (ctx->parameter_decl()) {
    // std::vector<std::pair<std::string,TypesMgr::TypeId>> && params = std::any_cast<std::vector<std::pair<std::string,TypesMgr::TypeId>>>(visit(ctx->declarations(0)));
    std::vector<std::pair<std::string,TypesMgr::TypeId>> && params = std::any_cast<std::vector<std::pair<std::string,TypesMgr::TypeId>>>(visit(ctx->parameter_decl()));
    for (auto & oneparam : params) {
      std::string paramName = oneparam.first;
      std::string paramTy;
      bool paramIsArray = Types.isArrayTy(oneparam.second);

      if (paramIsArray) {
        paramTy = Types.to_string(Types.getArrayElemType(oneparam.second));
      }
      else {
        paramTy = Types.to_string(oneparam.second);
      }

      subr.add_param(paramName,paramTy,paramIsArray);
    }
  }
  
  setCurrentFunctionTy(funcTy);
  instructionList && code = std::any_cast<instructionList>(visit(ctx->statements()));
  code = code || instruction(instruction::RETURN());
  subr.set_instructions(code);

  Symbols.popScope();
  DEBUG_EXIT();
  return subr;
}

// std::any CodeGenVisitor::visitDeclarations(AslParser::DeclarationsContext *ctx) {
//   DEBUG_ENTER();
//   DEBUG_EXIT();
//   return 0;
// }

std::any CodeGenVisitor::visitVariable_decl(AslParser::Variable_declContext *ctx) {
  DEBUG_ENTER();
  TypesMgr::TypeId   t1 = getTypeDecor(ctx->type());
  std::size_t      size = Types.getSizeOfType(t1);
  std::vector<var> vars;
  for (auto & idCtx : ctx->ID()) {
    if (Types.isArrayTy(t1)) {
      vars.push_back(var{idCtx->getText(),Types.to_string(Types.getArrayElemType(t1)),size});
    }
    else {
      vars.push_back(var{idCtx->getText(),Types.to_string(t1),size});
    }
  }
  DEBUG_EXIT();
  return vars;
}

std::any CodeGenVisitor::visitStatements(AslParser::StatementsContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  for (auto stCtx : ctx->statement()) {
    instructionList && codeS = std::any_cast<instructionList>(visit(stCtx));
    code = code || codeS;
  }
  DEBUG_EXIT();
  return code;
}

std::any CodeGenVisitor::visitAssignStmt(AslParser::AssignStmtContext *ctx) {
  DEBUG_ENTER();

  CodeAttribs     && codAtsE1 = std::any_cast<CodeAttribs>(visit(ctx->left_expr()));
  std::string           addr1 = codAtsE1.addr;
  std::string           offs1 = codAtsE1.offs;
  instructionList &     code1 = codAtsE1.code;
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->left_expr());

  CodeAttribs     && codAtsE2 = std::any_cast<CodeAttribs>(visit(ctx->expr()));
  std::string           addr2 = codAtsE2.addr;
  // std::string           offs2 = codAtsE2.offs;
  instructionList &     code2 = codAtsE2.code;
  TypesMgr::TypeId tid2 = getTypeDecor(ctx->expr());

  instructionList code = code1 || code2;

  std::string tempAddr2;
  if (Types.isFloatTy(tid1) and Types.isIntegerTy(tid2)) {
    tempAddr2 = "%"+codeCounters.newTEMP();
    code = code || instruction::FLOAT(tempAddr2,addr2);
  }
  else {
    tempAddr2 = addr2;
  }
  
  if (offs1 != "") {
    code = code || instruction::XLOAD(addr1,offs1,tempAddr2);
  }
  else if (not Types.isArrayTy(tid1)) {
    code = code || instruction::ILOAD(addr1,tempAddr2);
  }
  else { // És un array sense offset (Per tant l'adreça)
    TypesMgr::TypeId funcTy = getCurrentFunctionTy();
    std::vector<TypesMgr::TypeId> funcParamTypes = Types.getFuncParamsTypes(funcTy);
    bool addr1IsParamArray = false;
    bool addr2IsParamArray = false;
    for (auto & paramTy : funcParamTypes) {
      if (tid1 == paramTy) // and Types.isArrayTy(tid1))
        addr1IsParamArray = true;
      if (tid2 == paramTy) // and Types.isArrayTy(tid2))
        addr2IsParamArray = true;
    }

    std::string temp1;
    // Si és array (l'altre també ho serà pel type check) i ademés paràmetre
    if (addr1IsParamArray) { // and offs1 == "") {
      temp1 = "%"+codeCounters.newTEMP();
      code = code || instruction::ILOAD(temp1,addr1);
    }
    else {
      temp1 = addr1;
    }

    std::string temp2;
    if (addr2IsParamArray) {
      temp2 = "%"+codeCounters.newTEMP();
      code = code || instruction::ILOAD(temp2,addr2);
    }
    else {
      temp2 = addr2;
    }

    unsigned int addr1ArraySize = Types.getArraySize(tid1);
    std::string temp3 = "%"+codeCounters.newTEMP();
    std::string tempi = "%"+codeCounters.newTEMP();
    for (unsigned int i=0; i<addr1ArraySize; ++i) {
      code = code || 
      instruction::ILOAD(tempi,std::to_string(i)) || 
      instruction::LOADX(temp3,temp2,tempi) || 
      instruction::XLOAD(temp1,tempi,temp3);
    }
  }

  DEBUG_EXIT();
  return code;
}

std::any CodeGenVisitor::visitIfStmt(AslParser::IfStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs     && codAtsE = std::any_cast<CodeAttribs>(visit(ctx->expr()));
  std::string          addr1 = codAtsE.addr;
  instructionList &    code1 = codAtsE.code;
  instructionList &&   code2 = std::any_cast<instructionList>(visit(ctx->statements(0)));

  instructionList code3;
  if (ctx->ELSE())
    code3 = std::any_cast<instructionList>(visit(ctx->statements(1)));

  std::string label = codeCounters.newLabelIF();
  if (ctx->ELSE()) {
    std::string labelElse = "else"+label;
    std::string labelEndIf = "endif"+label;
    code =  code1 ||
            instruction::FJUMP(addr1,labelElse) ||
            code2 ||
            instruction::UJUMP(labelEndIf) ||
            instruction::LABEL(labelElse) ||
            code3 ||
            instruction::LABEL(labelEndIf);
  }
  else {
    std::string labelEndIf = "endif"+label;
    code =  code1 ||
            instruction::FJUMP(addr1,labelEndIf) ||
            code2 ||
            instruction::LABEL(labelEndIf);
  }

  DEBUG_EXIT();
  return code;
}

std::any CodeGenVisitor::visitProcCall(AslParser::ProcCallContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = std::any_cast<CodeAttribs>(visit(ctx->ident()));
  std::string         addr1 = codAt1.addr;
  std::vector<std::pair<CodeAttribs,TypesMgr::TypeId>> && params = std::any_cast<std::vector<std::pair<CodeAttribs,TypesMgr::TypeId>>>(visit(ctx->parameters()));

  instructionList code;

  // ProcCall mai guarda el resultat retornat (si el retorna)

  TypesMgr::TypeId funcCallTy = Symbols.getGlobalFunctionType(addr1);
  TypesMgr::TypeId funcCallRetTy = Types.getFuncReturnType(funcCallTy);
  if (not Types.isVoidTy(funcCallRetTy)) {
    code = code || instruction::PUSH(); // On retornar el resultat. En aquest cas el resultat no es guarda.
  }

  std::vector<TypesMgr::TypeId> funcCallParamTypes = Types.getFuncParamsTypes(funcCallTy);
  for (long unsigned int i=0; i<params.size(); ++i) {
    CodeAttribs codAt2 = params[i].first;
    std::string         addr2 = codAt2.addr;
    // std::string         offs2 = codAt2.offs;
    instructionList &   code2 = codAt2.code;
    code = code || code2;
    TypesMgr::TypeId tid2 = params[i].second;

    TypesMgr::TypeId paramTy = funcCallParamTypes[i];
    if (Types.isFloatTy(paramTy) and Types.isIntegerTy(tid2)) {
      std::string tempAddr2 = "%"+codeCounters.newTEMP();
      code = code || instruction::FLOAT(tempAddr2,addr2);
      code = code || instruction::PUSH(tempAddr2);
    }
    else if (Types.isArrayTy(paramTy)) {
      TypesMgr::TypeId funcCurrentTy = getCurrentFunctionTy();
      std::vector<TypesMgr::TypeId> funcCurrentParamTypes = Types.getFuncParamsTypes(funcCurrentTy);
      bool addr2IsParamArray = false;
      for (auto & paramTy2 : funcCurrentParamTypes) {
        if (tid2 == paramTy2) // and Types.isArrayTy(tid1))
          addr2IsParamArray = true;
      }

      std::string temp2 = "%"+codeCounters.newTEMP();;
      if (addr2IsParamArray) {
        code = code || instruction::ILOAD(temp2,addr2);
      }
      else {
        code = code || instruction::ALOAD(temp2,addr2);
      }
      code = code || instruction::PUSH(temp2);
    }
    else {
      code = code || instruction::PUSH(addr2);
    }
  }

  code = code || instruction::CALL(addr1);

  for (long unsigned int i=0; i<params.size(); ++i) {
    code = code || instruction::POP();
  }

  if (not Types.isVoidTy(funcCallRetTy)) {
    code = code || instruction::POP(); // On retornar el resultat. En aquest cas el resultat no es guarda.
  }

  // ProcCall mai guarda el resultat retornat (si el retorna)

  DEBUG_EXIT();
  return code;
}

std::any CodeGenVisitor::visitReadStmt(AslParser::ReadStmtContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAtsE = std::any_cast<CodeAttribs>(visit(ctx->left_expr()));
  std::string          addr1 = codAtsE.addr;
  std::string          offs1 = codAtsE.offs;
  instructionList &    code1 = codAtsE.code;
  instructionList &     code = code1;
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->left_expr());

  if (offs1 != "") {
    std::string temp1 = "%"+codeCounters.newTEMP();
    if (Types.isIntegerTy(tid1) or Types.isBooleanTy(tid1))
      code = code || instruction::READI(temp1);
    else if (Types.isFloatTy(tid1))
      code = code || instruction::READF(temp1);
    else if (Types.isCharacterTy(tid1)) {
      code = code || instruction::READC(temp1);
    }
    code = code || instruction::XLOAD(addr1,offs1,temp1);
  }
  else {
    if (Types.isIntegerTy(tid1) or Types.isBooleanTy(tid1))
      code = code || instruction::READI(addr1);
    else if (Types.isFloatTy(tid1))
      code = code || instruction::READF(addr1);
    else if (Types.isCharacterTy(tid1)) {
      code = code || instruction::READC(addr1);
    }
  }

  DEBUG_EXIT();
  return code;
}

std::any CodeGenVisitor::visitWriteExpr(AslParser::WriteExprContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = std::any_cast<CodeAttribs>(visit(ctx->expr()));
  std::string         addr1 = codAt1.addr;
  // std::string         offs1 = codAt1.offs;
  instructionList &   code1 = codAt1.code;
  instructionList &    code = code1;
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->expr());

  if (Types.isIntegerTy(tid1) or Types.isBooleanTy(tid1))
    code = code || instruction::WRITEI(addr1);
  else if (Types.isFloatTy(tid1))
    code = code || instruction::WRITEF(addr1);
  else if (Types.isCharacterTy(tid1)) {
    code = code || instruction::WRITEC(addr1);
  }

  DEBUG_EXIT();
  return code;
}

std::any CodeGenVisitor::visitWriteString(AslParser::WriteStringContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string s = ctx->STRING()->getText();
  code = code || instruction::WRITES(s);
  DEBUG_EXIT();
  return code;
}

/*
std::any CodeGenVisitor::visitLeft_expr(AslParser::Left_exprContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = std::any_cast<CodeAttribs>(visit(ctx->ident()));
  DEBUG_EXIT();
  return codAts;
}
*/

std::any CodeGenVisitor::visitArithmetic(AslParser::ArithmeticContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = std::any_cast<CodeAttribs>(visit(ctx->expr(0)));
  std::string         addr1 = codAt1.addr;
  instructionList &   code1 = codAt1.code;
  CodeAttribs     && codAt2 = std::any_cast<CodeAttribs>(visit(ctx->expr(1)));
  std::string         addr2 = codAt2.addr;
  instructionList &   code2 = codAt2.code;
  instructionList &&   code = code1 || code2;
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  TypesMgr::TypeId  t = getTypeDecor(ctx);

  std::string tempAddr1,tempAddr2;
  if (Types.isFloatTy(t1) and Types.isIntegerTy(t2)) {
    tempAddr1 = addr1;
    tempAddr2 = "%"+codeCounters.newTEMP();
    code = code || instruction::FLOAT(tempAddr2,addr2);
  }
  else if (Types.isFloatTy(t2) and Types.isIntegerTy(t1)) {
    tempAddr1 = "%"+codeCounters.newTEMP();
    tempAddr2 = addr2;
    code = code || instruction::FLOAT(tempAddr1,addr1);
  }
  else {
    tempAddr1 = addr1;
    tempAddr2 = addr2;
  }

  std::string temp = "%"+codeCounters.newTEMP();
  if (Types.isIntegerTy(t)) {
    if (ctx->MUL()) {
      code = code || instruction::MUL(temp,tempAddr1,tempAddr2);
    }
    else if(ctx->DIV()) {
      code = code || instruction::DIV(temp,tempAddr1,tempAddr2);
    }
    else if(ctx->MOD()) {
      std::string temp2 = "%"+codeCounters.newTEMP();
      instructionList mod = instruction::DIV(temp,tempAddr1,tempAddr2) || 
                            instruction::MUL(temp2,tempAddr2,temp) || 
                            instruction::SUB(temp,tempAddr1,temp2);
      code = code || mod;
    }
    else if(ctx->PLUS()) {
      code = code || instruction::ADD(temp,tempAddr1,tempAddr2);
    }
    else if(ctx->MINUS()) {
      code = code || instruction::SUB(temp,tempAddr1,tempAddr2);
    }
  }
  else if (Types.isFloatTy(t)) {
    if (ctx->MUL()) {
      code = code || instruction::FMUL(temp,tempAddr1,tempAddr2);
    }
    else if(ctx->DIV()) {
      code = code || instruction::FDIV(temp,tempAddr1,tempAddr2);
    }
    else if(ctx->PLUS()) {
      code = code || instruction::FADD(temp,tempAddr1,tempAddr2);
    }
    else if(ctx->MINUS()) {
      code = code || instruction::FSUB(temp,tempAddr1,tempAddr2);
    }
  }
  CodeAttribs codAts(temp,"",code);

  DEBUG_EXIT();
  return codAts;
}

std::any CodeGenVisitor::visitRelational(AslParser::RelationalContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = std::any_cast<CodeAttribs>(visit(ctx->expr(0)));
  std::string         addr1 = codAt1.addr;
  instructionList &   code1 = codAt1.code;
  CodeAttribs     && codAt2 = std::any_cast<CodeAttribs>(visit(ctx->expr(1)));
  std::string         addr2 = codAt2.addr;
  instructionList &   code2 = codAt2.code;
  instructionList &&   code = code1 || code2;
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(0));
  TypesMgr::TypeId t2 = getTypeDecor(ctx->expr(1));
  // TypesMgr::TypeId  t = getTypeDecor(ctx);

  std::string tempAddr1,tempAddr2;
  if (Types.isFloatTy(t1) and Types.isIntegerTy(t2)) {
    tempAddr1 = addr1;
    tempAddr2 = "%"+codeCounters.newTEMP();
    code = code || instruction::FLOAT(tempAddr2,addr2);
  }
  else if (Types.isFloatTy(t2) and Types.isIntegerTy(t1)) {
    tempAddr1 = "%"+codeCounters.newTEMP();
    tempAddr2 = addr2;
    code = code || instruction::FLOAT(tempAddr1,addr1);
  }
  else {
    tempAddr1 = addr1;
    tempAddr2 = addr2;
  }

  std::string temp = "%"+codeCounters.newTEMP();
  if (not (Types.isFloatTy(t1) or Types.isFloatTy(t2))) {
    if(ctx->EQ()) {
      code = code || instruction::EQ(temp,tempAddr1,tempAddr2);
    }
    else if(ctx->NE()) {
      std::string temp2 = "%"+codeCounters.newTEMP();
      instructionList eq = instruction::EQ(temp2,tempAddr1,tempAddr2);
      code = code || eq || instruction::NOT(temp,temp2);
    }
    else if(ctx->LT()) {
      code = code || instruction::LT(temp,tempAddr1,tempAddr2);
    }
    else if(ctx->GT()) {
      std::string temp2 = "%"+codeCounters.newTEMP();
      instructionList le = instruction::LE(temp2,tempAddr1,tempAddr2);
      code = code || le || instruction::NOT(temp,temp2);
    }
    else if(ctx->LE()) {
      code = code || instruction::LE(temp,tempAddr1,tempAddr2);
    }
    else if(ctx->GE()) {
      std::string temp2 = "%"+codeCounters.newTEMP();
      instructionList lt = instruction::LT(temp2,tempAddr1,tempAddr2);
      code = code || lt || instruction::NOT(temp,temp2);
    }
  }
  else {
    if(ctx->EQ()) {
      code = code || instruction::FEQ(temp,tempAddr1,tempAddr2);
    }
    else if(ctx->NE()) {
      std::string temp2 = "%"+codeCounters.newTEMP();
      instructionList eq = instruction::FEQ(temp2,tempAddr1,tempAddr2);
      code = code || eq || instruction::NOT(temp,temp2);
    }
    else if(ctx->LT()) {
      code = code || instruction::FLT(temp,tempAddr1,tempAddr2);
    }
    else if(ctx->GT()) {
      std::string temp2 = "%"+codeCounters.newTEMP();
      instructionList le = instruction::FLE(temp2,tempAddr1,tempAddr2);
      code = code || le || instruction::NOT(temp,temp2);
    }
    else if(ctx->LE()) {
      code = code || instruction::FLE(temp,tempAddr1,tempAddr2);
    }
    else if(ctx->GE()) {
      std::string temp2 = "%"+codeCounters.newTEMP();
      instructionList lt = instruction::FLT(temp2,tempAddr1,tempAddr2);
      code = code || lt || instruction::NOT(temp,temp2);
    }
  }
  CodeAttribs codAts(temp,"",code);
  DEBUG_EXIT();
  return codAts;
}

std::any CodeGenVisitor::visitValue(AslParser::ValueContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  std::string temp = "%"+codeCounters.newTEMP();

  if (ctx->INTVAL() or ctx->FLOATVAL())
    code = instruction::ILOAD(temp,ctx->getText());
  else if (ctx->BOOLVAL()) {
    if (ctx->getText() == "false")
      code = instruction::ILOAD(temp,"0");
    else // if (ctx->getText() == "true")
      code = instruction::ILOAD(temp,"1");
  }
  else if (ctx->CHARVAL()) {
    // code = instruction::CHLOAD(temp,ctx->getText()); // Posa ' ' extres.
    code = instruction::ILOAD(temp,ctx->getText());
  }

  CodeAttribs codAts(temp,"",code);
  DEBUG_EXIT();
  return codAts;
}

std::any CodeGenVisitor::visitExprIdent(AslParser::ExprIdentContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = std::any_cast<CodeAttribs>(visit(ctx->ident()));
  DEBUG_EXIT();
  return codAts;
}

std::any CodeGenVisitor::visitIdent(AslParser::IdentContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs codAts(ctx->ID()->getText(),"",instructionList());
  DEBUG_EXIT();
  return codAts;
}

/* Meu */
std::any CodeGenVisitor::visitParameter_decl(AslParser::Parameter_declContext *ctx) {
  DEBUG_ENTER();
  std::vector<std::pair<std::string,TypesMgr::TypeId>> params;
  for (unsigned int i=0; i<ctx->ID().size(); ++i) {
    std::string      name = ctx->ID(i)->getText();
    TypesMgr::TypeId   t1 = getTypeDecor(ctx->type(i));
    params.push_back(std::pair{name,t1});
  }
  DEBUG_EXIT();
  return params;
}

std::any CodeGenVisitor::visitParameters(AslParser::ParametersContext *ctx) {
  DEBUG_ENTER();
  std::vector<std::pair<CodeAttribs,TypesMgr::TypeId>> params;
  for (unsigned int i=0; i<ctx->expr().size(); ++i) {
    CodeAttribs     && codAts1 = std::any_cast<CodeAttribs>(visit(ctx->expr(i)));
    TypesMgr::TypeId t1 = getTypeDecor(ctx->expr(i));
    params.push_back(std::pair{codAts1,t1});
  }
  DEBUG_EXIT();
  return params;
}

std::any CodeGenVisitor::visitWhileStmt(AslParser::WhileStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;
  CodeAttribs     && codAtsE = std::any_cast<CodeAttribs>(visit(ctx->expr()));
  std::string          addr1 = codAtsE.addr;
  instructionList &    code1 = codAtsE.code;
  instructionList &&   code2 = std::any_cast<instructionList>(visit(ctx->statements()));

  std::string label = codeCounters.newLabelWHILE();
  std::string labelStartWhile = "startWhile"+label;
  std::string labelEndWhile = "endwhile"+label;
  code =  instruction::LABEL(labelStartWhile) ||
          code1 ||
          instruction::FJUMP(addr1,labelEndWhile) ||
          code2 ||
          instruction::UJUMP(labelStartWhile) ||
          instruction::LABEL(labelEndWhile);

  DEBUG_EXIT();
  return code;
}

std::any CodeGenVisitor::visitReturnStmt(AslParser::ReturnStmtContext *ctx) {
  DEBUG_ENTER();
  instructionList code;

  if (ctx->expr()) { // ExprFunc
    CodeAttribs   && codAt1 = std::any_cast<CodeAttribs>(visit(ctx->expr()));
    std::string       addr1 = codAt1.addr;
    // std::string       offs1 = codAt1.offs;
    instructionList& code1 = codAt1.code;
    TypesMgr::TypeId tid1 = getTypeDecor(ctx->expr());

    code = code1;

    TypesMgr::TypeId funcTy = getCurrentFunctionTy();
    TypesMgr::TypeId funcRetTy = Types.getFuncReturnType(funcTy);
    std::string paramNameFuncRet = "_result";

    if (Types.isFloatTy(funcRetTy) and Types.isIntegerTy(tid1)) {
      std::string tempAddr1 = "%"+codeCounters.newTEMP();
      code = code || instruction::FLOAT(tempAddr1,addr1);
      code = code || instruction::ILOAD(paramNameFuncRet,tempAddr1);
    }
    else {
      code = code || instruction::ILOAD(paramNameFuncRet,addr1);
    }
  }
  code = code || instruction(instruction::RETURN());

  // else {} // ProcCall
  
  DEBUG_EXIT();
  return code;
}

std::any CodeGenVisitor::visitLeft_exprIdent(AslParser::Left_exprIdentContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = std::any_cast<CodeAttribs>(visit(ctx->ident()));
  DEBUG_EXIT();
  return codAts;
}

std::any CodeGenVisitor::visitLeft_exprArray(AslParser::Left_exprArrayContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAtsE1 = std::any_cast<CodeAttribs>(visit(ctx->ident()));
  std::string           addr1 = codAtsE1.addr;
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->ident());

  CodeAttribs     && codAtsE2 = std::any_cast<CodeAttribs>(visit(ctx->expr()));
  std::string           addr2 = codAtsE2.addr;
  instructionList &     code2 = codAtsE2.code;

  instructionList code = code2;

  TypesMgr::TypeId funcTy = getCurrentFunctionTy();
  std::vector<TypesMgr::TypeId> funcParamTypes = Types.getFuncParamsTypes(funcTy);
  bool addr1IsParamArray = false;
  for (auto & paramTy : funcParamTypes) {
    if (tid1 == paramTy) // and Types.isArrayTy(tid1))
      addr1IsParamArray = true;
  }

  std::string temp1;
  if (addr1IsParamArray) {
    temp1 = "%"+codeCounters.newTEMP();
    code = code || instruction::ILOAD(temp1,addr1);
  }
  else {
    temp1 = addr1;
  }

  CodeAttribs codAts(temp1,addr2,code);
  DEBUG_EXIT();
  return codAts;
}

std::any CodeGenVisitor::visitParenthesis(AslParser::ParenthesisContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = std::any_cast<CodeAttribs>(visit(ctx->expr()));
  DEBUG_EXIT();
  return codAts;
}

std::any CodeGenVisitor::visitSign(AslParser::SignContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = std::any_cast<CodeAttribs>(visit(ctx->expr()));
  std::string         addr1 = codAt1.addr;
  instructionList &   code1 = codAt1.code;
  instructionList &    code = code1;
  TypesMgr::TypeId t1 = getTypeDecor(ctx->expr());

  std::string temp;
  if (Types.isIntegerTy(t1)) {
    if(ctx->PLUS()) {
      temp = addr1;
    }
    else if (ctx->MINUS()) {
      temp = "%"+codeCounters.newTEMP();
      code = code || instruction::NEG(temp,addr1);
    }
  }
  else if (Types.isFloatTy(t1)) {
    if(ctx->PLUS()) {
      temp = addr1;
    }
    else if (ctx->MINUS()) {
      temp = "%"+codeCounters.newTEMP();
      code = code || instruction::FNEG(temp,addr1);
    }
  }
  CodeAttribs codAts(temp,"",code);

  DEBUG_EXIT();
  return codAts;
}

std::any CodeGenVisitor::visitNot(AslParser::NotContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = std::any_cast<CodeAttribs>(visit(ctx->expr()));
  std::string         addr1 = codAt1.addr;
  instructionList &   code1 = codAt1.code;
  instructionList &    code = code1;

  std::string temp = "%"+codeCounters.newTEMP();
  code = code || instruction::NOT(temp,addr1);

  CodeAttribs codAts(temp,"",code);

  DEBUG_EXIT();
  return codAts;
}

std::any CodeGenVisitor::visitBoolean(AslParser::BooleanContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = std::any_cast<CodeAttribs>(visit(ctx->expr(0)));
  std::string         addr1 = codAt1.addr;
  instructionList &   code1 = codAt1.code;
  CodeAttribs     && codAt2 = std::any_cast<CodeAttribs>(visit(ctx->expr(1)));
  std::string         addr2 = codAt2.addr;
  instructionList &   code2 = codAt2.code;
  instructionList &&   code = code1 || code2;

  std::string temp = "%"+codeCounters.newTEMP();
  if(ctx->AND())
    code = code || instruction::AND(temp,addr1,addr2);
  else if(ctx->OR())
    code = code || instruction::OR(temp,addr1,addr2);
  CodeAttribs codAts(temp,"",code);

  DEBUG_EXIT();
  return codAts;
}

std::any CodeGenVisitor::visitExprArray(AslParser::ExprArrayContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAtsE1 = std::any_cast<CodeAttribs>(visit(ctx->ident()));
  std::string           addr1 = codAtsE1.addr;
  TypesMgr::TypeId tid1 = getTypeDecor(ctx->ident());

  CodeAttribs     && codAtsE2 = std::any_cast<CodeAttribs>(visit(ctx->expr()));
  std::string           addr2 = codAtsE2.addr;
  instructionList &     code2 = codAtsE2.code;

  instructionList code = code2;

  TypesMgr::TypeId funcTy = getCurrentFunctionTy();
  std::vector<TypesMgr::TypeId> funcParamTypes = Types.getFuncParamsTypes(funcTy);
  bool addr1IsParamArray = false;
  for (auto & paramTy : funcParamTypes) {
    if (tid1 == paramTy) // and Types.isArrayTy(tid1))
      addr1IsParamArray = true;
  }

  std::string temp1;
  if (addr1IsParamArray) {
    temp1 = "%"+codeCounters.newTEMP();
    code = code || instruction::ILOAD(temp1,addr1);
  }
  else {
    temp1 = addr1;
  }

  std::string temp2 = "%"+codeCounters.newTEMP();
  code = code || instruction::LOADX(temp2,temp1,addr2);
  CodeAttribs codAts(temp2,"",code);

  DEBUG_EXIT();
  return codAts;
}

std::any CodeGenVisitor::visitExprFunc(AslParser::ExprFuncContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs     && codAt1 = std::any_cast<CodeAttribs>(visit(ctx->ident()));
  std::string         addr1 = codAt1.addr;
  std::vector<std::pair<CodeAttribs,TypesMgr::TypeId>> && params = std::any_cast<std::vector<std::pair<CodeAttribs,TypesMgr::TypeId>>>(visit(ctx->parameters()));

  instructionList code = instruction::PUSH(); // On retornar el resultat. ExprFunc sempre retorna resultat (i es guarda).

  TypesMgr::TypeId funcCallTy = Symbols.getGlobalFunctionType(addr1);
  std::vector<TypesMgr::TypeId> funcCallParamTypes = Types.getFuncParamsTypes(funcCallTy);
  for (long unsigned int i=0; i<params.size(); ++i) {
    CodeAttribs codAt2 = params[i].first;
    std::string         addr2 = codAt2.addr;
    // std::string         offs2 = codAt2.offs;
    instructionList &   code2 = codAt2.code;
    code = code || code2;
    TypesMgr::TypeId tid2 = params[i].second;

    TypesMgr::TypeId paramTy = funcCallParamTypes[i];
    if (Types.isFloatTy(paramTy) and Types.isIntegerTy(tid2)) {
      std::string tempAddr2 = "%"+codeCounters.newTEMP();
      code =  code || instruction::FLOAT(tempAddr2,addr2);
      code = code || instruction::PUSH(tempAddr2);
    }
    else if (Types.isArrayTy(paramTy)) {
      TypesMgr::TypeId funcCurrentTy = getCurrentFunctionTy();
      std::vector<TypesMgr::TypeId> funcCurrentParamTypes = Types.getFuncParamsTypes(funcCurrentTy);
      bool addr2IsParamArray = false;
      for (auto & paramTy2 : funcCurrentParamTypes) {
        if (tid2 == paramTy2) // and Types.isArrayTy(tid1))
          addr2IsParamArray = true;
      }

      std::string temp2 = "%"+codeCounters.newTEMP();;
      if (addr2IsParamArray) {
        code = code || instruction::ILOAD(temp2,addr2);
      }
      else {
        code = code || instruction::ALOAD(temp2,addr2);
      }
      code = code || instruction::PUSH(temp2);
    }
    else {
      code = code || instruction::PUSH(addr2);
    }
  }

  code = code || instruction::CALL(addr1);

  for (long unsigned int i=0; i<params.size(); ++i) {
    code = code || instruction::POP();
  }

  std::string temp = "%"+codeCounters.newTEMP();
  code = code || instruction::POP(temp); // On obtenim el resultat. ExprFunc sempre retorna resultat (i es guarda)

  CodeAttribs codAts(temp,"",code);

  DEBUG_EXIT();
  return codAts;
}

std::any CodeGenVisitor::visitExprValue(AslParser::ExprValueContext *ctx) {
  DEBUG_ENTER();
  CodeAttribs && codAts = std::any_cast<CodeAttribs>(visit(ctx->value()));
  DEBUG_EXIT();
  return codAts;
}


// Getters for the necessary tree node atributes:
//   Scope and Type
SymTable::ScopeId CodeGenVisitor::getScopeDecor(antlr4::ParserRuleContext *ctx) const {
  return Decorations.getScope(ctx);
}
TypesMgr::TypeId CodeGenVisitor::getTypeDecor(antlr4::ParserRuleContext *ctx) const {
  return Decorations.getType(ctx);
}


// Constructors of the class CodeAttribs:
//
CodeGenVisitor::CodeAttribs::CodeAttribs(const std::string & addr,
                                         const std::string & offs,
                                         instructionList & code) :
  addr{addr},offs{offs},code{code} {
}

CodeGenVisitor::CodeAttribs::CodeAttribs(const std::string & addr,
                                         const std::string & offs,
                                         instructionList && code) :
  addr{addr},offs{offs},code{code} {
}


/*
  Shell commands:
    make antlr
    make
    ./asl ../examples/jp_genc_12.asl > ../examples/jp_genc_12.genc
    ../tvm/tvm  ../examples/jp_genc_12.genc < ../examples/jp_genc_12.in > ../examples/jp_genc_12.res
    ../tvm/tvm  ../examples/jp_genc_12.genc < ../examples/jp_genc_12.in > ../examples/jp_genc_12.res --debug
    cat ../examples/jp_genc_12.out
    cat ../examples/jp_genc_12.res
    cat ../examples/jp_genc_12.asl
    cat ../examples/jp_genc_12.genc

*/
