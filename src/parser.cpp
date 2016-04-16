//------------------------------------------------------------------------------
/// @brief SnuPL/0 parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/11/04 Bernhard Egger maintain unary '+' signs in the AST
/// 2016/04/01 Bernhard Egger adapted to SnuPL/1 (this is not a joke)
/// 2016/09/28 Bernhard Egger assignment 2: parser for SnuPL/-1
///
/// @section license_section License
/// Copyright (c) 2012-2016, Bernhard Egger
/// All rights reserved.
///
/// Redistribution and use in source and binary forms,  with or without modifi-
/// cation, are permitted provided that the following conditions are met:
///
/// - Redistributions of source code must retain the above copyright notice,
///   this list of conditions and the following disclaimer.
/// - Redistributions in binary form must reproduce the above copyright notice,
///   this list of conditions and the following disclaimer in the documentation
///   and/or other materials provided with the distribution.
///
/// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
/// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT LIMITED TO,  THE
/// IMPLIED WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE
/// ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDER  OR CONTRIBUTORS BE
/// LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSE-
/// QUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF  SUBSTITUTE
/// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
/// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN  CONTRACT, STRICT
/// LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY WAY
/// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
/// DAMAGE.
//------------------------------------------------------------------------------

#include <limits.h>
#include <cassert>
#include <errno.h>
#include <cstdlib>
#include <vector>
#include <iostream>
#include <exception>

#include "parser.h"
using namespace std;


//------------------------------------------------------------------------------
// CParser
//
CParser::CParser(CScanner *scanner)
{
  _scanner = scanner;
  _module = NULL;
}

CAstNode* CParser::Parse(void)
{
  _abort = false;

  if (_module != NULL) { delete _module; _module = NULL; }

  try {
    if (_scanner != NULL) _module = module();

    if (_module != NULL) {
      CToken t;
      string msg;
      //if (!_module->TypeCheck(&t, &msg)) SetError(t, msg);
    }
  } catch (...) {
    _module = NULL;
  }

  return _module;
}

const CToken* CParser::GetErrorToken(void) const
{
  if (_abort) return &_error_token;
  else return NULL;
}

string CParser::GetErrorMessage(void) const
{
  if (_abort) return _message;
  else return "";
}

void CParser::SetError(CToken t, const string message)
{
  _error_token = t;
  _message = message;
  _abort = true;
  throw message;
}

bool CParser::Consume(EToken type, CToken *token)
{
  if (_abort) return false;

  CToken t = _scanner->Get();

  if (t.GetType() != type) {
    SetError(t, "expected '" + CToken::Name(type) + "', got '" +
             t.GetName() + "'");
  }

  if (token != NULL) *token = t;

  return t.GetType() == type;
}

void CParser::InitSymbolTable(CSymtab *s)
{
  CTypeManager *tm = CTypeManager::Get();

  CSymProc* t = new CSymProc("DIM", tm->GetInt());
  t->AddParam(new CSymParam(0, "arg0", tm->GetPointer(tm->GetNull())));
  t->AddParam(new CSymParam(1, "arg1", tm->GetInt()));
  s->AddSymbol(t);

  t = new CSymProc("DOFS", tm->GetInt());
  t->AddParam(new CSymParam(0, "arg", tm->GetPointer(tm->GetNull())));
  s->AddSymbol(t);

  t = new CSymProc("PrintInt", tm->GetNull());
  t->AddParam(new CSymParam(0, "arg", tm->GetInt()));
  s->AddSymbol(t);

  t = new CSymProc("ReadInt", tm->GetInt());
  s->AddSymbol(t);

  t = new CSymProc("WriteChar", tm->GetNull());
  t->AddParam(new CSymParam(0, "arg", tm->GetChar()));
  s->AddSymbol(t);

  t = new CSymProc("WriteInt", tm->GetNull());
  t->AddParam(new CSymParam(0, "arg", tm->GetInt()));
  s->AddSymbol(t);

  t = new CSymProc("WriteLn", tm->GetNull());
  s->AddSymbol(t);

  t = new CSymProc("WriteStr", tm->GetNull());
  t->AddParam(new CSymParam(0, "arg", tm->GetPointer(tm->GetArray(CArrayType::OPEN, tm->GetChar()))));
  s->AddSymbol(t);
}

CAstModule* CParser::module(void)
{
  //
  // module ::= "module" ident ";" varDeclaration { subroutineDecl } "begin" statSequence "end" ident "."
  //
  CToken idToken;
  Consume(tModule);
  Consume(tId, &idToken);
  Consume(tSemicolon);

  CAstModule *m = new CAstModule(idToken, idToken.GetValue());
  InitSymbolTable(m->GetSymbolTable());

  vector<CVariable> vec = varDeclaration(m);
  for (CVariable it : vec) {
    CSymbol * sb = m->CreateVar(it.first, it.second);
    m->GetSymbolTable()->AddSymbol(sb);
  }
  while(_scanner->Peek().GetType() != tBegin) {
    CAstProcedure *proc = subroutineDecl(m);
    m->GetSymbolTable()->AddSymbol(proc->GetSymbol());
  }
  Consume(tBegin);
  CAstStatement *statseq = statSequence(m);
  m->SetStatementSequence(statseq);
  Consume(tEnd);
  CToken idToken2;
  Consume(tId, &idToken2);

  if (idToken.GetValue() != idToken2.GetValue()) {
    SetError(idToken2, "invalid end identifier");
  }

  Consume(tDot);

  return m;
}

CAstStatement* CParser::statSequence(CAstScope *s)
{
  //
  // statSequence ::= [ statement { ";" statement } ].
  // statement ::= assignment | subroutineCall
  // statement ::= ifStatement | whileStatement | returnStatement
  // FIRST(statSequence) = { tId, tIf, tWhile, tReturn }
  // FOLLOW(statSequence) = { tElse, tEnd }
  //
  CAstStatement *head = NULL;

  EToken tt = _scanner->Peek().GetType();
  if (!(tt == tEnd || tt == tElse)) {
    CAstStatement *tail = NULL;

    do {
      CToken t;
      EToken tt = _scanner->Peek().GetType();
      CAstStatement *st = NULL;

      switch (tt) {
        // statement ::= assignment | subroutineCall
        case tId:
          Consume(tId, &t);
          if (_scanner->Peek().GetType() == tLBrak ) {
            st = subroutineCall(s, t);
          } else {
            st = assignment(s, t);
          }
          break;
        // statement ::= ifStatement
        case tIf:
          st = ifStatement(s);
          break;
        // statement ::= whileStatement
        case tWhile:
          st = whileStatement(s);
          break;
        // statement ::= returnStatement
        case tReturn:
          st = returnStatement(s);
          break;

        default:
          SetError(_scanner->Peek(), "statement expected.");
          break;
      }

      assert(st != NULL);
      if (head == NULL) head = st;
      else tail->SetNext(st);
      tail = st;

      tt = _scanner->Peek().GetType();
      if (tt != tSemicolon) break;

      Consume(tSemicolon);
    } while (!_abort);
  }

  return head;
}

CAstStatAssign* CParser::assignment(CAstScope *s, CToken idToken)
{
  //
  // assignment ::= qualident ":=" expression.
  //
  CToken t;
  CAstDesignator *lhs = qualident(s, idToken);
  Consume(tAssign, &t);

  CAstExpression *rhs = expression(s);
  return new CAstStatAssign(t, lhs, rhs);
}

CAstStatCall* CParser::subroutineCall(CAstScope *s, CToken idToken)
{
  //
  // subroutineCall ::= ident "(" [ expression {"," expression} ] ")"
  //
  CAstStatCall* n = NULL;

  const CSymbol *tsb = s->GetSymbolTable()->FindSymbol(idToken.GetValue());
  const CSymProc *sb = dynamic_cast<const CSymProc *>(tsb);
  if (sb == NULL) {
    SetError(idToken, "invalid symbol.");
    return NULL;
  }
  CAstFunctionCall* fc = new CAstFunctionCall(idToken, sb);

  Consume(tLBrak);

  if(_scanner->Peek().GetType() != tRBrak) {
    CAstExpression* arg = expression(s);
    fc->AddArg(arg);
    while(_scanner->Peek().GetType() == tComma) {
      Consume(tComma);
      CAstExpression* arg = expression(s);
      fc->AddArg(arg);
    }
  }

  Consume(tRBrak);

  n = new CAstStatCall(idToken, fc);
  return n;
}

CAstExpression* CParser::expression(CAstScope* s)
{
  //
  // expression ::= simpleexpr [ relOp simpleexpression ].
  //
  CToken t;
  EOperation relop;
  CAstExpression *left = NULL, *right = NULL;

  left = simpleexpr(s);

  if (_scanner->Peek().GetType() == tRelOp) {
    Consume(tRelOp, &t);
    right = simpleexpr(s);

    if (t.GetValue() == "=")       relop = opEqual;
    else if (t.GetValue() == "#")  relop = opNotEqual;
    else if (t.GetValue() == "<")  relop = opLessThan;
    else if (t.GetValue() == "<=")  relop = opLessEqual;
    else if (t.GetValue() == ">")  relop = opBiggerThan;
    else if (t.GetValue() == ">=")  relop = opBiggerEqual;
    else SetError(t, "invalid relation.");

    return new CAstBinaryOp(t, relop, left, right);
  } else {
    return left;
  }
}

CAstExpression* CParser::simpleexpr(CAstScope *s)
{
  //
  // simpleexpr ::= ["+" | "-"] term { termOp term }.
  //
  CAstExpression *n = NULL;
  if (_scanner->Peek().GetType() == tTermOp) {
    if (_scanner->Peek().GetValue() == "||") {
      SetError(_scanner->Peek(), "'+' or '-' expected");
      return n;
    }
    CToken t;
    Consume(tTermOp, &t);
    n = new CAstUnaryOp(t, t.GetValue() == "+" ? opPos : opNeg, term(s) );
  } else {
    n = term(s);
  }

  while (_scanner->Peek().GetType() == tTermOp) {
    CToken t;
    CAstExpression *l = n, *r;

    Consume(tTermOp, &t);

    r = term(s);

    n = new CAstBinaryOp(t, t.GetValue() == "+" ? opAdd : t.GetValue() == "-" ? opSub : opOr, l, r);
  }


  return n;
}

CAstExpression* CParser::term(CAstScope *s)
{
  //
  // term ::= factor { ("*"|"/"|"&&") factor }.
  //
  CAstExpression *n = NULL;

  n = factor(s);

  EToken tt = _scanner->Peek().GetType();

  while ((tt == tFactOp)) {
    CToken t;
    CAstExpression *l = n, *r;

    Consume(tFactOp, &t);

    r = factor(s);

    n = new CAstBinaryOp(t, t.GetValue() == "*" ? opMul : t.GetValue() == "/" ? opDiv : opAnd , l, r);

    tt = _scanner->Peek().GetType();
  }

  return n;
}

CAstExpression* CParser::factor(CAstScope *s)
{
  //
  // factor ::= number | "(" expression ")" | number | boolean | character | string | "!" factor
  // factor ::= qualident | subroutineCall
  //

  CToken t;
  EToken tt = _scanner->Peek().GetType();
  CAstExpression *unary = NULL, *n = NULL;

  switch (tt) {
    // factor ::= number
    case tNumber:
      n = number();
      break;

    // factor ::= "(" expression ")"
    case tLBrak:
      Consume(tLBrak);
      n = expression(s);
      Consume(tRBrak);
      break;

    // factor ::= boolean
    case tBoolean:
      n = boolean();
      break;

    // factor ::= char
    case tChar:
      n = character();
      break;

    // factor ::= string
    case tString:
      n = strConstant(s);
      break;

    // factor ::= "!" factor
    case tCompl:
      Consume(tCompl, &t);
      n = new CAstUnaryOp(t, opNot, factor(s));
      break;

    // factor ::= qualident | subroutineCall
    // qualident and subroutineCall starts with Identifier.
    // Make lookahead to 2 for this case
    case tId:
      Consume(tId, &t);
      if (_scanner->Peek().GetType() == tLBrak ) {
        n = subroutineCall(s, t)->GetCall();
      } else {
        n = qualident(s, t);
      }
      break;

    default:
      cout << "got " << _scanner->Peek() << endl;
      SetError(_scanner->Peek(), "factor expected.");
      break;
  }

  return n;
}

CAstConstant* CParser::number(void)
{
  //
  // number ::= digit { digit }.
  //
  // "digit { digit }" is scanned as one token (tNumber)
  //

  CToken t;

  Consume(tNumber, &t);

  errno = 0;
  long long v = strtoll(t.GetValue().c_str(), NULL, 10);
  if (errno != 0) SetError(t, "invalid number.");

  return new CAstConstant(t, CTypeManager::Get()->GetInt(), v);
}

CAstConstant* CParser::boolean(void)
{
  //
  // boolean ::= "true" | "false"
  //
  // "true" | "false" is scanned as one token (tBoolean)
  //

  CToken t;

  Consume(tBoolean, &t);

  errno = 0;
  bool v = t.GetValue() == "true";
  if (errno != 0) SetError(t, "invalid boolean.");

  return new CAstConstant(t, CTypeManager::Get()->GetBool(), v);
}

CAstConstant* CParser::character(void)
{
  //
  // char ::= "'" character "'"
  //
  // "'" character "'" is scanned as one token (tChar)
  //

  CToken t;

  Consume(tChar, &t);

  errno = 0;
  char v = CToken::unescape(t.GetValue()).c_str()[0];
  if (errno != 0) SetError(t, "invalid character.");

  return new CAstConstant(t, CTypeManager::Get()->GetChar(), v);
}

CAstStringConstant* CParser::strConstant(CAstScope *s)
{
  //
  // string ::= '"' { character } '"'
  //
  // '"' { character } '"' is scanned as one token (tString)
  //

  CToken t;

  Consume(tString, &t);

  errno = 0;
  string v = t.GetValue();
  if (errno != 0) SetError(t, "invalid string.");

  return new CAstStringConstant(t, v, s);
}

CAstStatIf* CParser::ifStatement(CAstScope *s)
{
  //
  // ifStatement ::= "if" "(" expression ")" "then" stateSequence [ "else" stateSequence ] "end"
  //

  CToken t;

  CAstExpression* condition = NULL;
  CAstStatement* ifBody = NULL;
  CAstStatement* elseBody = NULL;

  EToken tt;

  Consume(tIf, &t);
  Consume(tLBrak);
  condition = expression(s);
  Consume(tRBrak);
  Consume(tThen);
  ifBody = statSequence(s);

  tt = _scanner->Peek().GetType();
  if (tt == tElse){
    Consume(tElse);
    elseBody = statSequence(s);
  }
  Consume(tEnd);

  return new CAstStatIf(t, condition, ifBody, elseBody);
}

CAstStatReturn* CParser::returnStatement(CAstScope *s)
{
  //
  // returnStatement ::= "return" [ expression ]
  //

  CToken t;

  CAstExpression* retval = NULL;

  Consume(tReturn, &t);

  EToken tt = _scanner->Peek().GetType();
  if (!(tt == tElse || tt == tEnd || tt == tSemicolon)){
    retval = expression(s);
  }

  return new CAstStatReturn(t, s, retval);
}

CAstStatWhile* CParser::whileStatement(CAstScope *s)
{
  //
  // whileStatement ::= "while" "(" expression ")" "do" statSequence "end"
  //

  CToken t;

  CAstExpression* condition = NULL;
  CAstStatement* body = NULL;

  Consume(tWhile, &t);
  Consume(tLBrak);
  condition = expression(s);
  Consume(tRBrak);
  Consume(tDo);
  body = statSequence(s);
  Consume(tEnd);

  return new CAstStatWhile(t, condition, body);
}

const CType* CParser::type()
{
  //
  // type ::= basetype | type "[" [ number ] "]"
  // this is left recursion -> left factoring
  // type ::= basetype {"[" [number] "]"}
  //
  CToken t, bt;
  Consume(tBaseType, &bt);
  const CType* n = NULL;
  if (bt.GetValue() == "char") n = CTypeManager::Get()->GetChar();
  else if (bt.GetValue() == "boolean") n = CTypeManager::Get()->GetBool();
  else if (bt.GetValue() == "integer") n = CTypeManager::Get()->GetInt();
  else SetError(bt, "invalid base type");
  vector<long long> v;
  while(_scanner->Peek().GetType() == tLSBrak) {
    Consume(tLSBrak);
    if (_scanner->Peek().GetType() == tNumber) {
      CAstConstant* c = number();
      v.push_back(c->GetValue());
    } else {
      v.push_back(CArrayType::OPEN);
    }

    Consume(tRSBrak);
  }
  for (int i = v.size() - 1; i >= 0; i--) {
    n = new CArrayType( (int) v[i], n);
  }
  return n;
}

vector<CVariable> CParser::varDecl(CAstScope *s)
{
  //
  // varDecl ::= ident { "," ident } ":" type
  //
  CToken t;
  Consume(tId, &t);
  vector<string> v;
  v.push_back(t.GetValue());
  while(_scanner->Peek().GetType() == tComma) {
    Consume(tComma);
    Consume(tId, &t);
    v.push_back(t.GetValue());
  }
  Consume(tColon);
  const CType* ct = type();
  vector<CVariable> ret;
  for (string it : v) {
    ret.push_back(make_pair(it,ct));
  }
  return ret;
}

vector<CVariable> CParser::varDeclSequence(CAstScope *s)
{
  //
  // varDeclSequence ::= varDecl { ";" varDecl }
  //
  vector<CVariable> ret;
  vector<CVariable> tmp = varDecl(s);
  ret.insert(ret.end(), tmp.begin(), tmp.end());
  while (_scanner->Peek().GetType() == tSemicolon) {
    Consume(tSemicolon);
    tmp = varDecl(s);
    ret.insert(ret.end(), tmp.begin(), tmp.end());
  }
  return ret;
}

vector<CVariable> CParser::varDeclaration(CAstScope *s)
{
  //
  // varDeclaration ::= [ "var" varDeclSequence ";" ]
  // because this is ambiguous, express varDeclSequence with varDecl
  // varDeclaration ::= [ "var" varDecl ";" { varDecl ";" } ]
  //
  vector<CVariable> ret;
  if (_scanner->Peek().GetType() != tVar)
    return ret;
  Consume(tVar);
  vector<CVariable> tmp = varDecl(s);
  ret.insert(ret.end(), tmp.begin(), tmp.end());
  Consume(tSemicolon);
  while (_scanner->Peek().GetType() == tId) {
    tmp = varDecl(s);
    ret.insert(ret.end(), tmp.begin(), tmp.end());
    Consume(tSemicolon);
  }
  return ret;
}

CAstProcedure* CParser::subroutineDecl(CAstScope *s)
{
  //
  // subroutineDecl ::= (procedureDecl | functionDecl) subroutineBody ident ";"
  // procedureDecl ::= "procedure" ident [ formalParam ] ";"
  // functionDecl ::= "function" ident [ formalParam ] ":" type ";"
  // formalParam ::= "(" [ varDeclSequence ] ")"
  // subroutineBody ::= varDeclaration "begin" statSequence "end"
  //
  // since variable other than subroutineDecl are used only in subroutineDecl,
  // we decided not to make functions of those variables
  //
  CToken idToken;
  CAstProcedure* n;
  bool isProc;
  if (_scanner->Peek().GetType() == tProcedure) {
    Consume(tProcedure);
    isProc = true;
  } else if (_scanner->Peek().GetType() == tFunction) {
    Consume(tFunction);
    isProc = false;
  } else {
    SetError(_scanner->Peek(), "expected \"procedure\" or \"function\"");
  }

  Consume (tId, &idToken);
  vector<CVariable> paramVec;
  if (_scanner->Peek().GetType() == tLBrak) {
    Consume(tLBrak);
    if (_scanner->Peek().GetType() != tRBrak)
      paramVec = varDeclSequence(s);
    Consume(tRBrak);
  }

  CSymProc* symb;

  if(isProc) {
    Consume(tSemicolon);
    symb = new CSymProc(idToken.GetValue(), CTypeManager::Get()->GetNull());
  } else {
    Consume(tColon);
    const CType* t = type();
    Consume(tSemicolon);
    symb = new CSymProc(idToken.GetValue(), t);
  }

  for (int i=0; i<paramVec.size(); i++) {
    CVariable it = paramVec[i];
    symb->AddParam( new CSymParam(i, it.first, it.second) );
  }

  n = new CAstProcedure(idToken, idToken.GetValue(), s, symb);

  for (int i=0; i<paramVec.size(); i++) {
    CVariable it = paramVec[i];
    n->GetSymbolTable()->AddSymbol(new CSymParam(i, it.first, it.second));
  }

  vector<CVariable> localVec = varDeclaration(n);
  for (CVariable it : localVec) {
    CSymbol * sb = n->CreateVar(it.first, it.second);
    n->GetSymbolTable()->AddSymbol(sb);
  }
  Consume(tBegin);
  CAstStatement* body = statSequence(n);
  n->SetStatementSequence(body);
  Consume(tEnd);

  CToken idToken2;
  Consume(tId, &idToken2);
  if (idToken.GetValue() != idToken2.GetValue()) {
    SetError(idToken2, "invalid end identifier");
  }
  Consume(tSemicolon);
  return n;
}

CAstDesignator* CParser::qualident(CAstScope *s, CToken idToken)
{
  //
  // qualident ::= ident {"[" expression "]"}
  //

  const CSymbol *sb = s->GetSymbolTable()->FindSymbol(idToken.GetValue());
  if (sb == NULL) SetError(idToken, "undefined identifier");
  const CType *st = sb->GetDataType();
  vector<CAstExpression*> ev;
  while (_scanner->Peek().GetType() == tLSBrak) {
    Consume(tLSBrak);
    ev.push_back(expression(s));
    Consume(tRSBrak);
  }
  if (ev.size() == 0) {
    return new CAstDesignator(idToken, sb);
  }

  CAstArrayDesignator* n = new CAstArrayDesignator(idToken, sb);
  for (CAstExpression* it : ev) {
    n->AddIndex(it);
  }
  n->IndicesComplete();
  return n;
}

