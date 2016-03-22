//------------------------------------------------------------------------------
/// @brief SnuPL/0 scanner
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/09/10 Bernhard Egger assignment 1: scans SnuPL/-1
/// 2016/03/13 Bernhard Egger assignment 1: adapted to modified SnuPL/-1 syntax
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

#include <iostream>
#include <sstream>
#include <cctype>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <cstdio>

#include "scanner.h"
using namespace std;

//------------------------------------------------------------------------------
// token names
//
#define TOKEN_STRLEN 16

char ETokenName[][TOKEN_STRLEN] = {
  "tTermOp",                        ///< '+' or '-' or '||'
  "tFactOp",                        ///< '*' or '/' or '&&'
  "tRelOp",                         ///< relational operator
  "tAssign",                        ///< assignment operator
  "tSemicolon",                     ///< a semicolon
  "tColon",                         ///< a colon
  "tDot",                           ///< a dot
  "tComma",                         ///< a comma
  "tLBrak",                         ///< a left bracket
  "tRBrak",                         ///< a right bracket
  "tLSBrak",                        ///< a left square bracket
  "tRSBrak",                        ///< a right square bracket
  "tCompl",                         ///< complement operator

  "tModule",                        ///< 'module' keyword
  "tBegin",                         ///< 'begin' keyword
  "tEnd",                           ///< 'end' keyword
  "tBoolean",                       ///< 'true' or 'false'
  "tBaseType",                      ///< 'character' or 'boolean' or 'integer'
  "tIf",                            ///< 'if' keyword
  "tThen",                          ///< 'then' keyword
  "tElse",                          ///< 'else' keyword
  "tWhile",                         ///< 'while' keyword
  "tDo",                            ///< 'do' keyword
  "tReturn",                        ///< 'return' keyword
  "tVar",                           ///< 'var' keyword
  "tProcedure",                     ///< 'procedure' keyword
  "tFunction",                      ///< 'function' keyword

  "tChar",                          ///< a character
  "tString",                        ///< a string
  "tId",                            ///< a identifier
  "tNumber",                        ///< a number

  "tEOF",                           ///< end of file
  "tIOError",                       ///< I/O error
  "tUndefined (%s)"                 ///< undefined
};


//------------------------------------------------------------------------------
// format strings used for printing tokens
//

char ETokenStr[][TOKEN_STRLEN] = {
  "tTermOp (%s)",                   ///< '+' or '-' or '||'
  "tFactOp (%s)",                   ///< '*' or '/' or '&&'
  "tRelOp (%s)",                    ///< relational operator
  "tAssign",                        ///< assignment operator
  "tSemicolon",                     ///< a semicolon
  "tColon",                         ///< a colon
  "tDot",                           ///< a dot
  "tComma",                         ///< a comma
  "tLBrak",                         ///< a left bracket
  "tRBrak",                         ///< a right bracket
  "tLSBrak",                        ///< a left square bracket
  "tRSBrak",                        ///< a right square bracket
  "tCompl",                         ///< complement operator

  "tModule",                        ///< 'module' keyword
  "tBegin",                         ///< 'begin' keyword
  "tEnd",                           ///< 'end' keyword
  "tBoolean (%s)",                  ///< 'true' or 'false'
  "tBaseType (%s)",                 ///< 'character' or 'boolean' or 'integer'
  "tIf",                            ///< 'if' keyword
  "tThen",                          ///< 'then' keyword
  "tElse",                          ///< 'else' keyword
  "tWhile",                         ///< 'while' keyword
  "tDo",                            ///< 'do' keyword
  "tReturn",                        ///< 'return' keyword
  "tVar",                           ///< 'var' keyword
  "tProcedure",                     ///< 'procedure' keyword
  "tFunction",                      ///< 'function' keyword

  "tChar (%s)",                     ///< a character
  "tString (%s)",                   ///< a string
  "tId (%s)",                       ///< a identifier
  "tNumber (%s)",                   ///< a number

  "tEOF",                           ///< end of file
  "tIOError",                       ///< I/O error
  "tUndefined (%s)"                 ///< undefined
};


//------------------------------------------------------------------------------
// reserved keywords
//
pair<const char*, EToken> Keywords[] =
{
};



//------------------------------------------------------------------------------
// CToken
//
CToken::CToken()
{
  _type = tUndefined;
  _value = "";
  _line = _char = 0;
}

CToken::CToken(int line, int charpos, EToken type, const string value)
{
  _type = type;
  _value = escape(value);
  _line = line;
  _char = charpos;
}

CToken::CToken(const CToken &token)
{
  _type = token.GetType();
  _value = token.GetValue();
  _line = token.GetLineNumber();
  _char = token.GetCharPosition();
}

CToken::CToken(const CToken *token)
{
  _type = token->GetType();
  _value = token->GetValue();
  _line = token->GetLineNumber();
  _char = token->GetCharPosition();
}

const string CToken::Name(EToken type)
{
  return string(ETokenName[type]);
}

const string CToken::GetName(void) const
{
  return string(ETokenName[GetType()]);
}

ostream& CToken::print(ostream &out) const
{
  int str_len = _value.length();
  str_len = TOKEN_STRLEN + (str_len < 64 ? str_len : 64);
  char *str = (char*)malloc(str_len);
  snprintf(str, str_len, ETokenStr[GetType()], _value.c_str());
  out << dec << _line << ":" << _char << ": " << str;
  free(str);
  return out;
}

string CToken::escape(const string text)
{
  const char *t = text.c_str();
  string s;

  while (*t != '\0') {
    switch (*t) {
      case '\n': s += "\\n";  break;
      case '\t': s += "\\t";  break;
      case '\0': s += "\\0";  break;
      case '\'': s += "\\'";  break;
      case '\"': s += "\\\""; break;
      case '\\': s += "\\\\"; break;
      default :  s += *t;
    }
    t++;
  }

  return s;
}

ostream& operator<<(ostream &out, const CToken &t)
{
  return t.print(out);
}

ostream& operator<<(ostream &out, const CToken *t)
{
  return t->print(out);
}


//------------------------------------------------------------------------------
// CScanner
//
map<string, EToken> CScanner::keywords;

CScanner::CScanner(istream *in)
{
  InitKeywords();
  _in = in;
  _delete_in = false;
  _line = _char = 1;
  _token = NULL;
  _good = in->good();
  NextToken();
}

CScanner::CScanner(string in)
{
  InitKeywords();
  _in = new istringstream(in);
  _delete_in = true;
  _line = _char = 1;
  _token = NULL;
  _good = true;
  NextToken();
}

CScanner::~CScanner()
{
  if (_token != NULL) delete _token;
  if (_delete_in) delete _in;
}

void CScanner::InitKeywords(void)
{
  if (keywords.size() == 0) {
    int size = sizeof(Keywords) / sizeof(Keywords[0]);
    for (int i=0; i<size; i++) {
      keywords[Keywords[i].first] = Keywords[i].second;
    }
  }
}

CToken CScanner::Get()
{
  CToken result(_token);

  EToken type = _token->GetType();
  _good = !(type == tIOError);

  NextToken();
  return result;
}

CToken CScanner::Peek() const
{
  return CToken(_token);
}

void CScanner::NextToken()
{
  if (_token != NULL) delete _token;

  _token = Scan();
}

void CScanner::RecordStreamPosition()
{
  _saved_line = _line;
  _saved_char = _char;
}

void CScanner::GetRecordedStreamPosition(int *lineno, int *charpos)
{
  *lineno = _saved_line;
  *charpos = _saved_char;
}

CToken* CScanner::NewToken(EToken type, const string token)
{
  return new CToken(_saved_line, _saved_char, type, token);
}

CToken* CScanner::Scan()
{
  EToken token;
  string tokval;
  char c;
  while (true) {
    while (_in->good() && IsWhite(_in->peek())) GetChar();

    RecordStreamPosition();

    if (_in->eof()) return NewToken(tEOF);
    if (!_in->good()) return NewToken(tIOError);

    c = GetChar();
    if (c == '/' && _in->peek() == '/') {
      while(_in->peek() != '\n' && !(_in->eof())) GetChar();
      continue;
    }
    break;
  }

  tokval = c;
  token = tUndefined;

  switch (c) {
    case '|':
      if (_in->peek() != '|')
        break;
      tokval += GetChar();
    case '+':
    case '-':
      token = tTermOp;
      break;

    case '&':
      if (_in->peek() != '&')
        break;
      tokval += GetChar();
    case '*':
    case '/':
      token = tFactOp;
      break;

    case ':':
      if (_in->peek() == '=') {
        tokval += GetChar();
        token = tAssign;
      } else {
        token = tColon;
      }
      break;

    case ';':
      token = tSemicolon;
      break;

    case '.':
      token = tDot;
      break;
    case ',':
      token = tComma;
      break;

    case '(':
      token = tLBrak;
      break;
    case ')':
      token = tRBrak;
      break;
    case '[':
      token = tLSBrak;
      break;
    case ']':
      token = tRSBrak;
      break;

    case '!':
      token = tCompl;
      break;

    case '<':
    case '>':
      if (_in->peek() == '=') {
        tokval += GetChar();
      }
    case '#':
    case '=':
      token = tRelOp;
      break;

    case '"': {
      string str = GetCharacterUntil('"');
      if (_in->peek() == '"') {
        GetChar();
        if (!IsUnescapable(str)) {
          tokval = "unescapable string \"";
          tokval += str;
          tokval += "\"";
        } else {
          tokval = Unescape(str);
          token = tString;
        }
      } else {
        tokval = "No closing \"";
      }
      break;
    }

    case '\'': {
      string str = GetCharacterUntil('\'');
      if (_in->peek() == '\'') {
        GetChar();
        if (!IsUnescapable(str)) {
          tokval = "unescapable string \"";
          tokval += str;
          tokval += "\"";
        } else {
          string unescapedStr = Unescape(str);
          if (unescapedStr.length() != 1) {
            tokval = "more than one character in string \"";
            tokval += str;
            tokval += "\"";
          } else {
            tokval = Unescape(str);
            token = tString;
          }
        }
      } else {
        tokval = "No closing '";
      }
      break;
    }

    default:
      if (IsDigit(c)) {
        while (true) {
          char lookAhead = _in->peek();
          if (IsDigit(lookAhead)) {
            tokval += GetChar();
          } else {
            break;
          }
        }
        token = tNumber;
      } else
      if (IsLetter(c)) {
        while (true) {
          char lookAhead = _in->peek();
          if (IsLetter(lookAhead) || IsDigit(lookAhead)) {
            tokval += GetChar();
          } else {
            break;
          }
        }
        token = TokenForIdentifier(tokval);
      } else {
        tokval = "invalid character '";
        tokval += c;
        tokval += "'";
      }
      break;
  }

  return NewToken(token, tokval);
}

char CScanner::GetChar()
{
  char c = _in->get();
  if (c == '\n') { _line++; _char = 1; } else _char++;
  return c;
}

string CScanner::GetChar(int n)
{
  string str;
  for (int i=0; i<n; i++) str += GetChar();
  return str;
}

bool CScanner::IsWhite(char c) const
{
  return ((c == ' ') || (c == '\n') || (c == '\t'));
}

bool CScanner::IsLetter(char c) const
{
  return ((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')) || (c == '_');
}

bool CScanner::IsDigit(char c) const
{
  return (c >= '0') && (c <= '9');
}
EToken CScanner::TokenForIdentifier(string s) const
{
  if (s == "module") return tModule;
  else if (s == "begin") return tBegin;
  else if (s == "end") return tEnd;
  else if (s == "true" || s == "false") return tBoolean;
  else if (s == "character" || s == "boolean" || s == "integer") return tBaseType;
  else if (s == "if") return tIf;
  else if (s == "then") return tThen;
  else if (s == "else") return tElse;
  else if (s == "while") return tWhile;
  else if (s == "do") return tDo;
  else if (s == "return") return tReturn;
  else if (s == "var") return tVar;
  else if (s == "procedure") return tProcedure;
  else if (s == "function") return tFunction;
  else return tId;
}

bool CScanner::IsUnescapable(string s) const
{
  for (std::string::iterator it = s.begin() ; it != s.end(); it++) {
    if ((*it) == '\\') {
      it++;
      if (it == s.end()) return false;
      switch (*it) {
      case 'n':
      case 't':
      case '0':
      case '"':
      case '\'':
      case '\\':
        break;
      default:
        return false;
      }
    }
  }
  return true;
}

string CScanner::Unescape(string s) const
{
  string retStr;
  for (std::string::iterator it = s.begin() ; it != s.end(); it++) {
    if ((*it) == '\\') {
      it++;
      if (it == s.end()) return retStr;
      switch (*it) {
      case 'n': retStr += '\n'; break;
      case 't': retStr += '\t'; break;
      case '0': retStr += '\0'; break;
      case '"': retStr += '"'; break;
      case '\'': retStr += '\''; break;
      case '\\': retStr += '\\'; break;
      default:
        return retStr;
      }
    } else {
      retStr += (*it);
    }
  }
  return retStr;
}

string CScanner::GetCharacterUntil(char stopc) {
  string str;
  while (!(_in->eof()) && _in->peek() != '\n' && _in->peek() != stopc) {
    char c = GetChar();
    str += c;
    if (c == '\\') {
      if (!(_in->eof()) && _in->peek() != '\n') {
        str += GetChar();
      }
      else return str;
    }
  }
  return str;
}
