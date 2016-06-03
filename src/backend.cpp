//------------------------------------------------------------------------------
/// @brief SnuPL backend
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/11/28 Bernhard Egger created
/// 2013/06/09 Bernhard Egger adapted to SnuPL/0
/// 2016/04/04 Bernhard Egger adapted to SnuPL/1
///
/// @section license_section License
/// Copyright (c) 2012-2016 Bernhard Egger
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

#include <fstream>
#include <sstream>
#include <iomanip>
#include <cassert>

#include "backend.h"
using namespace std;


//------------------------------------------------------------------------------
// CBackend
//
CBackend::CBackend(ostream &out)
  : _out(out)
{
}

CBackend::~CBackend(void)
{
}

bool CBackend::Emit(CModule *m)
{
  assert(m != NULL);
  _m = m;

  if (!_out.good()) return false;

  bool res = true;

  try {
    EmitHeader();
    EmitCode();
    EmitData();
    EmitFooter();

    res = _out.good();
  } catch (...) {
    res = false;
  }

  return res;
}

void CBackend::EmitHeader(void)
{
}

void CBackend::EmitCode(void)
{
}

void CBackend::EmitData(void)
{
}

void CBackend::EmitFooter(void)
{
}


//------------------------------------------------------------------------------
// CBackendx86
//
CBackendx86::CBackendx86(ostream &out)
  : CBackend(out), _curr_scope(NULL)
{
  _ind = string(4, ' ');
}

CBackendx86::~CBackendx86(void)
{
}

void CBackendx86::EmitHeader(void)
{
  _out << "##################################################" << endl
       << "# " << _m->GetName() << endl
       << "#" << endl
       << endl;
}

void CBackendx86::EmitCode(void)
{
  _out << _ind << "#-----------------------------------------" << endl
       << _ind << "# text section" << endl
       << _ind << "#" << endl
       << _ind << ".text" << endl
       << _ind << ".align 4" << endl
       << endl
       << _ind << "# entry point and pre-defined functions" << endl
       << _ind << ".global main" << endl
       << _ind << ".extern DIM" << endl
       << _ind << ".extern DOFS" << endl
       << _ind << ".extern ReadInt" << endl
       << _ind << ".extern WriteInt" << endl
       << _ind << ".extern WriteStr" << endl
       << _ind << ".extern WriteChar" << endl
       << _ind << ".extern WriteLn" << endl
       << endl;

  CModule* module = _m;
  for (CScope* s : module->GetSubscopes()) {
    EmitScope(s);
  }
  EmitScope(module);


  _out << _ind << "# end of text section" << endl
       << _ind << "#-----------------------------------------" << endl
       << endl;
}

void CBackendx86::EmitData(void)
{
  _out << _ind << "#-----------------------------------------" << endl
       << _ind << "# global data section" << endl
       << _ind << "#" << endl
       << _ind << ".data" << endl
       << _ind << ".align 4" << endl
       << endl;

  EmitGlobalData(_m);

  _out << _ind << "# end of global data section" << endl
       << _ind << "#-----------------------------------------" << endl
       << endl;
}

void CBackendx86::EmitFooter(void)
{
  _out << _ind << ".end" << endl
       << "##################################################" << endl;
}

void CBackendx86::SetScope(CScope *scope)
{
  _curr_scope = scope;
}

CScope* CBackendx86::GetScope(void) const
{
  return _curr_scope;
}

void CBackendx86::EmitScope(CScope *scope)
{
  assert(scope != NULL);
  SetScope(scope);

  string label;

  if (scope->GetParent() == NULL) label = "main";
  else label = scope->GetName();

  // label
  _out << _ind << "# scope " << scope->GetName() << endl
       << label << ":" << endl;

  size_t local_size = ComputeStackOffsets(scope->GetSymbolTable(), 8, -12);

  //Prologue Instructions
  _out << endl << _ind << "# prologue" << endl;

  EmitInstruction("pushl", "%ebp", "save ebp");
  EmitInstruction("movl", "%esp, %ebp", "move ebp");
  EmitInstruction("pushl", "%ebx");
  EmitInstruction("pushl", "%esi");
  EmitInstruction("pushl", "%edi", "save callee registers");
  EmitInstruction("subl", Imm(local_size) + ", %esp", "make room for locals");

  if (local_size != 0) {
    EmitInstruction("cld", "", "memset local parameters to 0");
    EmitInstruction("xorl", "%eax, %eax", "memset local parameters to 0");
    EmitInstruction("movl", Imm(local_size/4) + ", %ecx");
    EmitInstruction("movl", "%esp, %edi");
    EmitInstruction("rep", "stosl");
  }

  EmitLocalData(scope);

  _out << endl << _ind << "# function body" << endl;

  EmitCodeBlock(scope->GetCodeBlock());

  //Epilogue Insturctions

  _out << endl << Label("exit") << ":" << endl;
  _out << _ind << "# epilogue" << endl;

  EmitInstruction("addl", Imm(local_size) + ", %esp", "remove local variables");
  EmitInstruction("popl", "%edi", "load callee registers");
  EmitInstruction("popl", "%esi", "load callee registers");
  EmitInstruction("popl", "%ebx", "load callee registers");
  EmitInstruction("popl", "%ebp", "load callee registers");

  EmitInstruction("ret");
  SetScope(NULL);
  _out << endl;
}

void CBackendx86::EmitGlobalData(CScope *scope)
{
  assert(scope != NULL);

  // emit the globals for the current scope
  CSymtab *st = scope->GetSymbolTable();
  assert(st != NULL);

  bool header = false;

  vector<CSymbol*> slist = st->GetSymbols();

  _out << dec;

  size_t size = 0;

  for (size_t i=0; i<slist.size(); i++) {
    CSymbol *s = slist[i];
    const CType *t = s->GetDataType();

    if (s->GetSymbolType() == stGlobal) {
      if (!header) {
        _out << _ind << "# scope: " << scope->GetName() << endl;
        header = true;
      }

      // insert alignment only when necessary
      if ((t->GetAlign() > 1) && (size % t->GetAlign() != 0)) {
        size += t->GetAlign() - size % t->GetAlign();
        _out << setw(4) << " " << ".align "
             << right << setw(3) << t->GetAlign() << endl;
      }

      _out << left << setw(36) << s->GetName() + ":" << "# " << t << endl;

      if (t->IsArray()) {
        const CArrayType *a = dynamic_cast<const CArrayType*>(t);
        assert(a != NULL);
        int dim = a->GetNDim();

        _out << setw(4) << " "
          << ".long " << right << setw(4) << dim << endl;

        for (int d=0; d<dim; d++) {
          assert(a != NULL);

          _out << setw(4) << " "
            << ".long " << right << setw(4) << a->GetNElem() << endl;

          a = dynamic_cast<const CArrayType*>(a->GetInnerType());
        }
      }

      const CDataInitializer *di = s->GetData();
      if (di != NULL) {
        const CDataInitString *sdi = dynamic_cast<const CDataInitString*>(di);
        assert(sdi != NULL);  // only support string data initializers for now

        _out << left << setw(4) << " "
          << ".asciz " << '"' << sdi->GetData() << '"' << endl;
      } else {
        _out  << left << setw(4) << " "
          << ".skip " << dec << right << setw(4) << t->GetDataSize()
          << endl;
      }

      size += t->GetSize();
    }
  }

  _out << endl;

  // emit globals in subscopes (necessary if we support static local variables)
  vector<CScope*>::const_iterator sit = scope->GetSubscopes().begin();
  while (sit != scope->GetSubscopes().end()) EmitGlobalData(*sit++);
}

void CBackendx86::EmitLocalData(CScope *scope)
{
  assert(scope != NULL);
  vector<CSymbol*> slist = scope->GetSymbolTable()->GetSymbols();

  for (CSymbol* s : slist) {
    if (s->GetSymbolType() != stLocal) continue;
    if (! s->GetDataType()->IsArray()) continue;

    CSymLocal* localSym = dynamic_cast<CSymLocal*>(s);

    const CArrayType* arrayType = dynamic_cast<const CArrayType*> (localSym->GetDataType());
    int offset = localSym->GetOffset();
    string reg = localSym->GetBaseRegister();

    EmitInstruction("movl", Imm(arrayType->GetNDim()) + ", " + to_string(offset) + "("+reg+")", "Local Array " + s->GetName() + "'s dimension");

    int dimCnt = 1;

    while(arrayType != NULL) {
      offset += 4;
      EmitInstruction("movl", Imm(arrayType->GetNElem()) + ", " + to_string(offset) + "("+reg+")", "    dimension " + to_string(dimCnt));
      arrayType = dynamic_cast<const CArrayType*>( arrayType->GetInnerType());
      dimCnt++;
    }
  }

}

void CBackendx86::EmitCodeBlock(CCodeBlock *cb)
{
  assert(cb != NULL);

  const list<CTacInstr*> &instr = cb->GetInstr();
  list<CTacInstr*>::const_iterator it = instr.begin();

  while (it != instr.end()) EmitInstruction(*it++);
}

void CBackendx86::EmitInstruction(CTacInstr *i)
{
  assert(i != NULL);

  ostringstream cmt;
  string mnm;
  cmt << i;

  EOperation op = i->GetOperation();

  switch (op) {
    // binary operators
    // dst = src1 op src2
    case opAdd:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx");
      EmitInstruction("addl", "%ebx, %ebx");
      Store(i->GetDest(), 'a');
      break;
    case opSub:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx");
      EmitInstruction("subl", "%ebx, %ebx");
      Store(i->GetDest(), 'a');
      break;
    case opMul:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx");
      EmitInstruction("imull", "%ebx");
      Store(i->GetDest(), 'a');
      break;
    case opDiv:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx");
      EmitInstruction("idivl", "%ebx");
      Store(i->GetDest(), 'a');
      break;
    case opAnd:
      // never reached
      break;
    case opOr:
      // never reached
      break;

    // unary operators
    // dst = op src1
    case opNeg:
      Load(i->GetSrc(1), "%eax", cmt.str());
      EmitInstruction("negl", "%eax");
      Store(i->GetDest(), 'a');
      break;
    case opPos:
      EmitInstruction("# ???", "nothing to do", cmt.str());
      break;
    case opNot:
      // never reached
      break;

    // memory operations
    // dst = src1
    case opAssign:
      Load(i->GetSrc(1), "%eax", cmt.str());
      Store(i->GetDest(), 'a');
      break;

    // pointer operations
    // dst = &src1
    case opAddress:
      EmitInstruction("leal", Operand(i->GetSrc(1)) + ", %eax", cmt.str());
      Store(i->GetDest(), 'a');
      break;
    // dst = *src1
    case opDeref:
      // opDeref not generated for now
      EmitInstruction("# opDeref", "not implemented", cmt.str());
      break;

    // unconditional branching
    // goto dst
    case opGoto: {
      const CTacLabel *label = dynamic_cast<const CTacLabel*>(i->GetDest());
      assert(label != NULL);
      EmitInstruction("jmp", Label(label), cmt.str());
    } break;

    // conditional branching
    // if src1 relOp src2 then goto dst
    case opEqual:
    case opNotEqual:
    case opLessThan:
    case opLessEqual:
    case opBiggerThan:
    case opBiggerEqual: {
      Load(i->GetSrc(1), "%eax", cmt.str());
      Load(i->GetSrc(2), "%ebx");
      EmitInstruction("cmpl", "%ebx, %eax");
      const CTacLabel *label = dynamic_cast<const CTacLabel*>(i->GetDest());
      assert(label != NULL);
      EmitInstruction("j" + Condition(op), Label(label));
    } break;

    // function call-related operations
    case opCall: {
      EmitInstruction("call", Operand(i->GetSrc(1)), cmt.str());
      // call
      const CTacName *n = dynamic_cast<const CTacName*>(i->GetSrc(1));
      assert(n != NULL);
      const CSymProc *proc = dynamic_cast<const CSymProc*>(n->GetSymbol());
      assert(proc != NULL);
      EmitInstruction("addl", Imm(proc->GetNParams() * 4) + ", %esp");
      // restore stack pointer
      if (i->GetDest() != NULL)
        Store(i->GetDest(), 'a', "get return value");
      // if it has return value, store it to temp
    } break;
    case opReturn:
      Load(i->GetSrc(1), "%eax", cmt.str());
      EmitInstruction("jmp", Label("exit"));
      break;
    case opParam:
      Load(i->GetSrc(1), "%eax", cmt.str());
      EmitInstruction("pushl", "%eax");
      break;

    // special
    case opLabel:
      _out << Label(dynamic_cast<CTacLabel*>(i)) << ":" << endl;
      break;

    case opNop:
      EmitInstruction("nop", "", cmt.str());
      break;


    default:
      EmitInstruction("# ???", "not implemented", cmt.str());
  }
}

void CBackendx86::EmitInstruction(string mnemonic, string args, string comment)
{
  _out << left
       << _ind
       << setw(7) << mnemonic << " "
       << setw(23) << args;
  if (comment != "") _out << " # " << comment;
  _out << endl;
}

void CBackendx86::Load(CTacAddr *src, string dst, string comment)
{
  assert(src != NULL);

  string mnm = "mov";
  string mod = "l";

  // set operator modifier based on the operand size
  switch (OperandSize(src)) {
    case 1: mod = "zbl"; break;
    case 2: mod = "zwl"; break;
    case 4: mod = "l"; break;
  }

  // emit the load instruction
  EmitInstruction(mnm + mod, Operand(src) + ", " + dst, comment);
}

void CBackendx86::Store(CTac *dst, char src_base, string comment)
{
  assert(dst != NULL);

  string mnm = "mov";
  string mod = "l";
  string src = "%";

  // compose the source register name based on the operand size
  switch (OperandSize(dst)) {
    case 1: mod = "b"; src += string(1, src_base) + "l"; break;
    case 2: mod = "w"; src += string(1, src_base) + "x"; break;
    case 4: mod = "l"; src += "e" + string(1, src_base) + "x"; break;
  }

  // emit the store instruction
  EmitInstruction(mnm + mod, src + ", " + Operand(dst), comment);
}

string CBackendx86::Operand(const CTac *op)
{
  const CTacConst *constant = dynamic_cast<const CTacConst*>(op);
  if (constant != NULL){
    return Imm(constant->GetValue());
  }

  const CTacReference *reference = dynamic_cast<const CTacReference*>(op);
  if (reference != NULL){
    const CSymbol *symbol = reference->GetSymbol();
    EmitInstruction("movl", to_string(symbol->GetOffset()) + "(" + symbol->GetBaseRegister() + "), %edi");
    return "(%edi)";
  }

  const CTacName *name = dynamic_cast<const CTacName*>(op);
  if (name != NULL){
    const CSymbol *symbol = name->GetSymbol();
    switch (symbol->GetSymbolType()){
      case ESymbolType::stGlobal:
      case ESymbolType::stProcedure:
        return symbol->GetName();
      case ESymbolType::stLocal:
      case ESymbolType::stParam:
        return to_string(symbol->GetOffset()) + "(" + symbol->GetBaseRegister() +")";
    }
  }
  return "";
}

string CBackendx86::Imm(int value) const
{
  ostringstream o;
  o << "$" << dec << value;
  return o.str();
}

string CBackendx86::Label(const CTacLabel* label) const
{
  CScope *cs = GetScope();
  assert(cs != NULL);

  ostringstream o;
  o << "l_" << cs->GetName() << "_" << label->GetLabel();
  return o.str();
  return "l_" + cs->GetName() + "_" + label->GetLabel();
}

string CBackendx86::Label(string label) const
{
  CScope *cs = GetScope();
  assert(cs != NULL);

  return "l_" + cs->GetName() + "_" + label;
}

string CBackendx86::Condition(EOperation cond) const
{
  switch (cond) {
    case opEqual:       return "e";
    case opNotEqual:    return "ne";
    case opLessThan:    return "l";
    case opLessEqual:   return "le";
    case opBiggerThan:  return "g";
    case opBiggerEqual: return "ge";
    default:            assert(false); break;
  }
}

int CBackendx86::OperandSize(CTac *t) const
{
  const CType *type = NULL;
  if (dynamic_cast<CTacName*>(t) != NULL){
    if (dynamic_cast<CTacReference*>(t) != NULL){
      // CTacReference
      const CSymbol *deref_symbol = dynamic_cast<CTacReference*>(t)->GetDerefSymbol();
      if (deref_symbol->GetDataType()->IsPointer()) {
        const CPointerType* pointer_type = dynamic_cast<const CPointerType*>(deref_symbol->GetDataType());
        type = dynamic_cast<const CArrayType*>(pointer_type->GetBaseType());
      } else {
        type = dynamic_cast<const CArrayType*>(deref_symbol->GetDataType());
      }
      assert(type != NULL);
      while(type->IsArray()) {
        type = dynamic_cast<const CArrayType*>(type)->GetInnerType();
      }
    }
    else {
      const CSymbol *symbol = dynamic_cast<CTacName*>(t)->GetSymbol();
      type = symbol->GetDataType();
    }
  }
  CTypeManager* tm = CTypeManager::Get();
  if (type != NULL && (type->Match(tm->GetBool()) || type->Match(tm->GetChar()))){
    return 1;
  }
  else{
    return 4;
  }
}

size_t CBackendx86::ComputeStackOffsets(CSymtab *symtab,
                                        int param_ofs,int local_ofs)
{
  assert(symtab != NULL);
  vector<CSymbol*> slist = symtab->GetSymbols();
  int l_size = 0;

  _out << _ind << "# stack offsets:" << endl;
  for (CSymbol *s : slist){
    if (s->GetSymbolType() == ESymbolType::stLocal){
      s->SetBaseRegister("%ebp");
      const CType *type = s->GetDataType();
      l_size += type->GetSize();
      if (type->GetAlign() == 4 && l_size % 4 != 0){
        // need set align
        l_size += (4 - l_size % 4);
      }
      s->SetOffset(local_ofs - l_size);
    }
    if (s->GetSymbolType() == ESymbolType::stParam){
      s->SetBaseRegister("%ebp");
      assert(dynamic_cast<CSymParam*>(s) != NULL);
      s->SetOffset(param_ofs + 4 * dynamic_cast<CSymParam*>(s)->GetIndex());
    }

    if (s->GetSymbolType() == ESymbolType::stLocal || s->GetSymbolType() == ESymbolType::stParam)
      _out << _ind << "#" << " "
          << right << setw(6) << s->GetOffset() << "(" << s->GetBaseRegister() << ")" << " "
          << setw(3) << s->GetDataType()->GetSize() << "  "
          << "[" << " -"
          << left << setw(8) << s->GetName() << " " << s->GetDataType() << " " << "]" << endl;
  }
  return l_size;
}
