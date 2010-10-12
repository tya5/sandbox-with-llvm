#include <map>
#include <string>
#include <iostream>

extern "C" {
#include <strings.h>
#include <stdlib.h>
}

#include "llvm/Target/TargetSelect.h"
#include "llvm/DerivedTypes.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/ValueSymbolTable.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/GenericValue.h"

/**
 * Language Specification
 *-----------------------------------------------------------------------------*
 * - support primitive types
 *  double string function symbol bool cons
 * - nil equals to void *
 * - true instead of symbol t
 * - false instead of symbol nil
 * - support special op
 *  progn if tagbody go block return-from let
 * - support lambda
 */

/**
 * LLVM API Tips
 *-----------------------------------------------------------------------------*
 * - LLVMContext
 * - Module
 *  -- グローバル変数のインターン (.getOrInsertGlobal(...))
 *     new GlobalVariable(...) を parent につなげてもシンボルテーブルには追加されないようだ
 * - ValueSymbolTable
 *  -- 名前からValueを取り出す (.lookup(StringRef name))
 * - BasicBlock
 *  -- インストラクションリストをもつ
 *  -- 変数の一つのスコープに対応するようだ (.getValueSymbolTable())
 *  -- スコープのネストの実現方法は？できない？
 *     progn / let
 *  -- スタック変数と親関数の仮引数の参照が可能
 *  -- 複数のBasicBlockをつなげてコントロールフローを生成可能
 * - IRBuilder
 *  -- 各インストラクションコードはllmv::IRBuilder<>のメソッドから生成する
 *  -- ポインタ先への代入は (.CreateStore(...))
 *  -- ローカル変数の宣言方法は？ (.CreateAlloca(...))
 * - Function
 *  -- スタック変数／グローバル変数に対応するValue*はポインタ型であるが
 *     仮引数Value*はポインタ型ではない　→　代入できない？
 *  -- 仮引数への名前参照はBasicBlockのValueSymbolTableから可能
 * - GlobalVariable
 *
 *
 *-----------------------------------------------------------------------------*
 * 
 */

using namespace llvm;
using namespace std;

class FunctionSexp;

class SexpCompilerContext
{
  LLVMContext llvm;
  Module toplevel;
  map<string, FunctionSexp *> fnctab;
  IRBuilder<> *builder;
  vector<IRBuilder<> *> builders;
  ExecutionEngine *engine;

private:
  SexpCompilerContext(const SexpCompilerContext&);
  SexpCompilerContext& operator=(const SexpCompilerContext&);

public:
  SexpCompilerContext();

  LLVMContext& getLLVMContext()
  { return llvm; }

  Value *lookupSymbol(const string&  name)
  {
    Value *sym = NULL;
    BasicBlock *bb = builder ? builder->GetInsertBlock(): NULL;

    if (bb && (sym = bb->getValueSymbolTable()->lookup(name)))
      return sym;
    
    return toplevel.getNamedGlobal(StringRef(name));
  }

  FunctionSexp *lookupFunction(const string& name)
  { return fnctab[name]; }

  FunctionSexp& addFunction(const string& name, FunctionSexp& func)
  { fnctab[name] = &func; return func; };

  Module& getToplevelModule()
  { return toplevel; }

  IRBuilder<> *getBuilder()
  { return builder; }

  IRBuilder<> *pushBuilder(BasicBlock *block)
  {
    builder = new IRBuilder<>(llvm);
    if (block)
      builder->SetInsertPoint(block);
    builders.push_back(builder);
    return builder;
  }

  IRBuilder<> *popBuilder()
  {
    builders.pop_back();
    delete builder;
    builder = builders.back();
    return builder;
  }

  const Type *getVoidTy()
  { return Type::getVoidTy(llvm); }

  const Type *getDoubleTy()
  { return Type::getDoubleTy(llvm); }

  GenericValue execute(Function *ep, vector<GenericValue>& args)
  {
    if (engine)
      return engine->runFunction(ep, args);

    GenericValue v;
    v.DoubleVal = 0;
    return v;
  }
};

class Sexp
{
public:
  virtual ~Sexp() {}

  virtual void dump() = 0;

  virtual Value *codegen(SexpCompilerContext &ctx) = 0;
};

class DoubleSexp : public Sexp
{
  double value;

public:
  DoubleSexp(double value): value(value) {}

  double getValue()
  { return value; }

  virtual void dump()
  { cout << value; }

  virtual Value *codegen(SexpCompilerContext &ctx)
  {
    return ConstantFP::get(ctx.getLLVMContext(), APFloat(value));
  }
};

class SymbolSexp : public Sexp
{
  const string name;

public:
  SymbolSexp(const string& name): name(name) {}

  const string& getName()
  { return name; }

  virtual void dump()
  { cout << name; }

  virtual Value *codegen(SexpCompilerContext &ctx)
  {
    Value *ptr = ctx.lookupSymbol(name);

    if (!ptr)
      return NULL;

    if (PointerType::classof(ptr->getType()))
      return ctx.getBuilder()->CreateLoad(ptr, name);

    return ptr;
  }
};

class ConsSexp : public Sexp
{
public:
  Sexp *car;
  Sexp *cdr;

  ConsSexp(Sexp *car, Sexp *cdr): car(car), cdr(cdr) {}

  virtual Value *codegen(SexpCompilerContext &ctx);

  virtual void dump()
  {
    cout << "(";
    if (car)
      car->dump();
    else
      cout << "NIL";
    cout << " . ";
    if (cdr)
      cdr->dump();
    else
      cout << "NIL";
    cout << ")";
  }

  Sexp *nth(int i)
  {
    ConsSexp *sexp = this;

    while (sexp && i--)
      sexp = dynamic_cast<ConsSexp *>(sexp->cdr);

    return sexp ? sexp->car: NULL;
  }

  Sexp *nthcdr(int i)
  {
    ConsSexp *sexp = this;

    while (sexp && i--)
      sexp = dynamic_cast<ConsSexp *>(sexp->cdr);

    return sexp;
  }

  int length()
  {
    ConsSexp *p = this;
    int len = 0;

    for (; p; p = dynamic_cast<ConsSexp *>(p->cdr))
      len++;

    return len;
  }
};

class FunctionSexp : public Sexp
{
protected:
  const string name;
  
public:
  FunctionSexp(const string& name): name(name) {}

  virtual void dump()
  { cout << "<function " << name << ">"; }

  virtual Value *codegen(SexpCompilerContext& ctx)
  { return NULL; }

  const string& getName()
  { return name; }

  virtual Value *codegen_call(ConsSexp *args, SexpCompilerContext &ctx)
  {
    Function *ff = ctx.getToplevelModule().getFunction(name);

    if (ff->arg_size() != args->length())
      return NULL;

    vector<Value*> argsv;
    for (int i = 0, last = ff->arg_size(); i < last; i++) {
      argsv.push_back(args->nth(i)->codegen(ctx));
      if (argsv.back() == 0) return NULL;
    }
    
    return ctx.getBuilder()->CreateCall(ff, argsv.begin(), argsv.end(), "calltmp");
  }
};

class UserFunctionSexp : public FunctionSexp
{
  ConsSexp *args;
  ConsSexp *body;

public:
  UserFunctionSexp(const string& name, ConsSexp *args, ConsSexp *body)
    : FunctionSexp(name), args(args), body(body) {}

  virtual Value *codegen(SexpCompilerContext& ctx);
};

class AddSexp : public FunctionSexp
{
  AddSexp() : FunctionSexp("+") {}

public:
  static AddSexp& get()
  {
    static AddSexp instance;
    return instance;
  }

  virtual Value *codegen_call(ConsSexp *args, SexpCompilerContext& ctx)
  {
    if (!args)
      return NULL;
    
    Value *ret = args->car->codegen(ctx);

    while (args = dynamic_cast<ConsSexp*>(args->cdr))
      ret = ctx.getBuilder()->CreateFAdd(ret, args->car->codegen(ctx));

    return ret;
  }
};

class SubSexp : public FunctionSexp
{
  SubSexp() : FunctionSexp("-") {}

public:
  static SubSexp& get()
  {
    static SubSexp instance;
    return instance;
  }

  virtual Value *codegen_call(ConsSexp *args, SexpCompilerContext &ctx)
  {
    if (!args)
      return NULL;

    Value *ret = args->car->codegen(ctx);

    while (args = dynamic_cast<ConsSexp*>(args->cdr))
      ret = ctx.getBuilder()->CreateFSub(ret, args->car->codegen(ctx));

    return ret;
  }
};

class MulSexp : public FunctionSexp
{
  MulSexp() : FunctionSexp("*") {}

public:
  static MulSexp& get()
  {
    static MulSexp instance;
    return instance;
  }

  virtual Value *codegen_call(ConsSexp *args, SexpCompilerContext& ctx)
  {
    if (!args)
      return NULL;

    Value *ret = args->car->codegen(ctx);

    while (args = dynamic_cast<ConsSexp*>(args->cdr))
      ret = ctx.getBuilder()->CreateFMul(ret, args->car->codegen(ctx));

    return ret;
  }
};

class InternSexp : public FunctionSexp
{
  InternSexp() : FunctionSexp("INTERN") {}

public:
  static InternSexp& get()
  {
    static InternSexp instance;
    return instance;
  }

  virtual Value *codegen_call(ConsSexp *args, SexpCompilerContext& ctx)
  {
    SymbolSexp *sym;
    
    if (args && (sym = dynamic_cast<SymbolSexp*>(args->car)))
      return ctx.getToplevelModule().getOrInsertGlobal(sym->getName(), ctx.getDoubleTy());

    return NULL;
  }
};

class SetqSexp : public FunctionSexp
{
  SetqSexp() : FunctionSexp("SETQ") {}

public:
  static SetqSexp& get()
  {
    static SetqSexp instance;
    return instance;
  }

  virtual Value *codegen_call(ConsSexp *args, SexpCompilerContext &ctx)
  {
    SymbolSexp *sym = dynamic_cast<SymbolSexp*>(args->nth(0));
    Value *variable;
    Value *value;

    if (!sym)
      return NULL;

    variable = ctx.lookupSymbol(sym->getName());
    value = args->nth(1)->codegen(ctx);

    if (!variable) {
      variable = ctx.getToplevelModule().getOrInsertGlobal(sym->getName(), value->getType());
    }

    if (!variable)
      return NULL;

    if (!PointerType::classof(variable->getType()))
      return NULL; // prohibit to set some value to arg field.

    return ctx.getBuilder()->CreateStore(value, variable);
  }
};

class DefunSexp : public FunctionSexp
{
  DefunSexp() : FunctionSexp("DEFUN") {}

public:
  static DefunSexp& get()
  {
    static DefunSexp instance;
    return instance;
  }

  virtual Value *codegen_call(ConsSexp *args, SexpCompilerContext& ctx)
  {
    SymbolSexp *xname = dynamic_cast<SymbolSexp*>(args->nth(0));
    ConsSexp *xargs = dynamic_cast<ConsSexp*>(args->nth(1));
    ConsSexp *xbody = dynamic_cast<ConsSexp*>(args->nthcdr(2));

    if (!xname)
      return NULL;

    return ctx.addFunction(xname->getName(),
                           * new UserFunctionSexp(xname->getName(), xargs, xbody)).codegen(ctx);
  }
};

class IfSexp: public FunctionSexp
{
  IfSexp(): FunctionSexp("IF") {}

public:
  static IfSexp& get()
  {
    static IfSexp instance;
    return instance;
  }

  virtual Value *codegen_call(ConsSexp *args, SexpCompilerContext& ctx)
  {
    return NULL;
  }
};

class PrognSexp: public FunctionSexp
{
  PrognSexp(): FunctionSexp("PROGN") {}

public:
  static PrognSexp& get()
  {
    static PrognSexp instance;
    return instance;
  }

  virtual Value *codegen_call(ConsSexp *args, SexpCompilerContext& ctx)
  {
    Value *val = NULL;
    ConsSexp *cons;

    for (cons = args; cons; cons = dynamic_cast<ConsSexp*>(cons->cdr))
      if (cons->car)
        val = cons->car->codegen(ctx);

    return val;
  }
};

class LetSexp: public FunctionSexp
{
  LetSexp(): FunctionSexp("LET") {}

public:
  static LetSexp& get()
  {
    static LetSexp instance;
    return instance;
  }

  virtual Value *codegen_call(ConsSexp *args, SexpCompilerContext& ctx)
  {
    if (!args)
      return NULL;

    string name("");
      
    if (SymbolSexp *sym = dynamic_cast<SymbolSexp*>(args->car))
      name = sym->getName();
      
    return ctx.getBuilder()->CreateAlloca(ctx.getDoubleTy(), 0, name);
  }
};

class ExecSexp: public FunctionSexp
{
  ExecSexp(): FunctionSexp("EXEC") {}

public:
  static ExecSexp& get()
  {
    static ExecSexp instance;
    return instance;
  }

  virtual Value *codegen_call(ConsSexp *args, SexpCompilerContext& ctx)
  {
    if (!args)
      return NULL;

    if (SymbolSexp *sym = dynamic_cast<SymbolSexp*>(args->car)) {
      ConsSexp *cons = dynamic_cast<ConsSexp*>(args->cdr);
      vector<GenericValue> fargs;
      
      for (; cons; cons = dynamic_cast<ConsSexp*>(cons->cdr)) {
        if (DoubleSexp *num = dynamic_cast<DoubleSexp*>(cons->car)) {
          GenericValue v;
          v.DoubleVal = num->getValue();
          fargs.push_back(v);
        } else
          return NULL;
      }
        
      return ConstantFP::get(ctx.getLLVMContext(),
                             APFloat(ctx.execute(ctx.getToplevelModule().getFunction(sym->getName()), fargs).DoubleVal));
    }

    return NULL;
  }
};

Value *UserFunctionSexp::codegen(SexpCompilerContext& ctx)
{
  vector<const Type*> doubles(args ? args->length(): 0, ctx.getDoubleTy());
  FunctionType *type = FunctionType::get(ctx.getDoubleTy(), doubles, false);
  Function *func = Function::Create(type, Function::ExternalLinkage, name, &ctx.getToplevelModule());

  if (args) {
    Function::arg_iterator it = func->arg_begin();
    ConsSexp *cons = args;
    SymbolSexp *sym;

    while (cons && (sym = dynamic_cast<SymbolSexp*>(cons->car))) {
      it->setName(sym->getName());
      cons = dynamic_cast<ConsSexp*>(cons->cdr);
      ++it;
    }
  }

  if (body) {
    BasicBlock *block = BasicBlock::Create(ctx.getLLVMContext(), "entry", func);
    Value *val = NULL;
    IRBuilder<> *builder = ctx.pushBuilder(block);

    for (ConsSexp *cons = body; cons; cons = dynamic_cast<ConsSexp*>(cons->cdr)) {
      if (cons->car)
        val = cons->car->codegen(ctx);
    }

    if (val)
      builder->CreateRet(val);
    
    ctx.popBuilder();
  }

  return func;
}

Value *ConsSexp::codegen(SexpCompilerContext &ctx)
{
  ConsSexp *ccdr = dynamic_cast<ConsSexp *>(cdr);
    
  if (SymbolSexp *funSym = dynamic_cast<SymbolSexp *>(car)) {
    if (FunctionSexp *fun = ctx.lookupFunction(funSym->getName()))
      return fun->codegen_call(ccdr, ctx);
  }

  return NULL;
}

SexpCompilerContext::SexpCompilerContext()
  : llvm(), builder(NULL), toplevel("TopLevel", llvm)
{
  FunctionSexp *prim[] = {
    &SetqSexp::get(), &AddSexp::get(), &SubSexp::get(),
    &MulSexp::get(), &DefunSexp::get(), &InternSexp::get(),
    &IfSexp::get(), &LetSexp::get(), &PrognSexp::get(),
    &ExecSexp::get(),
    NULL};

  for (int i = 0; prim[i]; i++)
    fnctab[prim[i]->getName()] = prim[i];

  string err;
  engine = EngineBuilder(&toplevel).setErrorStr(&err).create();
  if (!engine)
    cout << err << endl;
}

/**
 * Lexer
 */

enum Token {
  TokNull,
  TokEOF,
  TokChar,
  TokIdentifier, // [A-Za-z]
  TokDouble,
};

static const char *TokChars = "()";

class SexpLexer {
  Token currentTok;
  string identifier;
  double number;

protected:
  char currentChar;
  virtual char nextchar() = 0;
public:
  SexpLexer();
  virtual ~SexpLexer() {}
  double getNumber() { return number; }
  const string& getIdentifier() { return identifier; }
  void reset();
  Token currentToken();
  void eatToken();
  Token nextToken();
  void dumpCurrentToken();
};

class StdinSexpLexer : public SexpLexer {
public:
  StdinSexpLexer() {}
private:
  StdinSexpLexer(const StdinSexpLexer&);
  StdinSexpLexer& operator=(const StdinSexpLexer&);
protected:
  virtual char nextchar() {
    currentChar = getchar();
    return currentChar;
  }
};

SexpLexer::SexpLexer(): currentTok(TokNull)
{
}

void SexpLexer::reset()
{
  nextchar();
}

Token SexpLexer::currentToken()
{
  return currentTok == TokNull ? nextToken(): currentTok;
}

void SexpLexer::eatToken()
{
  if (currentTok != TokEOF)
    currentTok = TokNull;
}

Token SexpLexer::nextToken()
{
  while (currentChar != EOF && isspace(currentChar))
    nextchar();

  if (currentChar == EOF) {
    currentTok = TokEOF;
    return TokEOF;
  }

  if (index(TokChars, currentChar)) {
    identifier = currentChar;
    currentTok = TokChar;
    nextchar();
    return TokChar;
  }

  identifier = "";

  while (currentChar != EOF && !isspace(currentChar) && !index(TokChars, currentChar)) {
    identifier += currentChar;
    nextchar();
  }

  const char *startp = identifier.c_str();
  char *endp = NULL;
  number = strtod(startp, &endp);

  currentTok = (endp - startp == strlen(startp)) ? TokDouble: TokIdentifier;
  return currentTok;
}

void SexpLexer::dumpCurrentToken()
{
  switch (currentTok) {
    case TokEOF:
      cout << "TokEOF";
      break;
    case TokChar:
      cout << "TokChar(" << identifier << ")" << endl;
      break;
    case TokIdentifier:
      cout << "TokIdent(" << identifier << ")" << endl;
      break;
    case TokDouble:
      cout << "TokDouble(" << number << ")" << endl;
      break;
  }
}

/**
 * Lexer Test
 */

int test_lexer()
{
  StdinSexpLexer lex;

  lex.reset();

  while (lex.currentToken() != TokEOF) {
    lex.dumpCurrentToken();
    lex.eatToken();
  }

  return 0;
}

/**
 * Parser (Parse and construct Sexp)
 */

Sexp *next_sexp(SexpLexer &lex)
{
  Sexp *sexp = NULL;
  Token tok = lex.currentToken();

  switch (tok) {
    case TokDouble:
      sexp = new DoubleSexp(lex.getNumber());
      lex.eatToken();
      return sexp;
    case TokIdentifier:
      sexp = new SymbolSexp(lex.getIdentifier());
      lex.eatToken();
      return sexp;
    case TokChar:
      switch (lex.getIdentifier().c_str()[0]) {
        case '(':
        {
          ConsSexp *sexp = NULL, **sexpp = &sexp;
          lex.eatToken();
          while (lex.currentToken() != TokEOF && lex.getIdentifier() != ")") {
            *sexpp = new ConsSexp(next_sexp(lex), NULL);
            sexpp = reinterpret_cast<ConsSexp**>(&(*sexpp)->cdr);
          }

          lex.eatToken();
          return sexp;
        }
        default:
          return NULL;
      }
  }

  return NULL;
}

int test_parser()
{
  StdinSexpLexer lex;

  lex.reset();

  while (Sexp *sexp = next_sexp(lex)) {
    sexp->dump();
    cout << endl;
  }

  return 0;
}

/**
 * Compile to LLVM IR
 */

int test_llvmir()
{
  if (!InitializeNativeTarget()) {
    cout << "LLVM InitializeNativeTarget() -> false" << endl;
  }
  
  SexpCompilerContext ctx;
  StdinSexpLexer lex;

  /**
   * Create Function 'void main()'
   */
  vector<const Type*> typev(0, ctx.getVoidTy());
  FunctionType *ftype = FunctionType::get(ctx.getVoidTy(), typev, false);
  Function *fmain = Function::Create(ftype, Function::ExternalLinkage, "main", &ctx.getToplevelModule());
  BasicBlock *fbody = BasicBlock::Create(ctx.getLLVMContext(), "entry", fmain);
  ctx.pushBuilder(fbody);

  /**
   * Prepare Lexer
   */
  lex.reset();

  /**
   * main loop
   */
  while (Sexp *sexp = next_sexp(lex)) {
    if (Value *val = sexp->codegen(ctx))
      val->dump();
  }

  ctx.getToplevelModule().dump();

  return 0;
}

/**
 * main()
 */

int main(int argc, char **argv)
{
  return test_llvmir();
}
