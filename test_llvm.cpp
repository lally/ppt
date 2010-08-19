#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/LinkAllVMCore.h"
#include "llvm/Constants.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/Target/TargetSelect.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/STLExtras.h"
#include <iostream>
#include <fstream>
#include <string.h>

using namespace llvm;


int main(int args, char ** argv) {
  cl::ParseCommandLineOptions(args, argv, " perftrace generator\n");

  LLVMContext& ctx = getGlobalContext();

  raw_ostream *out = &outs();

  Module * mod = new Module("perftrace_reader", ctx);
  Constant * puts = mod->getOrInsertFunction("puts",
					     IntegerType::getInt32Ty(ctx),
					     PointerType::getUnqual(
					       IntegerType::getInt8Ty(ctx)), NULL);

  std::vector<const Type *> printf_args(1, PointerType::getUnqual(IntegerType::getInt8Ty(ctx)));
  FunctionType *printf_type = FunctionType::get(IntegerType::getInt32Ty(ctx),
						 printf_args,
						 true);
  Constant * printf = mod->getOrInsertFunction("printf", printf_type);
					       
					       

  Function * _main = 
    cast<Function>(mod->getOrInsertFunction("main", 
					    IntegerType::getInt32Ty(ctx),
					    IntegerType::getInt32Ty(ctx),
					    PointerType::getUnqual(
					      PointerType::getUnqual(
						IntegerType::getInt8Ty(ctx))), NULL));
  Function::arg_iterator arrrgs = _main->arg_begin();
  arrrgs++->setName("argc");
  arrrgs++->setName("argv");

  BasicBlock *bb = BasicBlock::Create(ctx, "main.0", _main);
  Constant *msg = ConstantArray::get(ctx, "hello world! %s", true);

  GlobalVariable *glMsg = new GlobalVariable(*mod, msg->getType(), true, 
					     GlobalValue::InternalLinkage, msg, "hello_msg");
					     

  Constant *z32 = Constant::getNullValue(IntegerType::getInt32Ty(ctx));
  Constant *get_param[] = { z32, z32 };
  Constant *msgptr = ConstantExpr::getGetElementPtr(glMsg, get_param, array_lengthof(get_param));

  Value * argvec[] = { msgptr, msgptr };
  CallInst *puts_call = CallInst::Create(printf, argvec, array_endof(argvec), "", bb);
  puts_call->setTailCall(false);
  ReturnInst::Create(ctx, ConstantInt::get(ctx, APInt(32, 0)), bb);

  WriteBitcodeToFile(mod, *out);

  delete mod;
  llvm_shutdown();

  return 0;
}
