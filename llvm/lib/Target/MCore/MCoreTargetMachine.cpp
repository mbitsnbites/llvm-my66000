//===-- MCoreTargetMachine.cpp - Define TargetMachine for MCore -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "MCoreTargetMachine.h"
#include "MCTargetDesc/MCoreMCTargetDesc.h"
#include "TargetInfo/MCoreTargetInfo.h"
#include "MCore.h"
#include "MCoreTargetObjectFile.h"
#include "MCoreTargetTransformInfo.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

static Reloc::Model getEffectiveRelocModel(Optional<Reloc::Model> RM) {
  if (!RM.hasValue())
    return Reloc::Static;
  return *RM;
}

static CodeModel::Model
getEffectiveMCoreCodeModel(Optional<CodeModel::Model> CM) {
  if (CM) {
    if (*CM != CodeModel::Large)
      report_fatal_error("Target only supports CodeModel Small or Large");
    return *CM;
  }
  return CodeModel::Small;
}

/// Create an ILP32 architecture model
///
MCoreTargetMachine::MCoreTargetMachine(const Target &T, const Triple &TT,
				       StringRef CPU, StringRef FS,
				       const TargetOptions &Options,
				       Optional<Reloc::Model> RM,
				       Optional<CodeModel::Model> CM,
				       CodeGenOpt::Level OL, bool JIT)
    : LLVMTargetMachine(
	T, "E-S32-p:32:32-i8:8-i16:16-i32:32-i64:32-a:0:32",
	TT, CPU, FS, Options, getEffectiveRelocModel(RM),
        getEffectiveMCoreCodeModel(CM), OL),
      TLOF(llvm::make_unique<MCoreTargetObjectFile>()),
      Subtarget(TT, CPU, FS, *this) {
  initAsmInfo();
}

MCoreTargetMachine::~MCoreTargetMachine() = default;

namespace {

/// MCore Code Generator Pass Configuration Options.
class MCorePassConfig : public TargetPassConfig {
public:
  MCorePassConfig(MCoreTargetMachine &TM, PassManagerBase &PM)
    : TargetPassConfig(TM, PM) {}

  MCoreTargetMachine &getMCoreTargetMachine() const {
    return getTM<MCoreTargetMachine>();
  }

  bool addInstSelector() override;
};

} // end anonymous namespace

TargetPassConfig *MCoreTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new MCorePassConfig(*this, PM);
}

bool MCorePassConfig::addInstSelector() {
  addPass(createMCoreISelDag(getMCoreTargetMachine(), getOptLevel()));
  return false;
}

TargetTransformInfo
MCoreTargetMachine::getTargetTransformInfo(const Function &F) {
  return TargetTransformInfo(MCoreTTIImpl(this, F));
}

// Force static initialization.
extern "C" void LLVMInitializeMCoreTarget() {
  RegisterTargetMachine<MCoreTargetMachine> X(getTheMCoreTarget());
}
