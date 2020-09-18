//===-- MCoreAsmPrinter.cpp - MCore LLVM assembly writer ------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains a printer that converts from our internal representation
// of machine-dependent LLVM code to GAS-format MCore assembly language.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/MCoreInstPrinter.h"
#include "TargetInfo/MCoreTargetInfo.h"
#include "MCore.h"
#include "MCoreInstrInfo.h"
#include "MCoreMCInstLower.h"
#include "MCoreSubtarget.h"
#include "MCoreTargetMachine.h"
#include "MCoreTargetStreamer.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Mangler.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbolELF.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetLoweringObjectFile.h"
#include <algorithm>
#include <cctype>
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

namespace {
  class MCoreAsmPrinter : public AsmPrinter {
    MCoreMCInstLower MCInstLowering;
    MCoreTargetStreamer &getTargetStreamer();

  public:
    explicit MCoreAsmPrinter(TargetMachine &TM,
                             std::unique_ptr<MCStreamer> Streamer)
        : AsmPrinter(TM, std::move(Streamer)), MCInstLowering(*this) {}

    StringRef getPassName() const override { return "MCore Assembly Printer"; }

    void printOperand(const MachineInstr *MI, int opNum, raw_ostream &O);
    bool PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                         const char *ExtraCode, raw_ostream &O) override;
    bool PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNum,
                               const char *ExtraCode, raw_ostream &O) override;

    void emitArrayBound(MCSymbol *Sym, const GlobalVariable *GV);
    void EmitGlobalVariable(const GlobalVariable *GV) override;

    void EmitFunctionEntryLabel() override;
    void EmitInstruction(const MachineInstr *MI) override;
    void EmitFunctionBodyStart() override;
    void EmitFunctionBodyEnd() override;
  };
} // end of anonymous namespace

MCoreTargetStreamer &MCoreAsmPrinter::getTargetStreamer() {
  return static_cast<MCoreTargetStreamer&>(*OutStreamer->getTargetStreamer());
}

void MCoreAsmPrinter::emitArrayBound(MCSymbol *Sym, const GlobalVariable *GV) {
}

void MCoreAsmPrinter::EmitGlobalVariable(const GlobalVariable *GV) {
  // Check to see if this is a special global used by LLVM, if so, emit it.
  if (!GV->hasInitializer() ||
      EmitSpecialLLVMGlobal(GV))
    return;

  const DataLayout &DL = getDataLayout();
  OutStreamer->SwitchSection(getObjFileLowering().SectionForGlobal(GV, TM));

  MCSymbol *GVSym = getSymbol(GV);
  const Constant *C = GV->getInitializer();
  unsigned Align = (unsigned)DL.getPreferredTypeAlignmentShift(C->getType());

  // Mark the start of the global
  getTargetStreamer().emitCCTopData(GVSym->getName());

  switch (GV->getLinkage()) {
  case GlobalValue::AppendingLinkage:
    report_fatal_error("AppendingLinkage is not supported by this target!");
  case GlobalValue::LinkOnceAnyLinkage:
  case GlobalValue::LinkOnceODRLinkage:
  case GlobalValue::WeakAnyLinkage:
  case GlobalValue::WeakODRLinkage:
  case GlobalValue::ExternalLinkage:
  case GlobalValue::CommonLinkage:
    emitArrayBound(GVSym, GV);
    OutStreamer->EmitSymbolAttribute(GVSym, MCSA_Global);

    if (GV->hasWeakLinkage() || GV->hasLinkOnceLinkage() ||
        GV->hasCommonLinkage())
      OutStreamer->EmitSymbolAttribute(GVSym, MCSA_Weak);
    LLVM_FALLTHROUGH;
  case GlobalValue::InternalLinkage:
  case GlobalValue::PrivateLinkage:
    break;
  default:
    llvm_unreachable("Unknown linkage type!");
  }

  EmitAlignment(Align > 2 ? Align : 2, GV);

  if (GV->isThreadLocal()) {
    report_fatal_error("TLS is not supported by this target!");
  }
  unsigned Size = DL.getTypeAllocSize(C->getType());
  if (MAI->hasDotTypeDotSizeDirective()) {
    OutStreamer->EmitSymbolAttribute(GVSym, MCSA_ELF_TypeObject);
    OutStreamer->emitELFSize(GVSym, MCConstantExpr::create(Size, OutContext));
  }
  OutStreamer->EmitLabel(GVSym);

  EmitGlobalConstant(DL, C);
  // The ABI requires that unsigned scalar types smaller than 32 bits
  // are padded to 32 bits.
  if (Size < 4)
    OutStreamer->EmitZeros(4 - Size);

  // Mark the end of the global
  getTargetStreamer().emitCCBottomData(GVSym->getName());
}

void MCoreAsmPrinter::EmitFunctionBodyStart() {
  MCInstLowering.Initialize(&MF->getContext());
}

/// EmitFunctionBodyEnd - Targets can override this to emit stuff after
/// the last basic block in the function.
void MCoreAsmPrinter::EmitFunctionBodyEnd() {
  // Emit function end directives
  getTargetStreamer().emitCCBottomFunction(CurrentFnSym->getName());
}

void MCoreAsmPrinter::EmitFunctionEntryLabel() {
  // Mark the start of the function
  getTargetStreamer().emitCCTopFunction(CurrentFnSym->getName());
  OutStreamer->EmitLabel(CurrentFnSym);
}

void MCoreAsmPrinter::printOperand(const MachineInstr *MI, int opNum,
                                   raw_ostream &O) {
  const DataLayout &DL = getDataLayout();
  const MachineOperand &MO = MI->getOperand(opNum);
  switch (MO.getType()) {
  case MachineOperand::MO_Register:
    O << MCoreInstPrinter::getRegisterName(MO.getReg());
    break;
  case MachineOperand::MO_Immediate:
    O << MO.getImm();
    break;
  case MachineOperand::MO_MachineBasicBlock:
    MO.getMBB()->getSymbol()->print(O, MAI);
    break;
  case MachineOperand::MO_GlobalAddress:
    PrintSymbolOperand(MO, O);
    break;
  case MachineOperand::MO_ConstantPoolIndex:
    O << DL.getPrivateGlobalPrefix() << "CPI" << getFunctionNumber() << '_'
      << MO.getIndex();
    break;
  case MachineOperand::MO_BlockAddress:
    GetBlockAddressSymbol(MO.getBlockAddress())->print(O, MAI);
    break;
  default:
    llvm_unreachable("not implemented");
  }
}

bool MCoreAsmPrinter::PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                                      const char *ExtraCode, raw_ostream &O) {
  // Print the operand if there is no operand modifier.
  if (!ExtraCode || !ExtraCode[0]) {
    printOperand(MI, OpNo, O);
    return false;
  }

  // Otherwise fallback on the default implementation.
  return AsmPrinter::PrintAsmOperand(MI, OpNo, ExtraCode, O);
}

bool MCoreAsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI,
                                            unsigned OpNum,
                                            const char *ExtraCode,
                                            raw_ostream &O) {
  if (ExtraCode && ExtraCode[0]) {
    return true; // Unknown modifier.
  }
  printOperand(MI, OpNum, O);
  O << '[';
  printOperand(MI, OpNum + 1, O);
  O << ']';
  return false;
}

void MCoreAsmPrinter::EmitInstruction(const MachineInstr *MI) {
  SmallString<128> Str;
  raw_svector_ostream O(Str);

  MCInst TmpInst;
  MCInstLowering.Lower(MI, TmpInst);

  EmitToStreamer(*OutStreamer, TmpInst);
}


// Force static initialization.
extern "C" void LLVMInitializeMCoreAsmPrinter() {
  RegisterAsmPrinter<MCoreAsmPrinter> X(getTheMCoreTarget());
}
