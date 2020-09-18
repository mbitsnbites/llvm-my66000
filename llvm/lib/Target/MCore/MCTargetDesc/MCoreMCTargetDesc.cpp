//===-- MCoreMCTargetDesc.cpp - MCore Target Descriptions -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides MCore specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/MCoreMCTargetDesc.h"
#include "MCTargetDesc/MCoreInstPrinter.h"
#include "MCTargetDesc/MCoreMCAsmInfo.h"
#include "TargetInfo/MCoreTargetInfo.h"
#include "MCoreTargetStreamer.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/MC/MCDwarf.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define GET_INSTRINFO_MC_DESC
#include "MCoreGenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "MCoreGenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "MCoreGenRegisterInfo.inc"

static MCInstrInfo *createMCoreMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitMCoreMCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createMCoreMCRegisterInfo(const Triple &TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitMCoreMCRegisterInfo(X, MCore::R15);	// FIXME
  return X;
}

static MCSubtargetInfo *
createMCoreMCSubtargetInfo(const Triple &TT, StringRef CPU, StringRef FS) {
  return createMCoreMCSubtargetInfoImpl(TT, CPU, FS);
}

static MCAsmInfo *createMCoreMCAsmInfo(const MCRegisterInfo &MRI,
                                       const Triple &TT) {
  MCAsmInfo *MAI = new MCoreMCAsmInfo(TT);

  // Initial state of the frame pointer is SP.
  MCCFIInstruction Inst = MCCFIInstruction::createDefCfa(nullptr, MCore::SP, 0);
  MAI->addInitialFrameState(Inst);

  return MAI;
}

static MCInstPrinter *createMCoreMCInstPrinter(const Triple &T,
						 unsigned SyntaxVariant,
						 const MCAsmInfo &MAI,
						 const MCInstrInfo &MII,
						 const MCRegisterInfo &MRI) {
  return new MCoreInstPrinter(MAI, MII, MRI);
}

MCoreTargetStreamer::MCoreTargetStreamer(MCStreamer &S) : MCTargetStreamer(S) {}

MCoreTargetStreamer::~MCoreTargetStreamer() = default;

namespace {

class MCoreTargetAsmStreamer : public MCoreTargetStreamer {
  formatted_raw_ostream &OS;

public:
  MCoreTargetAsmStreamer(MCStreamer &S, formatted_raw_ostream &OS);

  void emitCCTopData(StringRef Name) override;
  void emitCCTopFunction(StringRef Name) override;
  void emitCCBottomData(StringRef Name) override;
  void emitCCBottomFunction(StringRef Name) override;
};

} // end anonymous namespace

MCoreTargetAsmStreamer::MCoreTargetAsmStreamer(MCStreamer &S,
                                               formatted_raw_ostream &OS)
    : MCoreTargetStreamer(S), OS(OS) {}

void MCoreTargetAsmStreamer::emitCCTopData(StringRef Name) {
//  OS << "\t.cc_top " << Name << ".data," << Name << '\n';
}

void MCoreTargetAsmStreamer::emitCCTopFunction(StringRef Name) {
//  OS << "\t.cc_top " << Name << ".function," << Name << '\n';
}

void MCoreTargetAsmStreamer::emitCCBottomData(StringRef Name) {
//  OS << "\t.cc_bottom " << Name << ".data\n";
}

void MCoreTargetAsmStreamer::emitCCBottomFunction(StringRef Name) {
//  OS << "\t.cc_bottom " << Name << ".function\n";
}

static MCTargetStreamer *createTargetAsmStreamer(MCStreamer &S,
                                                 formatted_raw_ostream &OS,
                                                 MCInstPrinter *InstPrint,
                                                 bool isVerboseAsm) {
  return new MCoreTargetAsmStreamer(S, OS);
}

// Force static initialization.
extern "C" void LLVMInitializeMCoreTargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfoFn X(getTheMCoreTarget(), createMCoreMCAsmInfo);

  // Register the MC instruction info.
  TargetRegistry::RegisterMCInstrInfo(getTheMCoreTarget(),
                                      createMCoreMCInstrInfo);

  // Register the MC register info.
  TargetRegistry::RegisterMCRegInfo(getTheMCoreTarget(),
                                    createMCoreMCRegisterInfo);

  // Register the MC subtarget info.
  TargetRegistry::RegisterMCSubtargetInfo(getTheMCoreTarget(),
                                          createMCoreMCSubtargetInfo);

  // Register the MCInstPrinter
  TargetRegistry::RegisterMCInstPrinter(getTheMCoreTarget(),
                                        createMCoreMCInstPrinter);

  TargetRegistry::RegisterAsmTargetStreamer(getTheMCoreTarget(),
                                            createTargetAsmStreamer);
}
