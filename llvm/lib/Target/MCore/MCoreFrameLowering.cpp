//===-- MCoreFrameLowering.cpp - Frame info for MCore Target --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains MCore frame information that doesn't fit anywhere else
// cleanly...
//
//===----------------------------------------------------------------------===//

#include "MCoreFrameLowering.h"
#include "MCore.h"
#include "MCoreInstrInfo.h"
#include "MCoreMachineFunctionInfo.h"
#include "MCoreSubtarget.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetOptions.h"
#include <algorithm> // std::sort

using namespace llvm;

#define DEBUG_TYPE "my66000-framelower"

static const unsigned FPReg = MCore::R14;
static const unsigned SPReg = MCore::SP;

bool MCoreFrameLowering::hasFP(const MachineFunction &MF) const {
// needsStackRealignment(MF) || isFrameAddressTaken() ???
  return MF.getTarget().Options.DisableFramePointerElim(MF) ||
         MF.getFrameInfo().hasVarSizedObjects();
}

// Determines the size of the frame and maximum call frame size.
void MCoreFrameLowering::determineFrameLayout(MachineFunction &MF) const {
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const MCoreRegisterInfo *RI = STI.getRegisterInfo();
LLVM_DEBUG(dbgs() << "MCoreFrameLowering::determineFrameLayout\n");

  // Get the number of bytes to allocate from the FrameInfo.
  uint64_t FrameSize = MFI.getStackSize();

  // Get the alignment.
  unsigned StackAlign = getStackAlignment();
  if (RI->needsStackRealignment(MF)) {
LLVM_DEBUG(dbgs() << "needsStackRealignment: was=" << StackAlign <<
" max=" << MFI.getMaxAlignment() << '\n');
    unsigned MaxStackAlign = std::max(StackAlign, MFI.getMaxAlignment());
    FrameSize += (MaxStackAlign - StackAlign);
    StackAlign = MaxStackAlign;
  }

  // Set Max Call Frame Size
  uint64_t MaxCallSize = alignTo(MFI.getMaxCallFrameSize(), StackAlign);
  MFI.setMaxCallFrameSize(MaxCallSize);

  // Make sure the frame is aligned.
  FrameSize = alignTo(FrameSize, StackAlign);

  // Update frame info.
  MFI.setStackSize(FrameSize);
}

void MCoreFrameLowering::adjustReg(MachineBasicBlock &MBB,
                                   MachineBasicBlock::iterator MBBI,
                                   const DebugLoc &DL, unsigned DstReg,
                                   unsigned SrcReg, int64_t Val,
                                   MachineInstr::MIFlag Flag) const {
  const MCoreInstrInfo *TII = STI.getInstrInfo();
LLVM_DEBUG(dbgs() << "MCoreFrameLowering::adjustReg: " << Val << "\n");

  if (DstReg == SrcReg && Val == 0)
    return;
  bool isNeg = false;
  if (Val < 0) {
    isNeg = true;
    Val =-Val;
  }
  if (Val <= 64) {
    // issue multiple addi/subi to SP
    int n;
    while (Val > 0) {
      n = Val;
      if (n > 32) n = 32;
      BuildMI(MBB, MBBI, DL, TII->get(isNeg?MCore::SUBi:MCore::ADDi), DstReg)
          .addReg(SrcReg)
          .addImm(n)
          .setMIFlag(Flag);
      Val -= n;
    }
  } else {
    TII->buildConstant(MBB, MBBI, DL, Val, MCore::R1, Flag);
    BuildMI(MBB, MBBI, DL, TII->get(isNeg?MCore::SUB:MCore::ADD), DstReg)
	.addReg(DstReg)
	.addReg(MCore::R1, RegState::Kill)
	.setMIFlag(Flag);
  }
}

void MCoreFrameLowering::emitPrologue(MachineFunction &MF,
                                      MachineBasicBlock &MBB) const {
LLVM_DEBUG(dbgs() << "MCoreFrameLowering::emitPrologue\n");
  assert(&MF.front() == &MBB && "Shrink-wrapping not yet supported");

  MachineFrameInfo &MFI = MF.getFrameInfo();
  const MCoreRegisterInfo *RI = STI.getRegisterInfo();
  const MCoreInstrInfo *TII = STI.getInstrInfo();
  MachineBasicBlock::iterator MBBI = MBB.begin();

if (RI->needsStackRealignment(MF)) { dbgs() << "needsStackRealignment\n"; }
  if (RI->needsStackRealignment(MF) && MFI.hasVarSizedObjects()) {
    report_fatal_error(
        "MCore backend can't currently handle functions that need stack "
        "realignment and have variable sized objects");
  }

  // Debug location must be unknown since the first debug location is used
  // to determine the end of the prologue.
  DebugLoc DL;

  // Determine the correct frame layout
  determineFrameLayout(MF);

  // FIXME (note copied from Lanai): This appears to be overallocating.  Needs
  // investigation. Get the number of bytes to allocate from the FrameInfo.
  uint64_t StackSize = MFI.getStackSize();

  // Early exit if there is no need to allocate on the stack
  if (StackSize == 0 && !MFI.adjustsStack())
    return;
LLVM_DEBUG(dbgs() << "initial StackSize=" << StackSize << '\n');
  // Emit ".cfi_def_cfa_offset StackSize"
  unsigned CFIIndex = MF.addFrameInst(
      MCCFIInstruction::createDefCfaOffset(nullptr, -StackSize));
  BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex);

  // Iterate over list of callee-saved registers and emit .cfi_offset
  // directives.
  const std::vector<CalleeSavedInfo> &CSI = MFI.getCalleeSavedInfo();
  int64_t NSave = 0;
  unsigned HiReg = 0;
  unsigned LoReg = MCore::R15;
  int64_t Offset;;
  for (const auto &Entry : CSI) {
    Offset = MFI.getObjectOffset(Entry.getFrameIdx());
    unsigned Reg = Entry.getReg();
    unsigned CFIIndex = MF.addFrameInst(MCCFIInstruction::createOffset(
        nullptr, RI->getDwarfRegNum(Reg, true), Offset));
    BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
        .addCFIIndex(CFIIndex);
    if (Reg > HiReg) { HiReg = Reg; }
    if (Reg < LoReg) { LoReg = Reg; }
    NSave += 1;
  }
  MCoreFunctionInfo *XFI = MF.getInfo<MCoreFunctionInfo>();
  XFI->setLoSavedReg(LoReg);	// save for epilogue
  XFI->setHiSavedReg(HiReg);

  if (NSave) {
LLVM_DEBUG(dbgs() << "HiReg=" << HiReg << " LoReg=" << LoReg
<< " NSave=" << NSave << '\n');
    if (NSave > 1 && HiReg != MCore::R15)
	NSave += 1;	// must save R15 in STM
    // adjust stack for saved registers
    adjustReg(MBB, MBBI, DL, SPReg, SPReg, -(NSave*4), MachineInstr::FrameSetup);
    StackSize -= (NSave*4);
    bool isLive = MBB.isLiveIn(LoReg);
    if (NSave == 1) {
      BuildMI(MBB, MBBI, DL, TII->get(MCore::STW))
	      .addReg(LoReg, RegState::Define)
	      .addReg(SPReg).addImm(0);
    }
    else {
      BuildMI(MBB, MBBI, DL, TII->get(MCore::STM))
	     .addReg(LoReg, getKillRegState(!isLive));
    }
    if (!isLive)
      MBB.addLiveIn(LoReg);
  }
/*
  // Generate new FP.
  if (hasFP(MF)) {
    // copy unadjusted SP to FP
    adjustReg(MBB, MBBI, DL, FPReg, SPReg, StackSize, MachineInstr::FrameSetup);
  }
*/
LLVM_DEBUG(dbgs() << "final StackSize=" << StackSize << '\n');
  // Allocate space on the stack for locals, if necessary.
  if (StackSize != 0)
    adjustReg(MBB, MBBI, DL, SPReg, SPReg, -StackSize, MachineInstr::FrameSetup);
}

void MCoreFrameLowering::emitEpilogue(MachineFunction &MF,
                                     MachineBasicBlock &MBB) const {
LLVM_DEBUG(dbgs() << "MCoreFrameLowering::emitEpilogue\n");
  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  const MCoreRegisterInfo *RI = STI.getRegisterInfo();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  DebugLoc DL = MBBI->getDebugLoc();
  const MCoreInstrInfo *TII = STI.getInstrInfo();

  uint64_t StackSize = MFI.getStackSize();

  // Restore the stack pointer using the value of the frame pointer. Only
  // necessary if the stack pointer was modified, meaning the stack size is
  // unknown.
  if (RI->needsStackRealignment(MF) || MFI.hasVarSizedObjects()) {
//    assert(hasFP(MF) && "frame pointer should not have been eliminated");
if (RI->needsStackRealignment(MF))
  dbgs() << "No FP but needsStackRealignment\n";
if (MFI.hasVarSizedObjects())
  dbgs() << "No FP but hasVarSizedObjects\n";
    auto *XFI = MF.getInfo<MCoreFunctionInfo>();
    uint64_t FPOffset = StackSize - XFI->getVarArgsSaveSize();
dbgs() << "Epilogue needs FP to recover SP: " << FPOffset << "\n";
//    adjustReg(MBB, MBBI, DL, SPReg, FPReg, -FPOffset,
//	      MachineInstr::FrameDestroy);
  }
/*
  if (hasFP(MF)) {
    // To find the instruction restoring FP from stack.
    for (auto &I = LastFrameDestroy; I != MBBI; ++I) {
      if (I->mayLoad() && I->getOperand(0).isReg()) {
        unsigned DestReg = I->getOperand(0).getReg();
        if (DestReg == FPReg) {
          // If there is frame pointer, after restoring $fp registers, we
          // need adjust CFA to ($sp - FPOffset).
          // Emit ".cfi_def_cfa $sp, -FPOffset"
          unsigned CFIIndex = MF.addFrameInst(MCCFIInstruction::createDefCfa(
              nullptr, RI->getDwarfRegNum(SPReg, true), -FPOffset));
          BuildMI(MBB, std::next(I), DL,
                  TII->get(TargetOpcode::CFI_INSTRUCTION))
              .addCFIIndex(CFIIndex);
          break;
        }
      }
    }
  }
*/
  // Add CFI directives for callee-saved registers.
  const std::vector<CalleeSavedInfo> &CSI = MFI.getCalleeSavedInfo();
  // Iterate over list of callee-saved registers and emit .cfi_restore
  // directives.
  int64_t NSave = 0;
  for (const auto &Entry : CSI) {
    unsigned Reg = Entry.getReg();
    unsigned CFIIndex = MF.addFrameInst(MCCFIInstruction::createRestore(
        nullptr, RI->getDwarfRegNum(Reg, true)));
    BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
        .addCFIIndex(CFIIndex);
    NSave += 1;
  }

  // Deallocate space for locals
  adjustReg(MBB, MBBI, DL, SPReg, SPReg, StackSize-(NSave*4), MachineInstr::FrameDestroy);

  MCoreFunctionInfo *XFI = MF.getInfo<MCoreFunctionInfo>();
  unsigned LoReg = XFI->getLoSavedReg();
  unsigned HiReg = XFI->getHiSavedReg();
  if (NSave) {
    if (NSave == 1) {
      BuildMI(MBB, MBBI, DL, TII->get(MCore::LDW))
	      .addReg(LoReg, RegState::Define)
	      .addReg(SPReg).addImm(0);
    }
    else {
      if (HiReg != MCore::R15)
	NSave += 1;
      BuildMI(MBB, MBBI, DL, TII->get(MCore::LDM))
	      .addReg(LoReg, RegState::Define);
    }
    // Deallocate register save area
    adjustReg(MBB, MBBI, DL, SPReg, SPReg, (NSave*4), MachineInstr::FrameDestroy);
  }

  // After restoring $sp, we need to adjust CFA to $(sp + 0)
  // Emit ".cfi_def_cfa_offset 0"
  unsigned CFIIndex =
      MF.addFrameInst(MCCFIInstruction::createDefCfaOffset(nullptr, 0));
  BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex);
}

void MCoreFrameLowering::determineCalleeSaves(MachineFunction &MF,
                                              BitVector &SavedRegs,
                                              RegScavenger *RS) const {
LLVM_DEBUG(dbgs() << "MCoreFrameLowering::determineCalleeSaves \n");
  TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);
  // Unconditionally spill RA and FP only if the function uses a frame
  // pointer.
  if (hasFP(MF)) {
    SavedRegs.set(FPReg);
  }
  MCoreFunctionInfo *FI = MF.getInfo<MCoreFunctionInfo>();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  if (FI->isCBitSpilled())
    RS->addScavengingFrameIndex(MFI.CreateFixedObject(4, 4, false));
}

bool MCoreFrameLowering::
spillCalleeSavedRegisters(MachineBasicBlock &MBB,
                          MachineBasicBlock::iterator MI,
                          const std::vector<CalleeSavedInfo> &CSI,
                          const TargetRegisterInfo *TRI) const {
LLVM_DEBUG(dbgs() << "MCoreFrameLowering::spillCalleeSavedRegisters\n");
  if (CSI.empty())
    return true;
  MachineFunction *MF = MBB.getParent();
//  const TargetInstrInfo &TII = *MF->getSubtarget().getInstrInfo();
  MCoreFunctionInfo *XFI = MF->getInfo<MCoreFunctionInfo>();
  bool emitFrameMoves = MCoreRegisterInfo::needsFrameMoves(*MF);

  DebugLoc DL;
  if (MI != MBB.end() && !MI->isDebugInstr())
    DL = MI->getDebugLoc();

  for (std::vector<CalleeSavedInfo>::const_iterator it = CSI.begin();
                                                    it != CSI.end(); ++it) {
    unsigned Reg = it->getReg();
    // Add the callee-saved register as live-in. It's killed at the spill.
    MBB.addLiveIn(Reg);
    if (emitFrameMoves) {
      auto Store = MI;
      --Store;
      XFI->getSpillLabels().push_back(std::make_pair(Store, *it));
    }
  }
  return true;
}

bool MCoreFrameLowering::
restoreCalleeSavedRegisters(MachineBasicBlock &MBB,
                            MachineBasicBlock::iterator MI,
                            std::vector<CalleeSavedInfo> &CSI,
                            const TargetRegisterInfo *TRI) const{
LLVM_DEBUG(dbgs() << "MCoreFrameLowering::restoreCalleeSavedRegisters\n");
  return true;
}

int MCoreFrameLowering::getFrameIndexReference(const MachineFunction &MF,
                                               int FI,
                                               unsigned &FrameReg) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetRegisterInfo *RI = MF.getSubtarget().getRegisterInfo();
  const auto *XFI = MF.getInfo<MCoreFunctionInfo>();
LLVM_DEBUG(dbgs() << "MCoreFrameLowering::getFrameIndexReference\n");

  // Callee-saved registers should be referenced relative to the stack
  // pointer (positive offset), otherwise use the frame pointer (negative
  // offset).
  const std::vector<CalleeSavedInfo> &CSI = MFI.getCalleeSavedInfo();
  int MinCSFI = 0;
  int MaxCSFI = -1;

LLVM_DEBUG(dbgs() << "ObjectOffset=" << MFI.getObjectOffset(FI) <<
" OffsetOfLocalArea=" << getOffsetOfLocalArea() <<
" OffsetAdjustment=" << MFI.getOffsetAdjustment() << '\n');
  int Offset = MFI.getObjectOffset(FI) - getOffsetOfLocalArea() +
               MFI.getOffsetAdjustment();

  if (CSI.size()) {
    MinCSFI = CSI[0].getFrameIdx();
    MaxCSFI = CSI[CSI.size() - 1].getFrameIdx();
  }
LLVM_DEBUG(dbgs() << "FI=" << FI << " MinCSFI=" << MinCSFI <<
" MaxCSFI=" << MaxCSFI << '\n');
  if (FI >= MinCSFI && FI <= MaxCSFI) {
    FrameReg = SPReg;
    Offset += MF.getFrameInfo().getStackSize();
  } else if (RI->needsStackRealignment(MF)) {
    assert(!MFI.hasVarSizedObjects() &&
           "Unexpected combination of stack realignment and varsized objects");
    // If the stack was realigned, the frame pointer is set in order to allow
    // SP to be restored, but we still access stack objects using SP.
    FrameReg = SPReg;
    Offset += MF.getFrameInfo().getStackSize();
  } else {
    FrameReg = RI->getFrameRegister(MF);
    if (hasFP(MF))
      Offset += XFI->getVarArgsSaveSize();
    else
      Offset += MF.getFrameInfo().getStackSize();
  }
  return Offset;
}


// This function eliminates ADJCALLSTACKDOWN, ADJCALLSTACKUP pseudo instructions
MachineBasicBlock::iterator MCoreFrameLowering::eliminateCallFramePseudoInstr(
    MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator MI) const {
LLVM_DEBUG(dbgs() << "MCoreFrameLowering::eliminateCallFramePseudoInstr\n");
  DebugLoc DL = MI->getDebugLoc();

  if (!hasReservedCallFrame(MF)) {
    // If space has not been reserved for a call frame, ADJCALLSTACKDOWN and
    // ADJCALLSTACKUP must be converted to instructions manipulating the stack
    // pointer. This is necessary when there is a variable length stack
    // allocation (e.g. alloca), which means it's not possible to allocate
    // space for outgoing arguments from within the function prologue.
    int64_t Amount = MI->getOperand(0).getImm();

    if (Amount != 0) {
      // Ensure the stack remains aligned after adjustment.
      Amount = alignSPAdjust(Amount);

      if (MI->getOpcode() == MCore::ADJCALLSTACKDOWN)
        Amount = -Amount;

      adjustReg(MBB, MI, DL, SPReg, SPReg, Amount, MachineInstr::NoFlags);
    }
  }
  return MBB.erase(MI);
}

void MCoreFrameLowering::
processFunctionBeforeFrameFinalized(MachineFunction &MF,
                                    RegScavenger *RS) const {
LLVM_DEBUG(dbgs() << "MCoreFrameLowering::processFunctionBeforeFrameFinalized\n");
  assert(RS && "requiresRegisterScavenging failed");
}
