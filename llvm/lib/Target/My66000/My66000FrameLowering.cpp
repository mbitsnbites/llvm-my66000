//===-- My66000FrameLowering.cpp - Frame info for My66000 Target --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains My66000 frame information that doesn't fit anywhere else
// cleanly...
//
//===----------------------------------------------------------------------===//

#include "My66000FrameLowering.h"
#include "My66000.h"
#include "My66000InstrInfo.h"
#include "My66000MachineFunctionInfo.h"
#include "My66000Subtarget.h"
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

static const unsigned FPReg = My66000::R1;
static const unsigned SPReg = My66000::SP;

bool My66000FrameLowering::hasFP(const MachineFunction &MF) const {
// needsStackRealignment(MF) || isFrameAddressTaken() ???
  return MF.getTarget().Options.DisableFramePointerElim(MF) ||
         MF.getFrameInfo().hasVarSizedObjects();
}

// Determines the size of the frame and maximum call frame size.
void My66000FrameLowering::determineFrameLayout(MachineFunction &MF) const {
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const My66000RegisterInfo *RI = STI.getRegisterInfo();

  // Get the number of bytes to allocate from the FrameInfo.
  uint64_t FrameSize = MFI.getStackSize();

  // Get the alignment.
  unsigned StackAlign = getStackAlignment();
  if (RI->needsStackRealignment(MF)) {
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

void My66000FrameLowering::adjustReg(MachineBasicBlock &MBB,
                                   MachineBasicBlock::iterator MBBI,
                                   const DebugLoc &DL, unsigned DstReg,
                                   unsigned SrcReg, int64_t Val,
                                   MachineInstr::MIFlag Flag) const {
  const My66000InstrInfo *TII = STI.getInstrInfo();
LLVM_DEBUG(dbgs() << "My66000FrameLowering::adjustReg: " << Val << "\n");

  if (DstReg == SrcReg && Val == 0)
    return;

  if (isInt<16>(Val)) {
    BuildMI(MBB, MBBI, DL, TII->get(My66000::ADDri), DstReg)
        .addReg(SrcReg)
        .addImm(Val)
        .setMIFlag(Flag);
  } else if (isInt<32>(Val)) {
    // FIXME
    report_fatal_error("32 bit adjustments not yet implemented");
  } else {
    report_fatal_error("adjustReg cannot yet handle adjustments >32 bits");
  }
}

void My66000FrameLowering::emitPrologue(MachineFunction &MF,
                                      MachineBasicBlock &MBB) const {
LLVM_DEBUG(dbgs() << "My66000FrameLowering::emitPrologue\n");
  assert(&MF.front() == &MBB && "Shrink-wrapping not yet supported");

  MachineFrameInfo &MFI = MF.getFrameInfo();
  const My66000RegisterInfo *RI = STI.getRegisterInfo();
  const My66000InstrInfo *TII = STI.getInstrInfo();
  MachineBasicBlock::iterator MBBI = MBB.begin();

  if (RI->needsStackRealignment(MF) && MFI.hasVarSizedObjects()) {
    report_fatal_error(
        "My66000 backend can't currently handle functions that need stack "
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

  // Allocate space on the stack if necessary.
//  adjustReg(MBB, MBBI, DL, SPReg, SPReg, -StackSize, MachineInstr::FrameSetup);

  // Emit ".cfi_def_cfa_offset StackSize"
  unsigned CFIIndex = MF.addFrameInst(
      MCCFIInstruction::createDefCfaOffset(nullptr, -StackSize));
  BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex);

  // Iterate over list of callee-saved registers and emit .cfi_offset
  // directives.
  const std::vector<CalleeSavedInfo> &CSI = MFI.getCalleeSavedInfo();
  unsigned NSave = 0;
  unsigned HiReg = My66000::R0;
  unsigned LoReg = 100;
  int64_t Offset;;
  for (const auto &Entry : CSI) {
    Offset = MFI.getObjectOffset(Entry.getFrameIdx());
    unsigned Reg = Entry.getReg();
    unsigned CFIIndex = MF.addFrameInst(MCCFIInstruction::createOffset(
        nullptr, RI->getDwarfRegNum(Reg, true), Offset));
    BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
        .addCFIIndex(CFIIndex);
    if (Reg > My66000::R0 && Reg < LoReg) { LoReg = Reg; }
    NSave += 1;
  }
  if (NSave == 1) LoReg = HiReg;
  My66000FunctionInfo *XFI = MF.getInfo<My66000FunctionInfo>();
  XFI->setHiSavedReg(HiReg);	// save for epilogue
  XFI->setLoSavedReg(LoReg);	// save for epilogue

  if (NSave) {
    bool isLive = MBB.isLiveIn(LoReg);
    Offset = StackSize - (NSave*8) - XFI->getVarArgsSaveSize();
    BuildMI(MBB, MBBI, DL, TII->get(My66000::ENTER))
	      .addReg(LoReg, getKillRegState(!isLive))
	      .addReg(HiReg, getKillRegState(!isLive))
	      .addImm(0)
	      .addImm(Offset);
    if (!isLive)
      MBB.addLiveIn(LoReg);
  }
  // Generate new FP.
  if (hasFP(MF)) {
    // copy unadjusted SP to FP
    adjustReg(MBB, MBBI, DL, FPReg, SPReg, StackSize, MachineInstr::FrameSetup);
  }
}

void My66000FrameLowering::emitEpilogue(MachineFunction &MF,
                                     MachineBasicBlock &MBB) const {
LLVM_DEBUG(dbgs() << "My66000FrameLowering::emitEpilogue\n");
  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  const My66000RegisterInfo *RI = STI.getRegisterInfo();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  DebugLoc DL = MBBI->getDebugLoc();
  const My66000InstrInfo *TII = STI.getInstrInfo();

  uint64_t StackSize = MFI.getStackSize();

  // Restore the stack pointer using the value of the frame pointer. Only
  // necessary if the stack pointer was modified, meaning the stack size is
  // unknown.
  if (RI->needsStackRealignment(MF) || MFI.hasVarSizedObjects()) {
    assert(hasFP(MF) && "frame pointer should not have been eliminated");
    auto *XFI = MF.getInfo<My66000FunctionInfo>();
    uint64_t FPOffset = StackSize - XFI->getVarArgsSaveSize();
dbgs() << "Epilogue needs FP to recover SP: " << FPOffset << "\n";
    adjustReg(MBB, MBBI, DL, SPReg, FPReg, -FPOffset,
	      MachineInstr::FrameDestroy);
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
  unsigned NSave = 0;
  for (const auto &Entry : CSI) {
    unsigned Reg = Entry.getReg();
    unsigned CFIIndex = MF.addFrameInst(MCCFIInstruction::createRestore(
        nullptr, RI->getDwarfRegNum(Reg, true)));
    BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
        .addCFIIndex(CFIIndex);
    NSave += 1;
  }

  My66000FunctionInfo *XFI = MF.getInfo<My66000FunctionInfo>();
  unsigned HiReg = XFI->getHiSavedReg();
  unsigned LoReg = XFI->getLoSavedReg();
  if (NSave) {
    int64_t Offset = StackSize - (NSave*8) - XFI->getVarArgsSaveSize();
    if (MFI.hasTailCall()) {
      if (NSave > 1) {
	BuildMI(MBB, MBBI, DL, TII->get(My66000::LDM))
	      .addReg(LoReg, RegState::Define)
 	      .addReg(HiReg, RegState::Define)
	      .addReg(SPReg)		// base register
	      .addReg(My66000::R0)	// index register (none)
	      .addImm(0)		// shift amount
	      .addImm(Offset);
	// if we saved more than 1, then SP was saved and will be restored
      } else {
	BuildMI(MBB, MBBI, DL, TII->get(My66000::LDDri))
	      .addReg(HiReg, RegState::Define)
	      .addReg(SPReg).addImm(Offset);
        BuildMI(MBB, MBBI, DL, TII->get(My66000::ADDri), SPReg)
	      .addReg(SPReg)
	      .addImm(8);
      }
    } else {	// normal exit
      BuildMI(MBB, MBBI, DL, TII->get(My66000::EXIT))
	      .addReg(LoReg, RegState::Define)
	      .addReg(HiReg, RegState::Define)
	      .addImm(0)
	      .addImm(Offset);
      MBB.erase(MBBI); 	// remove the return
    }
  }
  // Deallocate stack
//  adjustReg(MBB, MBBI, DL, SPReg, SPReg, StackSize, MachineInstr::FrameDestroy);
/*
  // After restoring $sp, we need to adjust CFA to $(sp + 0)
  // Emit ".cfi_def_cfa_offset 0"
  unsigned CFIIndex =
      MF.addFrameInst(MCCFIInstruction::createDefCfaOffset(nullptr, 0));
  BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex);
*/
}

void My66000FrameLowering::determineCalleeSaves(MachineFunction &MF,
                                              BitVector &SavedRegs,
                                              RegScavenger *RS) const {
LLVM_DEBUG(dbgs() << "My66000FrameLowering::determineCalleeSaves \n");
  TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);
  // Unconditionally spill RA and FP only if the function uses a frame
  // pointer.
  if (hasFP(MF)) {
    SavedRegs.set(FPReg);
  }
}

bool My66000FrameLowering::
spillCalleeSavedRegisters(MachineBasicBlock &MBB,
                          MachineBasicBlock::iterator MI,
                          const std::vector<CalleeSavedInfo> &CSI,
                          const TargetRegisterInfo *TRI) const {
LLVM_DEBUG(dbgs() << "My66000FrameLowering::spillCalleeSavedRegisters\n");
  if (CSI.empty())
    return true;
  MachineFunction *MF = MBB.getParent();
//  const TargetInstrInfo &TII = *MF->getSubtarget().getInstrInfo();
  My66000FunctionInfo *XFI = MF->getInfo<My66000FunctionInfo>();
  bool emitFrameMoves = My66000RegisterInfo::needsFrameMoves(*MF);

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

bool My66000FrameLowering::
restoreCalleeSavedRegisters(MachineBasicBlock &MBB,
                            MachineBasicBlock::iterator MI,
                            std::vector<CalleeSavedInfo> &CSI,
                            const TargetRegisterInfo *TRI) const{
LLVM_DEBUG(dbgs() << "My66000FrameLowering::restoreCalleeSavedRegisters\n");
  return true;
}

int My66000FrameLowering::getFrameIndexReference(const MachineFunction &MF,
                                               int FI,
                                               unsigned &FrameReg) const {
  const MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetRegisterInfo *RI = MF.getSubtarget().getRegisterInfo();
  const auto *XFI = MF.getInfo<My66000FunctionInfo>();
LLVM_DEBUG(dbgs() << "My66000FrameLowering::getFrameIndexReference\n");

  // Callee-saved registers should be referenced relative to the stack
  // pointer (positive offset), otherwise use the frame pointer (negative
  // offset).
  const std::vector<CalleeSavedInfo> &CSI = MFI.getCalleeSavedInfo();
  int MinCSFI = 0;
  int MaxCSFI = -1;

  int Offset = MFI.getObjectOffset(FI) - getOffsetOfLocalArea() +
               MFI.getOffsetAdjustment();

  if (CSI.size()) {
    MinCSFI = CSI[0].getFrameIdx();
    MaxCSFI = CSI[CSI.size() - 1].getFrameIdx();
  }

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
MachineBasicBlock::iterator My66000FrameLowering::eliminateCallFramePseudoInstr(
    MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator MI) const {
LLVM_DEBUG(dbgs() << "My66000FrameLowering::eliminateCallFramePseudoInstr\n");
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

      if (MI->getOpcode() == My66000::ADJCALLSTACKDOWN)
        Amount = -Amount;

      adjustReg(MBB, MI, DL, SPReg, SPReg, Amount, MachineInstr::NoFlags);
    }
  }
  return MBB.erase(MI);
}

void My66000FrameLowering::
processFunctionBeforeFrameFinalized(MachineFunction &MF,
                                    RegScavenger *RS) const {
LLVM_DEBUG(dbgs() << "My66000FrameLowering::processFunctionBeforeFrameFinalized\n");
  assert(RS && "requiresRegisterScavenging failed");
}
