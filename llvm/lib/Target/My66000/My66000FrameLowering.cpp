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

static const unsigned FramePtr = My66000::R30;
static const int MaxImmU16 = (1<<16) - 1;

// helper functions. FIXME: Eliminate.
static inline bool isImmU6(unsigned val) {
  return val < (1 << 6);
}

static inline bool isImmU16(unsigned val) {
  return val < (1 << 16);
}

// Helper structure with compare function for handling stack slots.
namespace {
struct StackSlotInfo {
  int FI;
  int Offset;
  unsigned Reg;
  StackSlotInfo(int f, int o, int r) : FI(f), Offset(o), Reg(r){};
};
}  // end anonymous namespace
/*
static MachineMemOperand *getFrameIndexMMO(MachineBasicBlock &MBB,
                                           int FrameIndex,
                                           MachineMemOperand::Flags flags) {
  MachineFunction *MF = MBB.getParent();
  const MachineFrameInfo &MFI = MF->getFrameInfo();
  MachineMemOperand *MMO = MF->getMachineMemOperand(
      MachinePointerInfo::getFixedStack(*MF, FrameIndex), flags,
      MFI.getObjectSize(FrameIndex), MFI.getObjectAlignment(FrameIndex));
  return MMO;
}
*/

//===----------------------------------------------------------------------===//
// My66000FrameLowering:
//===----------------------------------------------------------------------===//

bool My66000FrameLowering::hasFP(const MachineFunction &MF) const {
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

//  unsigned FPReg = My66000::R30;
  unsigned SPReg = My66000::SP;

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
  adjustReg(MBB, MBBI, DL, SPReg, SPReg, -StackSize, MachineInstr::FrameSetup);

  // Emit ".cfi_def_cfa_offset StackSize"
  unsigned CFIIndex = MF.addFrameInst(
      MCCFIInstruction::createDefCfaOffset(nullptr, -StackSize));
  BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex);

  // The frame pointer is callee-saved, and code has been generated for us to
  // save it to the stack. We need to skip over the storing of callee-saved
  // registers as the frame pointer must be modified after it has been saved
  // to the stack, not before.
  // FIXME: assumes exactly one instruction is used to save each callee-saved
  // register.
  const std::vector<CalleeSavedInfo> &CSI = MFI.getCalleeSavedInfo();
  std::advance(MBBI, CSI.size());

  // Iterate over list of callee-saved registers and emit .cfi_offset
  // directives.
  for (const auto &Entry : CSI) {
    int64_t Offset = MFI.getObjectOffset(Entry.getFrameIdx());
    unsigned Reg = Entry.getReg();
    unsigned CFIIndex = MF.addFrameInst(MCCFIInstruction::createOffset(
        nullptr, RI->getDwarfRegNum(Reg, true), Offset));
    BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
        .addCFIIndex(CFIIndex);
  }

  // Generate new FP.
  if (hasFP(MF)) {
//dbgs() << "\thasFP is true\n";
/*
// FIXME - next statement doesn't compile
    auto *RVFI = MF.getInfo<My66000MachineFunctionInfo>();
    adjustReg(MBB, MBBI, DL, FPReg, SPReg,
              StackSize - RVFI->getVarArgsSaveSize(), MachineInstr::FrameSetup);
    // Emit ".cfi_def_cfa $fp, 0"
    unsigned CFIIndex = MF.addFrameInst(MCCFIInstruction::createDefCfa(
        nullptr, RI->getDwarfRegNum(FPReg, true), 0));
    BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
        .addCFIIndex(CFIIndex);
    // Realign Stack
    const My66000RegisterInfo *RI = STI.getRegisterInfo();
    if (RI->needsStackRealignment(MF)) {
      unsigned MaxAlignment = MFI.getMaxAlignment();

      const My66000InstrInfo *TII = STI.getInstrInfo();
      if (isInt<12>(-(int)MaxAlignment)) {
        BuildMI(MBB, MBBI, DL, TII->get(RISCV::ANDI), SPReg)
            .addReg(SPReg)
            .addImm(-(int)MaxAlignment);
      } else {
        unsigned ShiftAmount = countTrailingZeros(MaxAlignment);
        unsigned VR =
            MF.getRegInfo().createVirtualRegister(&RISCV::GPRRegClass);
        BuildMI(MBB, MBBI, DL, TII->get(RISCV::SRLI), VR)
            .addReg(SPReg)
            .addImm(ShiftAmount);
        BuildMI(MBB, MBBI, DL, TII->get(RISCV::SLLI), SPReg)
            .addReg(VR)
            .addImm(ShiftAmount);
      }
    }
*/
  }
}

void My66000FrameLowering::emitEpilogue(MachineFunction &MF,
                                     MachineBasicBlock &MBB) const {
LLVM_DEBUG(dbgs() << "My66000FrameLowering::emitEpilogue\n");
  MachineBasicBlock::iterator MBBI = MBB.getLastNonDebugInstr();
  const My66000RegisterInfo *RI = STI.getRegisterInfo();
  MachineFrameInfo &MFI = MF.getFrameInfo();
//  auto *RVFI = MF.getInfo<My66000MachineFunctionInfo>();
  DebugLoc DL = MBBI->getDebugLoc();
  const My66000InstrInfo *TII = STI.getInstrInfo();
//  unsigned FPReg = My66000::R30;
  unsigned SPReg = My66000::SP;

  // Skip to before the restores of callee-saved registers
  // FIXME: assumes exactly one instruction is used to restore each
  // callee-saved register.
  auto LastFrameDestroy = std::prev(MBBI, MFI.getCalleeSavedInfo().size());

  uint64_t StackSize = MFI.getStackSize();
/*
  uint64_t FPOffset = StackSize - RVFI->getVarArgsSaveSize();

  // Restore the stack pointer using the value of the frame pointer. Only
  // necessary if the stack pointer was modified, meaning the stack size is
  // unknown.
  if (RI->needsStackRealignment(MF) || MFI.hasVarSizedObjects()) {
    assert(hasFP(MF) && "frame pointer should not have been eliminated");
    adjustReg(MBB, LastFrameDestroy, DL, SPReg, FPReg, -FPOffset,
              MachineInstr::FrameDestroy);
  }

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
  for (const auto &Entry : CSI) {
    unsigned Reg = Entry.getReg();
    unsigned CFIIndex = MF.addFrameInst(MCCFIInstruction::createRestore(
        nullptr, RI->getDwarfRegNum(Reg, true)));
    BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
        .addCFIIndex(CFIIndex);
  }

  // Deallocate stack
  adjustReg(MBB, MBBI, DL, SPReg, SPReg, StackSize, MachineInstr::FrameDestroy);

  // After restoring $sp, we need to adjust CFA to $(sp + 0)
  // Emit ".cfi_def_cfa_offset 0"
  unsigned CFIIndex =
      MF.addFrameInst(MCCFIInstruction::createDefCfaOffset(nullptr, 0));
  BuildMI(MBB, MBBI, DL, TII->get(TargetOpcode::CFI_INSTRUCTION))
      .addCFIIndex(CFIIndex);
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
  const TargetInstrInfo &TII = *MF->getSubtarget().getInstrInfo();
  My66000FunctionInfo *XFI = MF->getInfo<My66000FunctionInfo>();
  bool emitFrameMoves = My66000RegisterInfo::needsFrameMoves(*MF);

  DebugLoc DL;
  if (MI != MBB.end() && !MI->isDebugInstr())
    DL = MI->getDebugLoc();

  for (std::vector<CalleeSavedInfo>::const_iterator it = CSI.begin();
                                                    it != CSI.end(); ++it) {
    unsigned Reg = it->getReg();
    assert(!(Reg == My66000::R30 && hasFP(*MF)) && "FP handled in emitPrologue");

    // Add the callee-saved register as live-in. It's killed at the spill.
    MBB.addLiveIn(Reg);
    const TargetRegisterClass *RC = TRI->getMinimalPhysRegClass(Reg);
    TII.storeRegToStackSlot(MBB, MI, Reg, true, it->getFrameIdx(), RC, TRI);
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
  MachineFunction *MF = MBB.getParent();
  const TargetInstrInfo &TII = *MF->getSubtarget().getInstrInfo();
  bool AtStart = MI == MBB.begin();
  MachineBasicBlock::iterator BeforeI = MI;
  if (!AtStart)
    --BeforeI;
  for (std::vector<CalleeSavedInfo>::const_iterator it = CSI.begin();
                                                    it != CSI.end(); ++it) {
    unsigned Reg = it->getReg();
    assert(!(Reg == My66000::R30 && hasFP(*MF)) && "FP are handled in emitEpilogue");

    const TargetRegisterClass *RC = TRI->getMinimalPhysRegClass(Reg);
    TII.loadRegFromStackSlot(MBB, MI, Reg, it->getFrameIdx(), RC, TRI);
    assert(MI != MBB.begin() &&
           "loadRegFromStackSlot didn't insert any code!");
    // Insert in reverse order.  loadRegFromStackSlot can insert multiple
    // instructions.
    if (AtStart)
      MI = MBB.begin();
    else {
      MI = BeforeI;
      ++MI;
    }
  }
  return true;
}

// This function eliminates ADJCALLSTACKDOWN, ADJCALLSTACKUP pseudo instructions
MachineBasicBlock::iterator My66000FrameLowering::eliminateCallFramePseudoInstr(
    MachineFunction &MF, MachineBasicBlock &MBB,
    MachineBasicBlock::iterator MI) const {
LLVM_DEBUG(dbgs() << "My66000FrameLowering::eliminateCallFramePseudoInstr\n");
  unsigned SPReg = My66000::SP;
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

void My66000FrameLowering::determineCalleeSaves(MachineFunction &MF,
                                              BitVector &SavedRegs,
                                              RegScavenger *RS) const {
LLVM_DEBUG(dbgs() << "My66000FrameLowering::determineCalleeSaves\n");
  TargetFrameLowering::determineCalleeSaves(MF, SavedRegs, RS);
  // Unconditionally spill RA and FP only if the function uses a frame
  // pointer.
  if (hasFP(MF)) {
    SavedRegs.set(My66000::R0);
    SavedRegs.set(My66000::R30);
  }
}

void My66000FrameLowering::
processFunctionBeforeFrameFinalized(MachineFunction &MF,
                                    RegScavenger *RS) const {
LLVM_DEBUG(dbgs() << "My66000FrameLowering::processFunctionBeforeFrameFinalized\n");
  assert(RS && "requiresRegisterScavenging failed");
  MachineFrameInfo &MFI = MF.getFrameInfo();
  const TargetRegisterClass &RC = My66000::GRegsRegClass;
  const TargetRegisterInfo &TRI = *MF.getSubtarget().getRegisterInfo();
  My66000FunctionInfo *XFI = MF.getInfo<My66000FunctionInfo>();
  // Reserve slots close to SP or frame pointer for Scavenging spills.
  // When using SP for small frames, we don't need any scratch registers.
  // When using SP for large frames, we may need 2 scratch registers.
  // When using FP, for large or small frames, we may need 1 scratch register.
  unsigned Size = TRI.getSpillSize(RC);
  unsigned Align = TRI.getSpillAlignment(RC);
  if (XFI->isLargeFrame(MF) || hasFP(MF))
    RS->addScavengingFrameIndex(MFI.CreateStackObject(Size, Align, false));
  if (XFI->isLargeFrame(MF) && !hasFP(MF))
    RS->addScavengingFrameIndex(MFI.CreateStackObject(Size, Align, false));
}
