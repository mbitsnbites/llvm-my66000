//===-- My66000FrameLowering.h - Frame info for My66000 Target ------*- C++ -*-===//
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

#ifndef LLVM_LIB_TARGET_MY66000_MY66000FRAMELOWERING_H
#define LLVM_LIB_TARGET_MY66000_MY66000FRAMELOWERING_H

#include "llvm/CodeGen/TargetFrameLowering.h"

namespace llvm {
  class My66000Subtarget;

  class My66000FrameLowering: public TargetFrameLowering {
  public:
    explicit My66000FrameLowering(const My66000Subtarget &STI)
      : TargetFrameLowering(TargetFrameLowering::StackGrowsDown, 8, 0, 8),
        STI(STI) {}


    /// emitProlog/emitEpilog - These methods insert prolog and epilog code into
    /// the function.
    void emitPrologue(MachineFunction &MF, MachineBasicBlock &MBB) const override;
    void emitEpilogue(MachineFunction &MF, MachineBasicBlock &MBB) const override;

    int getFrameIndexReference(const MachineFunction &MF, int FI,
                             unsigned &FrameReg) const override;

    bool spillCalleeSavedRegisters(MachineBasicBlock &MBB,
                                 MachineBasicBlock::iterator MI,
                                 const std::vector<CalleeSavedInfo> &CSI,
                                 const TargetRegisterInfo *TRI) const override;

    bool restoreCalleeSavedRegisters(MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator MI,
                              std::vector<CalleeSavedInfo> &CSI,
                              const TargetRegisterInfo *TRI) const override;

    MachineBasicBlock::iterator
    eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                                MachineBasicBlock::iterator I) const override;
    bool hasFP(const MachineFunction &MF) const override;

    void determineCalleeSaves(MachineFunction &MF, BitVector &SavedRegs,
                              RegScavenger *RS = nullptr) const override;

    void processFunctionBeforeFrameFinalized(MachineFunction &MF,
                                     RegScavenger *RS = nullptr) const override;

    static int stackSlotSize() {
      return 8;
    }

  protected:
    const My66000Subtarget &STI;

  private:
    void determineFrameLayout(MachineFunction &MF) const;
    void adjustReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
		   const DebugLoc &DL, unsigned DstReg, unsigned SrcReg,
		   int64_t Val, MachineInstr::MIFlag Flag) const;

  };
}

#endif
