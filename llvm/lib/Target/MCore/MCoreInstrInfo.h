//===- MCoreInstrInfo.h - MCore Instruction Information ---------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the MCore implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MCORE_MCOREINSTRINFO_H
#define LLVM_LIB_TARGET_MCORE_MCOREINSTRINFO_H

#include "MCoreRegisterInfo.h"
#include "llvm/CodeGen/TargetInstrInfo.h"

#define GET_INSTRINFO_HEADER
#include "MCoreGenInstrInfo.inc"

namespace llvm {

class MCoreInstrInfo : public MCoreGenInstrInfo {
  const MCoreRegisterInfo RI;
  virtual void anchor();

public:
  MCoreInstrInfo(void);

  void buildConstant(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
			const DebugLoc &DL, uint32_t val,
			unsigned reg,
			MachineInstr::MIFlag Flag = MachineInstr::NoFlags) const;

  void copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator MBBI,
			const DebugLoc &DL, unsigned DstReg, unsigned SrcReg,
			bool KillSrc) const override;

  unsigned isStoreToStackSlot(const MachineInstr &MI,
			int &FrameIndex) const override;

  void storeRegToStackSlot(MachineBasicBlock &MBB,
			MachineBasicBlock::iterator MBBI, unsigned SrcReg,
			bool IsKill, int FrameIndex,
			const TargetRegisterClass *RC,
			const TargetRegisterInfo *TRI) const override;

  void loadRegFromStackSlot(MachineBasicBlock &MBB,
			MachineBasicBlock::iterator MBBI, unsigned DstReg,
			int FrameIndex, const TargetRegisterClass *RC,
			const TargetRegisterInfo *TRI) const override;

  bool analyzeBranch(MachineBasicBlock &MBB, MachineBasicBlock *&TBB,
			MachineBasicBlock *&FBB,
			SmallVectorImpl<MachineOperand> &Cond,
			bool AllowModify) const override;

  unsigned insertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
                        MachineBasicBlock *FBB, ArrayRef<MachineOperand> Cond,
                        const DebugLoc &DL,
                        int *BytesAdded = nullptr) const override;

  unsigned removeBranch(MachineBasicBlock &MBB,
                        int *BytesRemoved = nullptr) const override;

  bool
  reverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond) const override;

  const MCoreRegisterInfo &getRegisterInfo() const { return RI; }
};

}

#endif
