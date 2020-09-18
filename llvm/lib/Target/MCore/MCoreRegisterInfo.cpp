//===- MCoreRegisterInfo.cpp - MCore Register Information -------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the MCore implementation of the TargetRegisterInfo class.
//
//===----------------------------------------------------------------------===//

#include "MCoreRegisterInfo.h"
#include "MCore.h"
#include "MCoreInstrInfo.h"
#include "MCoreMachineFunctionInfo.h"
#include "MCoreSubtarget.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterScavenging.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/CodeGen/TargetFrameLowering.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

using namespace llvm;
#define DEBUG_TYPE "mcore-reg-info"

#define GET_REGINFO_TARGET_DESC
#include "MCoreGenRegisterInfo.inc"


static const unsigned FPReg = MCore::R14;
static const unsigned SPReg = MCore::SP;

MCoreRegisterInfo::MCoreRegisterInfo()
  : MCoreGenRegisterInfo(MCore::R15) {
}



bool MCoreRegisterInfo::needsFrameMoves(const MachineFunction &MF) {
  return MF.getMMI().hasDebugInfo() || MF.getFunction().needsUnwindTableEntry();
}

const MCPhysReg *
MCoreRegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
LLVM_DEBUG(dbgs() << "MCoreRegisterInfo::getCalleeSavedRegs\n");
  return CSR_ABI_SaveList;
}

BitVector MCoreRegisterInfo::getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  const MCoreFrameLowering *TFI = getFrameLowering(MF);

  Reserved.set(SPReg);
  Reserved.set(MCore::R15);	// is this necessary, since its live in
  Reserved.set(MCore::R1);	// don't allocate, save for special use
//  Reserved.set(MCore::PSR);
  if (TFI->hasFP(MF)) {
    Reserved.set(FPReg);
  }
  return Reserved;
}

const TargetRegisterClass*
MCoreRegisterInfo::getPointerRegClass(const MachineFunction &MF,
                                        unsigned Kind) const {
  return &MCore::GRegsRegClass;
}

const TargetRegisterClass *
MCoreRegisterInfo::getLargestLegalSuperClass(const TargetRegisterClass *RC,
                                           const MachineFunction &MF) const {
  if (RC == &MCore::CBitRegClass)
    return &MCore::SpillCBitRegClass;
  return TargetRegisterInfo::getLargestLegalSuperClass(RC, MF);
}

bool
MCoreRegisterInfo::trackLivenessAfterRegAlloc(const MachineFunction &MF) const {
  return true;
}

bool
MCoreRegisterInfo::useFPForScavengingIndex(const MachineFunction &MF) const {
  return false;
}

void
MCoreRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                       int SPAdj, unsigned FIOperandNum,
                                       RegScavenger *RS) const {
  assert(SPAdj == 0 && "Unexpected non-zero SPAdj value");

  MachineInstr *MI = &*II;
  MachineBasicBlock &MBB = *MI->getParent();
  MachineFunction &MF = *MBB.getParent();
  MachineRegisterInfo &MRI = MF.getRegInfo();
  const MCoreInstrInfo *TII = MF.getSubtarget<MCoreSubtarget>().getInstrInfo();
  DebugLoc DL = MI->getDebugLoc();

LLVM_DEBUG(dbgs() << "MCoreRegisterInfo::eliminateFrameIndex start\n");
LLVM_DEBUG(dbgs() << "MI= " << *MI << '\n');
  // Expand Pseudo-ops first
  int FrameIndex = MI->getOperand(FIOperandNum).getIndex();
  unsigned FrameReg;
  int Offset =
      getFrameLowering(MF)->getFrameIndexReference(MF, FrameIndex, FrameReg) +
      MI->getOperand(FIOperandNum + 1).getImm();
LLVM_DEBUG(dbgs() << "FrameReg=" << FrameReg << " Offset=" << Offset << '\n');
  if (!isInt<32>(Offset)) {
    report_fatal_error(
        "Frame offsets outside of the signed 32-bit range not supported");
  }
  bool FrameRegIsKill = false;
  unsigned DstReg = MI->getOperand(0).getReg();

  if (MI->getOpcode() == MCore::ADDi) {
    // ADDi here is just a placeholder for "take address of frame object"
LLVM_DEBUG(dbgs() << "eliminateFrameIndex ADDi: FrameReg=" << FrameReg <<
" Offset=" << Offset << '\n');
    if (Offset == 0) {
      BuildMI(MBB, II, DL, TII->get(MCore::MOV), DstReg)
	.addReg(FrameReg);
    } else {
LLVM_DEBUG(dbgs() << "address of frame object offset: " << Offset << '\n');
      TII->buildConstant(MBB, II, DL, Offset, DstReg);
      BuildMI(MBB, II, DL, TII->get(MCore::ADD), DstReg)
	.addReg(DstReg, RegState::Kill)
	.addReg(FrameReg);
    }
      MI->eraseFromParent();	// erase ADDi
      return;
  } else {				// load/store frame object
    if (DstReg == MCore::PSR) {
      MachineInstr *NewMI;
      unsigned NewReg = MRI.createVirtualRegister(&MCore::GRegsRegClass);
      if (MI->getOpcode() == MCore::LDW) {
	NewMI = BuildMI(MBB, II, DL, TII->get(MCore::LDW), NewReg)
		    .add(MI->getOperand(1))
		    .add(MI->getOperand(2))
		    .getInstr();
	BuildMI(MBB, II, DL, TII->get(MCore::BTSTi))
	    .add(MI->getOperand(0))
	    .addReg(NewReg, getKillRegState(true))
	    .addImm(0);
      } else {	// STW
	BuildMI(MBB, II, DL, TII->get(MCore::MVC), NewReg).add(MI->getOperand(0));
	NewMI = BuildMI(MBB, II, DL, TII->get(MCore::STW))
		    .addReg(NewReg, getKillRegState(true))
		    .add(MI->getOperand(1))
		    .add(MI->getOperand(2))
		    .getInstr();
      }
      MBB.erase(MI->getIterator());
      MI = NewMI;
    }
    unsigned scale = 1;	// assume byte
    switch (MI->getOpcode()) {
    case MCore::STW:
    case MCore::LDW: scale = 4;
      break;
    case MCore::STH:
    case MCore::LDHx:
    case MCore::LDHz: scale = 2;
      break;
    }
    if (Offset/scale > 15) {
LLVM_DEBUG(dbgs() << "eliminateFrameIndex large offset: " << Offset << '\n');
      unsigned ScratchReg = MCore::R1;
      TII->buildConstant(MBB, II, DL, Offset, ScratchReg);
      BuildMI(MBB, II, DL, TII->get(MCore::ADD), ScratchReg)
	.addReg(ScratchReg, RegState::Kill)
	.addReg(FrameReg);
      Offset = 0;
      FrameReg = ScratchReg;
      FrameRegIsKill = true;
    }
  }
  MI->getOperand(FIOperandNum)
      .ChangeToRegister(FrameReg, false, false, FrameRegIsKill);
  MI->getOperand(FIOperandNum+1).ChangeToImmediate(Offset);
LLVM_DEBUG(dbgs() << "MCoreRegisterInfo::eliminateFrameIndex final\n");
LLVM_DEBUG(dbgs() << "MI= " << *MI << '\n');
}


Register MCoreRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  const MCoreFrameLowering *TFI = getFrameLowering(MF);

  return TFI->hasFP(MF) ? FPReg : SPReg;
}

const uint32_t *
MCoreRegisterInfo::getCallPreservedMask(const MachineFunction & MF,
                                        CallingConv::ID /*CC*/) const {
LLVM_DEBUG(dbgs() << "MCoreRegisterInfo::getCallPreservedMask\n");
  return CSR_ABI_RegMask;
}
