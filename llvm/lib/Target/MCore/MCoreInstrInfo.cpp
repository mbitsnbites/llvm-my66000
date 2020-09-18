//===- MCoreInstrInfo.cpp - MCore Instruction Information -------*- C++ -*-===//
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

#include "MCore.h"
#include "MCoreInstrInfo.h"
#include "MCoreMachineFunctionInfo.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/Debug.h"


#define DEBUG_TYPE "my66000-instrinfo"

#define GET_INSTRINFO_CTOR_DTOR
#include "MCoreGenInstrInfo.inc"

using namespace llvm;

// Pin the vtable to this file.
void MCoreInstrInfo::anchor() {}

MCoreInstrInfo::MCoreInstrInfo()
  : MCoreGenInstrInfo(MCore::ADJCALLSTACKDOWN, MCore::ADJCALLSTACKUP),
    RI() {
}


static bool
FindConst1(uint32_t n, unsigned &inst, uint32_t &valu)
{
    if (n <= 127)
    {   inst = MCore::MOVi;
        valu = n;
	return true;
    }
    if ((n & (n-1)) == 0)
    {   inst = MCore::BGENi;
	valu = 31 - countLeadingZeros(n);
	return true;
    }
    if ((n & (n+1)) == 0)
    {   inst = MCore::BMASKi;
        valu = 32 - countLeadingZeros(n);
	return true;
    }
    return false;
}

static void
FindConst2(uint32_t n, unsigned &inst1, uint32_t &valu1,
		       unsigned &inst2, uint32_t &valu2)
{
    int i;
    uint32_t tmp;

    if (FindConst1(n, inst1, valu1))
    {	inst2 = 0;
	return;
    }
    if (FindConst1(~n, inst1, valu1))
    {   inst2 = MCore::NOT;
	valu2 = 0;
	return;
    }
    for (i = 1; i <= 32; i++)
    {   if (FindConst1(n-i, inst1, valu1))
	{   inst2 = MCore::ADDi;
	    valu2 = i;
	    return;
	}
	if (FindConst1(n+i, inst1, valu1))
	{   inst2 = MCore::SUBi;
	    valu2 = i;
	    return;
	}
	if (FindConst1(i - n, inst1, valu1))
	{   inst2 = MCore::RSUBi;
	    valu2 = i;
	    return;
	}
    }
    for (i = 31, tmp = 0x80000000L; i >= 0; i--, tmp >>= 1)
    {   if (FindConst1(n & ~tmp, inst1, valu1))
	{   inst2 = MCore::BSETi;
	    valu2 = i;
	    return;
	}
	if (FindConst1(n | tmp, inst1, valu1))
	{   inst2 = MCore::BCLRi;
	    valu2 = i;
	    return;
	}
    }
    if ((n % 3) == 0 && FindConst1(n / 3, inst1, valu1))
    {	inst2 = MCore::IXH;
	valu2 = 0;
	return;
    }
    if ((n % 5) == 0 && FindConst1(n / 5, inst1, valu1))
    {	inst2 = MCore::IXW;
	valu2 = 0;
	return;
    }
    if ((n&1) == 0)		/* try shift left */
    {   tmp = n;
	i = 1;
	while ((n&1) == 0 && i < 32) {
	    n >>= 1;
	    if (FindConst1(n, inst1, valu1)) {
	        inst2 = MCore::LSLi;
		valu2 = i;
		return;
	    }
	    i += 1;
	}
    }
    tmp = n;
    for (i = 1; i < 31; i++)	/* try rotate left */
    {   int c;
	c = tmp << 31;
	tmp >>= 1;
	tmp &= 0x7FFFFFFF;
	tmp |= c;   /* Simulate rotate.  */
	if (FindConst1(tmp, inst1, valu1))
	{   inst2 = MCore::ROTLi;
	    valu2 = i;
	    return;
	}
    }
    inst1 = MCore::LRW;
    valu1 = n;
    return;
}

void MCoreInstrInfo::buildConstant(MachineBasicBlock &MBB,
				MachineBasicBlock::iterator MBBI,
				const DebugLoc &DL, uint32_t val,
				unsigned reg, MachineInstr::MIFlag Flag) const {
  unsigned inst1, inst2;
  uint32_t valu1, valu2;

  FindConst2(val, inst1, valu1, inst2, valu2);
LLVM_DEBUG(dbgs() << "buildConstant inst1=" << inst1 << " inst2=" << inst2 << '\n');
  BuildMI(MBB, MBBI, DL, get(inst1), reg).addImm(valu1).setMIFlag(Flag);
  if (inst2 != 0) {
    switch (inst2) {
      case MCore::NOT:
	BuildMI(MBB, MBBI, DL, get(inst2), reg)
	    .addReg(reg, RegState::Kill)
	    .setMIFlag(Flag);
	break;
      case MCore::ADDi:
      case MCore::SUBi:
      case MCore::RSUBi:
      case MCore::BSETi:
      case MCore::BCLRi:
      case MCore::LSLi:
      case MCore::ROTLi:
	BuildMI(MBB, MBBI, DL, get(inst2), reg)
	    .addReg(reg, RegState::Kill)
	    .addImm(valu2)
	    .setMIFlag(Flag);
	break;
      case MCore::IXH:
      case MCore::IXW:
	BuildMI(MBB, MBBI, DL, get(inst2), reg)
	    .addReg(reg)
	    .addReg(reg, RegState::Kill)
	    .setMIFlag(Flag);
	break;
      default:
	break;
      }
  }
}

void MCoreInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                               MachineBasicBlock::iterator I,
                               const DebugLoc &dl, unsigned DstReg,
                               unsigned SrcReg, bool KillSrc) const {
  if (SrcReg == MCore::PSR) {
    BuildMI(MBB, I, dl, get(MCore::MVC), DstReg)
	.addReg(SrcReg, getKillRegState(KillSrc));
    return;
  }
  if (DstReg == MCore::PSR) {
    BuildMI(MBB, I, dl, get(MCore::BTSTi), DstReg)
        .addReg(SrcReg, getKillRegState(true))
        .addImm(0);
    return;
  }
  // assume GReg to Greg
  BuildMI(MBB, I, dl, get(MCore::MOV), DstReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
}

unsigned MCoreInstrInfo::isStoreToStackSlot(const MachineInstr &MI,
				int &FrameIndex) const {
LLVM_DEBUG(dbgs() << "MCoreInstrInfo::isStoreToStackSlot\n");
  switch (MI.getOpcode()) {
  default:
    return 0;
  case MCore::STB:
  case MCore::STH:
  case MCore::STW:
    break;
  }
  if (MI.getOperand(0).isFI() && MI.getOperand(1).isImm() &&
      MI.getOperand(1).getImm() == 0) {
    FrameIndex = MI.getOperand(0).getIndex();
    return MI.getOperand(2).getReg();
  }
  return 0;
}

void MCoreInstrInfo::storeRegToStackSlot(MachineBasicBlock &MBB,
		    MachineBasicBlock::iterator I,
                    unsigned SrcReg, bool isKill, int FI,
                    const TargetRegisterClass *RC,
		    const TargetRegisterInfo *TRI) const {
LLVM_DEBUG(dbgs() << "MCoreInstrInfo::storeRegToStackSlot\n");
  DebugLoc DL;
  if (I != MBB.end())
    DL = I->getDebugLoc();
  BuildMI(MBB, I, DL, get(MCore::STW))
	.addReg(SrcReg, getKillRegState(isKill))
	.addFrameIndex(FI)
	.addImm(0);
}

void MCoreInstrInfo::loadRegFromStackSlot(MachineBasicBlock &MBB,
		    MachineBasicBlock::iterator I,
		    unsigned DstReg, int FI,
		    const TargetRegisterClass *RC,
		    const TargetRegisterInfo *TRI) const {
LLVM_DEBUG(dbgs() << "MCoreInstrInfo::loadRegFromStackSlot\n");
  DebugLoc DL;
  if (I != MBB.end())
    DL = I->getDebugLoc();
  BuildMI(MBB, I, DL, get(MCore::LDW), DstReg)
	.addFrameIndex(FI)
	.addImm(0);
}

// Pushed on the Cond Vector:
// [0] - instruction opcode (as Imm) either JT or JF
static void parseCondBranch(MachineInstr &LastInst, MachineBasicBlock *&Target,
                            SmallVectorImpl<MachineOperand> &Cond) {
LLVM_DEBUG(dbgs() << "MCoreInstrInfo::parseCondBranch: " <<
LastInst.getOpcode() << '\n');

  // Block ends with fall-through condbranch.
  assert(LastInst.getDesc().isConditionalBranch() && "Unknown condbranch");
  Target = LastInst.getOperand(0).getMBB();
  Cond.push_back(MachineOperand::CreateImm(LastInst.getOpcode()));
  Cond.push_back(LastInst.getOperand(1));	// CBit register
}

bool MCoreInstrInfo::analyzeBranch(MachineBasicBlock &MBB,
				     MachineBasicBlock *&TBB,
				     MachineBasicBlock *&FBB,
				     SmallVectorImpl<MachineOperand> &Cond,
				     bool AllowModify) const {
  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();

LLVM_DEBUG(dbgs() << "MCoreInstrInfo::analyzeBranch: " <<
printMBBReference(MBB) << '\n');
  // If the block has no terminators, it just falls into the block after it.
  if (I == MBB.end() || !isUnpredicatedTerminator(*I))
    return false;
  // Count the number of terminators and find the first unconditional or
  // indirect branch.
  MachineBasicBlock::iterator FirstUncondOrIndirectBr = MBB.end();
  int NumTerminators = 0;
  for (auto J = I.getReverse(); J != MBB.rend() && isUnpredicatedTerminator(*J);
       J++) {
    NumTerminators++;
    if (J->getDesc().isUnconditionalBranch() ||
        J->getDesc().isIndirectBranch()) {
      FirstUncondOrIndirectBr = J.getReverse();
    }
  }
  // If AllowModify is true, we can erase any terminators after
  // FirstUncondOrIndirectBR.
  if (AllowModify && FirstUncondOrIndirectBr != MBB.end()) {
    while (std::next(FirstUncondOrIndirectBr) != MBB.end()) {
      std::next(FirstUncondOrIndirectBr)->eraseFromParent();
      NumTerminators--;
    }
    I = FirstUncondOrIndirectBr;
  }
  // We can't handle blocks that end in an indirect branch.
  if (I->getDesc().isIndirectBranch()) {
LLVM_DEBUG(dbgs() << "\tindirect\n");
    return true;
  }
LLVM_DEBUG(dbgs() << "\tNumTerminators=" << NumTerminators << '\n');
  // We can't handle blocks with more than 2 terminators.
  if (NumTerminators > 2)
    return true;

  // Handle a single unconditional branch.
  if (NumTerminators == 1 && I->getDesc().isUnconditionalBranch()) {
    TBB = I->getOperand(0).getMBB();
LLVM_DEBUG(dbgs() << "\tunconditional\n");
    return false;
  }
  // Handle a single conditional branch.
  if (NumTerminators == 1 && I->getDesc().isConditionalBranch()) {
    parseCondBranch(*I, TBB, Cond);
LLVM_DEBUG(dbgs() << "\tconditional\n");
    return false;
  }
  // Handle a conditional branch followed by an unconditional branch.
  if (NumTerminators == 2 && std::prev(I)->getDesc().isConditionalBranch() &&
      I->getDesc().isUnconditionalBranch()) {
    parseCondBranch(*std::prev(I), TBB, Cond);
    FBB = I->getOperand(0).getMBB();
LLVM_DEBUG(dbgs() << "\ttwo-way\n");
    return false;
  }

  return true;		// can't handle this
}

unsigned MCoreInstrInfo::insertBranch(MachineBasicBlock &MBB,
		    MachineBasicBlock *TBB, MachineBasicBlock *FBB,
		    ArrayRef<MachineOperand> Cond,
		    const DebugLoc &DL, int *BytesAdded)const{
LLVM_DEBUG(dbgs() << "MCoreInstrInfo::InsertBranch: " <<
printMBBReference(MBB) << '\n');
  assert(!BytesAdded && "code size not handled");

  if (Cond.empty()) {
LLVM_DEBUG(dbgs() << "\tunconditional to " <<
printMBBReference(*TBB) << '\n');
    BuildMI(&MBB, DL, get(MCore::JMP)).addMBB(TBB);
    return 1;
  }
  // Conditional branch.
  unsigned Opc = Cond[0].getImm();
LLVM_DEBUG(dbgs() << "\tconditional " << Opc << " to " <<
printMBBReference(*TBB) << '\n');
  BuildMI(&MBB, DL, get(Opc)).addMBB(TBB).add(Cond[1]);

  // One-way conditional branch.
  if (!FBB)
    return 1;

  // Two-way conditional branch.
LLVM_DEBUG(dbgs() << "\ttwo-way to " <<
printMBBReference(*FBB) << '\n');
  BuildMI(&MBB, DL, get(MCore::JMP)).addMBB(FBB);
  return 2;
}

unsigned MCoreInstrInfo::removeBranch(MachineBasicBlock &MBB,
                                    int *BytesRemoved) const {
LLVM_DEBUG(dbgs() << "MCoreInstrInfo::removeBranch: " <<
printMBBReference(MBB) << '\n');
  assert(!BytesRemoved && "code size not handled");
  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();

  if (I == MBB.end())
    return 0;
  if (!I->getDesc().isBranch())
    return 0;
LLVM_DEBUG(dbgs() << "\tfirst " << I->getOpcode() << '\n');
  I->eraseFromParent();		// Remove the branch.
  I = MBB.end();
  if (I == MBB.begin()) return 1;
  --I;
  if (!I->getDesc().isConditionalBranch())
    return 1;
LLVM_DEBUG(dbgs() << "\tsecond " << I->getOpcode() << "\n");
  I->eraseFromParent();		// Remove the branch.
  return 2;
}

bool MCoreInstrInfo::reverseBranchCondition(
    SmallVectorImpl<MachineOperand> &Cond) const {
LLVM_DEBUG(dbgs() << "MCoreInstrInfo::reverseBranchCondition\n");
  assert((Cond.size() == 2) && "Invalid branch condition!");

  if (Cond[0].getImm() == MCore::JT) {
LLVM_DEBUG(dbgs() << "\treverse JT\n");
     Cond[0].setImm(MCore::JF);
     return false;
  } else if (Cond[0].getImm() == MCore::JF) {
LLVM_DEBUG(dbgs() << "\treverse JF\n");
     Cond[0].setImm(MCore::JT);
     return false;
  }
  // not reversible
  return true;
}
