//===- My66000InstrInfo.cpp - My66000 Instruction Information ---*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the My66000 implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "My66000InstrInfo.h"
#include "My66000MachineFunctionInfo.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

#define DEBUG_TYPE "my66000-instrinfo"

#define GET_INSTRINFO_CTOR_DTOR
#include "My66000GenInstrInfo.inc"

namespace llvm {
namespace My66000 {

  // My66000 Condition Codes
  enum CondCode {
    COND_TRUE,
    COND_FALSE,
    COND_INVALID
  };
}
}

// Pin the vtable to this file.
void My66000InstrInfo::anchor() {}

My66000InstrInfo::My66000InstrInfo()
  : My66000GenInstrInfo(My66000::ADJCALLSTACKDOWN, My66000::ADJCALLSTACKUP),
    RI() {
}

void My66000InstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                               MachineBasicBlock::iterator I,
                               const DebugLoc &dl, unsigned DstReg,
                               unsigned SrcReg, bool KillSrc) const {
  BuildMI(MBB, I, dl, get(My66000::MOVrr), DstReg)
      .addReg(SrcReg, getKillRegState(KillSrc));
}

void My66000InstrInfo::storeRegToStackSlot(MachineBasicBlock &MBB,
                                         MachineBasicBlock::iterator I,
                                         unsigned SrcReg, bool IsKill, int FI,
                                         const TargetRegisterClass *RC,
                                         const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end())
    DL = I->getDebugLoc();

  BuildMI(MBB, I, DL, get(My66000::STDri))
      .addReg(SrcReg, getKillRegState(IsKill))
      .addFrameIndex(FI)
      .addImm(0);
}

void My66000InstrInfo::loadRegFromStackSlot(MachineBasicBlock &MBB,
                                          MachineBasicBlock::iterator I,
                                          unsigned DstReg, int FI,
                                          const TargetRegisterClass *RC,
                                          const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (I != MBB.end())
    DL = I->getDebugLoc();

  BuildMI(MBB, I, DL, get(My66000::LDDri), DstReg)
	.addFrameIndex(FI)
	.addImm(0);
}

static inline bool IsCondBranch(unsigned Opc) {
  return Opc == My66000::BRC ||
         Opc == My66000::BRIB || Opc == My66000::BRFB || Opc == My66000::BBIT;
}

static inline bool IsUncondBranch(unsigned Opc) {
  return Opc == My66000::BRU || Opc == My66000::BRI;
}
static inline bool IsBranch(unsigned Opc) {
  return IsUncondBranch(Opc) || IsCondBranch(Opc);
}

// Pushed on the Cond Vector:
// [0] - instruction opcode (as Imm)
// [1] - register
// [2] - condition code or condition bits
static void parseCondBranch(MachineInstr &LastInst, MachineBasicBlock *&Target,
                            SmallVectorImpl<MachineOperand> &Cond) {
  // Block ends with fall-through condbranch.
  assert(LastInst.getDesc().isConditionalBranch() && "Unknown condbranch");
  switch (LastInst.getOpcode()) {
    case My66000::LOOP1rr:
    case My66000::LOOP1ri:
    case My66000::LOOP1ir:
    case My66000::LOOP1ii:
      Target = LastInst.getOperand(5).getMBB();
      Cond.push_back(MachineOperand::CreateImm(LastInst.getOpcode()));
      Cond.push_back(LastInst.getOperand(0));	// cond code/bits
      Cond.push_back(LastInst.getOperand(1));	// register
      Cond.push_back(LastInst.getOperand(2));	// increment
      Cond.push_back(LastInst.getOperand(3));	// bound
      Cond.push_back(LastInst.getOperand(4));	// loop top register
      break;
    default:
      Target = LastInst.getOperand(0).getMBB();
      Cond.push_back(MachineOperand::CreateImm(LastInst.getOpcode()));
      Cond.push_back(LastInst.getOperand(1));	// register
      Cond.push_back(LastInst.getOperand(2));	// cond code/bits
    }
}


// Analyze the branching code at the end of MBB, returning
// true if it cannot be understood (e.g. it's a switch dispatch or isn't
// implemented for a target).  Upon success, this returns false and returns
// with the following information in various cases:
//
// 1. If this block ends with no branches (it just falls through to its succ)
//    just return false, leaving TBB/FBB null.
// 2. If this block ends with only an unconditional branch, it sets TBB to be
//    the destination block.
// 3. If this block ends with a conditional branch and it falls through to a
//    successor block, it sets TBB to be the branch destination block and a
//    list of operands that evaluate the condition. These operands can be
//    passed to other TargetInstrInfo methods to create new branches.
// 4. If this block ends with a conditional branch followed by an
//    unconditional branch, it returns the 'true' destination in TBB, the
//    'false' destination in FBB, and a list of operands that evaluate the
//    condition.  These operands can be passed to other TargetInstrInfo
//    methods to create new branches.
//
// Note that RemoveBranch and InsertBranch must be implemented to support
// cases where this method returns success.
//
// If AllowModify is true, then this routine is allowed to modify the basic
// block (e.g. delete instructions after the unconditional branch).
bool My66000InstrInfo::analyzeBranch(MachineBasicBlock &MBB,
				     MachineBasicBlock *&TBB,
				     MachineBasicBlock *&FBB,
				     SmallVectorImpl<MachineOperand> &Cond,
				     bool AllowModify) const {
  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();

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
  if (I->getDesc().isIndirectBranch())
    return true;
  // We can't handle blocks with more than 2 terminators.
  if (NumTerminators > 2)
    return true;

  // Handle a single unconditional branch.
  if (NumTerminators == 1 && I->getDesc().isUnconditionalBranch()) {
    TBB = I->getOperand(0).getMBB();
    return false;
  }
  // Handle a single conditional branch.
  if (NumTerminators == 1 && I->getDesc().isConditionalBranch()) {
    parseCondBranch(*I, TBB, Cond);
    return false;
  }
  // Handle a conditional branch followed by an unconditional branch.
  if (NumTerminators == 2 && std::prev(I)->getDesc().isConditionalBranch() &&
      I->getDesc().isUnconditionalBranch()) {
    parseCondBranch(*std::prev(I), TBB, Cond);
    FBB = I->getOperand(0).getMBB();
    return false;
  }

  return true;		// can't handle this
}

// Inserts a branch into the end of the specific MachineBasicBlock, returning
// the number of instructions inserted.
unsigned My66000InstrInfo::insertBranch(
    MachineBasicBlock &MBB, MachineBasicBlock *TBB,
    MachineBasicBlock *FBB, ArrayRef<MachineOperand> Cond,
    const DebugLoc &DL, int *BytesAdded) const {
LLVM_DEBUG(dbgs() << "My66000InstrInfo::insertBranch\n");
  assert(TBB && "insertBranch must not be told to insert a fallthrough");
//  assert((Cond.size() == 1 || Cond.size() == 0) &&
//         "My66000 branch conditions should have one component!");
  assert(!BytesAdded && "code size not handled");

  if (Cond.empty()) {
    assert(!FBB && "Unconditional branch with multiple successors!");
    BuildMI(&MBB, DL, get(My66000::BRU)).addMBB(TBB);
    return 1;
  }
  // Conditional branch.
  unsigned Opc = Cond[0].getImm();
LLVM_DEBUG(dbgs() << "\tconditional " << Opc << "\n");
  switch (Opc) {
    case My66000::LOOP1rr:
    case My66000::LOOP1ri:
    case My66000::LOOP1ir:
    case My66000::LOOP1ii:
      BuildMI(&MBB, DL, get(Opc)).add(Cond[1]).add(Cond[2]).add(Cond[3])
	    .add(Cond[4]).add(Cond[5]).addMBB(TBB);
      break;
    default:
      BuildMI(&MBB, DL, get(Opc)).addMBB(TBB).add(Cond[1]).add(Cond[2]);
  }

  // One-way conditional branch.
  if (!FBB)
    return 1;

  // Two-way conditional branch.
LLVM_DEBUG(dbgs() << "\ttwo-way\n");
  MachineInstr &MI = *BuildMI(&MBB, DL, get(My66000::BRU)).addMBB(FBB);
  if (BytesAdded)
    *BytesAdded += getInstSizeInBytes(MI);
  return 2;
}

unsigned My66000InstrInfo::removeBranch(MachineBasicBlock &MBB,
                                    int *BytesRemoved) const {
LLVM_DEBUG(dbgs() << "My66000InstrInfo::removeBranch\n");
  assert(!BytesRemoved && "code size not handled");
  MachineBasicBlock::iterator I = MBB.getLastNonDebugInstr();

  if (I == MBB.end())
    return 0;
  if (!I->getDesc().isBranch())
//  if (!IsBranch(I->getOpcode()))
    return 0;
LLVM_DEBUG(dbgs() << *I);
  I->eraseFromParent();		// Remove the branch.
  I = MBB.end();
  if (I == MBB.begin()) return 1;
  --I;
  if (!I->getDesc().isConditionalBranch())
//  if (!IsCondBranch(I->getOpcode()))
    return 1;
LLVM_DEBUG(dbgs() << *I);
  I->eraseFromParent();		// Remove the branch.
  return 2;
}

unsigned My66000InstrInfo::reverseBRC(MYCC::CondCodes cc) const {
  switch (cc) {
  default:
    llvm_unreachable("Unrecognized condition code");
  case MYCC::EQ0: return MYCC::NE0;  case MYCC::NE0: return MYCC::EQ0;
  case MYCC::GE0: return MYCC::LT0;  case MYCC::LT0: return MYCC::GE0;
  case MYCC::GT0: return MYCC::LE0;  case MYCC::LE0: return MYCC::GT0;
  case MYCC::FEQ: return MYCC::FNE;  case MYCC::FNE: return MYCC::FEQ;
  case MYCC::FGE: return MYCC::FLT;  case MYCC::FLT: return MYCC::FGE;
  case MYCC::FLE: return MYCC::FGT;  case MYCC::FGT: return MYCC::FLE;
  case MYCC::FOR: return MYCC::FUN;  case MYCC::FUN: return MYCC::FOR;
  case MYCC::FEQF: return MYCC::FNEF;  case MYCC::FNEF: return MYCC::FEQF;
  case MYCC::FGEF: return MYCC::FLTF;  case MYCC::FLTF: return MYCC::FGEF;
  case MYCC::FLEF: return MYCC::FGTF;  case MYCC::FGTF: return MYCC::FLEF;
  case MYCC::FORF: return MYCC::FUNF;  case MYCC::FUNF: return MYCC::FORF;
  }
}

unsigned My66000InstrInfo::reverseBRIB(MYCB::CondBits cb) const {
  switch (cb) {
  default:
    llvm_unreachable("Unrecognized condition bit");
  case MYCB::NE: return MYCB::EQ;  case MYCB::EQ: return MYCB::NE;
  case MYCB::GT: return MYCB::LE;  case MYCB::LE: return MYCB::GT;
  case MYCB::GE: return MYCB::LT;  case MYCB::LT: return MYCB::GE;
  case MYCB::HI: return MYCB::LS;  case MYCB::LS: return MYCB::HI;
  case MYCB::LO: return MYCB::HS;  case MYCB::HS: return MYCB::LO;
  }
}

bool My66000InstrInfo::reverseBranchCondition(
    SmallVectorImpl<MachineOperand> &Cond) const {
LLVM_DEBUG(dbgs() << "My66000InstrInfo::reverseBranchCondition\n");
  assert((Cond.size() == 3) && "Invalid branch condition!");
  if (Cond[0].getImm() == My66000::BRC) {
//dbgs() << "\tBRC " << Cond[2].getImm() << "\n";
     MYCC::CondCodes cc = static_cast<MYCC::CondCodes>(Cond[2].getImm());
     Cond[2].setImm(reverseBRC(cc));
     return false;
  } else if (Cond[0].getImm() == My66000::BRIB) {
//dbgs() << "\tBRB " << Cond[2].getImm() << "\n";
     MYCB::CondBits cb = static_cast<MYCB::CondBits>(Cond[2].getImm());
     Cond[2].setImm(reverseBRIB(cb));
     return false;
  }
  // BRFB, BBIT not reversible
  return true;
}
