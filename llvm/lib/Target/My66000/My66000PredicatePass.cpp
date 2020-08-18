//===-- My66000PredicatePass.cpp - Transform to Predicated Code -----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "My66000.h"
#include "My66000MachineFunctionInfo.h"
#include "My66000Subtarget.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineInstrBundle.h"
#include "llvm/CodeGen/MachineOperand.h"

using namespace llvm;

#define DEBUG_TYPE "my66000-predicate"
#define PASS_NAME "My66000 predicate transform pass"

static cl::opt<bool> EnablePred("enable-predication", cl::Hidden,
  cl::desc("Enable predication instructions"));

STATISTIC(NumPREDs,        "Number of predicated blocks inserted");


namespace {
  class My66000PredBlock : public MachineFunctionPass {
  public:
    static char ID;
    const My66000InstrInfo *TII;

    My66000PredBlock() : MachineFunctionPass(ID) {}

    bool runOnMachineFunction(MachineFunction &MF) override;

    StringRef getPassName() const override {
      return PASS_NAME;
    }
  private:
    bool InsertPredInstructions(MachineBasicBlock *MBB);
    bool onePass(MachineFunction &MF);
    void getConditionInfo(SmallVector<MachineOperand, 4> &Cond,
			bool invert, unsigned &op, unsigned &cc, unsigned &reg);
    int checkBlock(MachineBasicBlock *MBB);
    bool Convert(MachineBasicBlock *Head,
		 MachineBasicBlock *TBB, MachineBasicBlock *FBB,
		 MachineBasicBlock *Tail, SmallVector<MachineOperand, 4> &Cond);
//    bool Triangle(BBInfo &BBIA, BBInfo &BBIB);
//    bool Diamond(BBInfo &BBIA, BBInfo &BBIB, BBInfo &BBIC);
  };

} // end anonymous namespace

char My66000PredBlock::ID = 0;

INITIALIZE_PASS(My66000PredBlock, DEBUG_TYPE, PASS_NAME, false, false)


// This procedure cribbed from EarlyIfConversion

bool My66000PredBlock::InsertPredInstructions(MachineBasicBlock *MBB) {
LLVM_DEBUG(dbgs() << "My66000PredBlock::InsertPredInstructions\n");
  bool Modified = false;
  MachineBasicBlock *Head, *TBB, *FBB, *Tail;	// In a class?
  SmallVector<MachineOperand, 4> Cond;		// In a class?
  Head = MBB;
  TBB = FBB = Tail = nullptr;
LLVM_DEBUG(dbgs() << "\tHead:  " << printMBBReference(*Head) << '\n');
  if (Head->succ_size() != 2)
    return false;
  MachineBasicBlock *Succ0 = Head->succ_begin()[0];
  MachineBasicBlock *Succ1 = Head->succ_begin()[1];

  // Canonicalize so Succ0 has Head as its single predecessor.
  if (Succ0->pred_size() != 1) {
LLVM_DEBUG(dbgs() << "\tswapped arms\n");
    std::swap(Succ0, Succ1);
  }
  if (Succ0->pred_size() != 1)
    return false;
  if (Succ0->succ_size() == 1) {
    Tail = Succ0->succ_begin()[0];
LLVM_DEBUG(dbgs() << "\tTail:  " << printMBBReference(*Tail) << '\n');

    if (Tail == Succ1) {
LLVM_DEBUG(dbgs() << "\ttriangle\n");
    } else {
      // Check for a diamond. We won't deal with any critical edges.
      if (Succ1->pred_size() == 1 && Succ1->succ_size() == 1 &&
          Succ1->succ_begin()[0] == Tail) {
LLVM_DEBUG(dbgs() << "\tdiamond\n");
      } else {
        return false;
      }
    }
    // We have a simple triangle or diamond
    Cond.clear();
    if (TII->analyzeBranch(*Head, TBB, FBB, Cond, false)) {
LLVM_DEBUG(dbgs() << "Branch not analyzable.\n");
      return false;
    }
    if (!TBB) { // This is weird, probably some sort of degenerate CFG.
LLVM_DEBUG(dbgs() << "AnalyzeBranch didn't find conditional branch.\n");
      return false;
    }
    // Make sure the analyzed branch is conditional; one of the successors
    // could be a landing pad. (Empty landing pads can be generated on Windows.)
    if (Cond.empty()) {
LLVM_DEBUG(dbgs() << "AnalyzeBranch found an unconditional branch.\n");
      return false;
    }
    if (FBB) {
      if (FBB == Head->getFallThrough()) {
LLVM_DEBUG(dbgs() << "\tcond/uncond branch pair, uncond branch to fallthru?\n");
        MachineBasicBlock::iterator I = Head->getLastNonDebugInstr();
        I->eraseFromParent();		// Remove the branch.
      }
    }
    // AnalyzeBranch doesn't set FBB on a fall-through branch.
    FBB = TBB == Succ0 ? Succ1 : Succ0;

    Modified = Convert(Head, TBB, FBB, Tail, Cond);

  } else {
LLVM_DEBUG(dbgs() << "\tpossible && or ||\n");
    MachineBasicBlock *Head1 = Succ0;
    if (Head1->succ_size() != 2)
      return false;
    if (Head1->succ_begin()[0] == Succ1) {
      Succ0 = Head1->succ_begin()[1];
    } else {
      Succ0 = Head1->succ_begin()[0];
    }
    // Canonicalize so Succ0 has Head1 as its single predecessor.
    if (Succ0->pred_size() != 1) {
LLVM_DEBUG(dbgs() << "\tswapped arms\n");
      std::swap(Succ0, Succ1);
    }
LLVM_DEBUG(dbgs() << "\tHead1:  " << printMBBReference(*Head1) << '\n');
LLVM_DEBUG(dbgs() << "\tSucc0: " << printMBBReference(*Succ0) <<
" #P=" << Succ0->pred_size() << " #S=" << Succ0->succ_size() << '\n');
LLVM_DEBUG(dbgs() << "\tSucc1: " << printMBBReference(*Succ1) <<
" #P=" << Succ1->pred_size() << " #S=" << Succ1->succ_size() << '\n');
    if (Succ1->pred_size() != 2)
      return false;
    if (Succ0->succ_size() != 1)
      return false;
    Tail = Succ0->succ_begin()[0];
    if (Tail == Succ1) {
LLVM_DEBUG(dbgs() << "\tTriangle2\n");
    } else {
      if (Succ1->succ_size() != 1)
	return false;
LLVM_DEBUG(dbgs() << "\tDiamond2\n");
    }
    return false;
  }

  if (Modified) NumPREDs += 1;
  return Modified;
}

// Count how many valid instructions can be predicated.
// If theres a call that is not at the end, return 0.
// Any branch should be at the end.
int My66000PredBlock::checkBlock(MachineBasicBlock *MBB) {
  MachineBasicBlock::iterator I = MBB->begin();
  MachineBasicBlock::iterator E = MBB->getFirstTerminator();
  unsigned NumInstrs = 0;

  while (I != E) {
    MachineInstr *MI = &*I;
    // FIXME - why are CFI_INSTRUCTIONs in the code?
    // answer: because of tail merged RETs
    if (!MI->isCFIInstruction()) {
      if (MI->isCall() && I != E) return -1;
      NumInstrs += 1;
    }
    ++I;
  }
  return NumInstrs;
}

void My66000PredBlock::getConditionInfo(SmallVector<MachineOperand, 4> &Cond,
			bool invert, unsigned &op, unsigned &cc, unsigned &reg) {

  unsigned brop = Cond[0].getImm();
  reg = Cond[1].getReg();
  cc = Cond[2].getImm();
  switch (brop) {
    case My66000::BRC:
      op = My66000::PRC;
      if (invert)
        cc = TII->reverseBRC(static_cast<MYCC::CondCodes>(cc));
      break;
    case My66000::BRIB:
      op = My66000::PRIB;
      if (invert)
        cc = TII->reverseBRIB(static_cast<MYCB::CondBits>(cc));
      break;
  }
}

bool My66000PredBlock::Convert(MachineBasicBlock *Head,
			MachineBasicBlock *TBB, MachineBasicBlock *FBB,
			MachineBasicBlock *Tail,
			SmallVector<MachineOperand, 4> &Cond) {

LLVM_DEBUG(dbgs() << "My66000PredBlock::Convert\n");
LLVM_DEBUG(dbgs() << "\tTBB=" << printMBBReference(*TBB) << '\n');
LLVM_DEBUG(dbgs() << "\tFBB=" << printMBBReference(*FBB) << '\n');
  // See how many instructions we can shadow
  int ninstrsT, ninstrsF;
  if (TBB == Tail)
    ninstrsT = 0;
  else
    ninstrsT = checkBlock(TBB);
  if (FBB == Tail)
    ninstrsF = 0;
  else
    ninstrsF = checkBlock(FBB);
LLVM_DEBUG(dbgs() << "\tninstr=" << ninstrsT << ',' << ninstrsF << '\n');
  if (ninstrsT < 0 || ninstrsF < 0 ||	// unpredicatable instructions
     (ninstrsT == 0 && ninstrsF == 0) || ninstrsT+ninstrsF > 8)
    return false;

  MachineBasicBlock::iterator IP = Head->getFirstTerminator();
  DebugLoc dl = IP->getDebugLoc();

  unsigned cc;
  unsigned prop;
  unsigned reg;
  bool invert = false;	// FIXME
  getConditionInfo(Cond, invert, prop, cc, reg);

  // Create the predicate instruction
  MachineInstrBuilder MIB = BuildMI(*Head, IP, dl, TII->get(prop));
  MIB.addImm(cc);
  MIB.addReg(reg);
  MIB.addImm(((ninstrsT+ninstrsF-1)<<8) | ((1 << ninstrsT)-1));

  // Move all instructions into Head, except for the terminators.
  if (TBB != Tail)
    Head->splice(IP, TBB, TBB->begin(), TBB->getFirstTerminator());
  if (FBB != Tail)
    Head->splice(IP, FBB, FBB->begin(), FBB->getFirstTerminator());

  // Are there extra Tail predecessors?
  bool ExtraPreds = Tail->pred_size() != 2;

  // Fix up the CFG, temporarily leave Head without any successors.
  Head->removeSuccessor(TBB);
  Head->removeSuccessor(FBB, true);
  if (TBB != Tail)
    TBB->removeSuccessor(Tail, true);
  if (FBB != Tail)
    FBB->removeSuccessor(Tail, true);

  // Fix up Head's terminators.
  // It should become a single branch or a fallthrough.
  DebugLoc HeadDL = Head->getFirstTerminator()->getDebugLoc();
LLVM_DEBUG(dbgs() << "\tremove branch from Head\n");
  TII->removeBranch(*Head);
  if (Head->getFirstTerminator() != nullptr) {
LLVM_DEBUG(dbgs() << "\tperhaps 2 branches were in head?\n");
  }

  // Erase the now empty conditional blocks. It is likely that Head can fall
  // through to Tail, and we can join the two blocks.
  if (TBB != Tail) {
    TBB->eraseFromParent();
  }
  if (FBB != Tail) {
    FBB->eraseFromParent();
  }

  assert(Head->succ_empty() && "Additional head successors?");
  if (!ExtraPreds && Head->isLayoutSuccessor(Tail)) {
    // Splice Tail onto the end of Head.
LLVM_DEBUG(dbgs() << "\tjoining tail " << printMBBReference(*Tail)
           << " into head " << printMBBReference(*Head) << '\n');
    Head->splice(Head->end(), Tail, Tail->begin(), Tail->end());
    Head->transferSuccessors(Tail);
    Tail->eraseFromParent();
  } else {
    // We need a branch to Tail, let code placement work it out later.
LLVM_DEBUG(dbgs() << "\tconverting to unconditional branch.\n");
    SmallVector<MachineOperand, 0> EmptyCond;
    TII->insertBranch(*Head, Tail, nullptr, EmptyCond, HeadDL);
    Head->addSuccessor(Tail);
  }
  return true;  // FIXME - temp
}


bool My66000PredBlock::onePass(MachineFunction &MF) {
  // If we did any inserts, blocks may have been deleted so
  // we must start at the beginning again.
  for (auto &MBB : MF ) {
    if (InsertPredInstructions(&MBB))
      return true;
  }
  return false;
}

bool My66000PredBlock::runOnMachineFunction(MachineFunction &MF) {
  TII = MF.getSubtarget<My66000Subtarget>().getInstrInfo();

  if (!EnablePred) return false;
LLVM_DEBUG(dbgs() << "My66000PredBlock::runOnMachineFunction\n");
// begin debug
LLVM_DEBUG(dbgs() << "*** Original basic blocks ***\n");
    for (auto &MBB : MF ) {
      LLVM_DEBUG(dbgs() << MBB);
    }
// end debug
  bool Modified = false;
  bool Mod;
  do {
    Mod = onePass(MF);
    Modified |= Mod;
// begin debug
  if (Mod) {
LLVM_DEBUG(dbgs() << "*** Modified basic blocks ***\n");
    for (auto &MBB : MF ) {
      LLVM_DEBUG(dbgs() << MBB);
    }
  }
// end debug
  } while (Mod);
  return Modified;
}

/// createMy66000PredBlock - Returns an instance of the My66000PredBlock
/// insertion pass.
FunctionPass *llvm::createMy66000PredBlockPass() { return new My66000PredBlock(); }
