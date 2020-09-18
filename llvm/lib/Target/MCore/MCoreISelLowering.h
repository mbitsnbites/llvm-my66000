//===-- MCoreISelLowering.h - MCore DAG Lowering Interface ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the interfaces that MCore uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#ifndef MCOREISELLOWERING_H
#define MCOREISELLOWERING_H

#include "MCore.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/CodeGen/SelectionDAG.h"

namespace llvm {

namespace MCoreISD {
  enum {
    FIRST_NUMBER = ISD::BUILTIN_OP_END,
    CMP,	// Compare two operands, set flag
    SET0,	// Set T-bit (carry) to 0
    SET1,	// Set T-bit (carry) to 1
    BRCOND,	// Branch to dest conditional on flag
    SETCC,	// Move the flag to a register
    SELECT,	// Select between two values using the current flag
    CALL,	// A call instruction
    CALLI,	// A call via register instruction
    MCRET,	// Return
    GA		// Wrapper for global address
  };
}

  class MCoreTargetLowering : public TargetLowering {
  public:
    MCoreTargetLowering(const TargetMachine &TM,
			const MCoreSubtarget &Subtarget);

  /// Provide custom lowering hooks for some operations.
  SDValue LowerOperation(SDValue Op, SelectionDAG &DAG) const override;

  // getTargetNodeName - This method returns the name of a target specific DAG node.
  const char *getTargetNodeName(unsigned Opcode) const override;
/*
    virtual MachineBasicBlock *
	InsertAtEndOfBasicBlock(MachineInstr *MI, MachineBasicBlock *MBB);
    virtual MachineBasicBlock *
	EmitInstrWithCustomInserter(MachineInstr *MI, MachineBasicBlock *MBB);
    virtual void
	computeMaskedBitsForTargetNode(const SDOperand Op,
                                       const APInt &Mask,
                                       APInt &KnownZero, APInt &KnownOne,
                                       const SelectionDAG &DAG,
                                       unsigned Depth = 0) const;
*/
  private:
  const MCoreSubtarget &Subtarget;
  SDValue LowerBRCOND(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerGlobalAddress(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerJumpTable(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSELECT(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSETCC(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerADDC(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSUBC(SDValue Op, SelectionDAG &DAG) const;

  // Lower Operand helpers
  SDValue LowerFormalArguments(SDValue Chain, CallingConv::ID CallConv,
			bool isVarArg,
			const SmallVectorImpl<ISD::InputArg> &Ins,
			const SDLoc &dl, SelectionDAG &DAG,
			SmallVectorImpl<SDValue> &InVals) const override;
  SDValue LowerCall(TargetLowering::CallLoweringInfo &CLI,
			SmallVectorImpl<SDValue> &InVals) const override;
  SDValue LowerReturn(SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
			const SmallVectorImpl<ISD::OutputArg> &Outs,
			const SmallVectorImpl<SDValue> &OutVals, const SDLoc &dl,
			SelectionDAG &DAG) const override;
  bool CanLowerReturn(CallingConv::ID CallConv, MachineFunction &MF,
			bool isVarArg,
			const SmallVectorImpl<ISD::OutputArg> &ArgsFlags,
			LLVMContext &Context) const override;
  };
}

#endif

