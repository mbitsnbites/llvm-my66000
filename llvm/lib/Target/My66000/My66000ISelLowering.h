//===- My66000ISelLowering.h - My66000 DAG Lowering Interface ---*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the interfaces that My66000 uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MY66000_MY66000ISELLOWERING_H
#define LLVM_LIB_TARGET_MY66000_MY66000ISELLOWERING_H

#include "My66000.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/TargetLowering.h"

namespace llvm {

// Forward delcarations
class My66000Subtarget;
class My66000TargetMachine;

namespace My66000ISD {

enum NodeType : unsigned {
  // Start the numbering where the builtin ops and target ops leave off.
  FIRST_NUMBER = ISD::BUILTIN_OP_END,
  RET,		// Return with a flag operand. Operand 0 is the chain operand.
  CALL,		// Branch and link (direct call)
  CALLI,	// Branch and link thru register (indirect call)
  CMP,		// CMP
  EXT,		// Extract
  CMOV,		// Conditional move
  BRcc,		// BRcc i.e. branch bit set by CMP
  BRcond,	// branch compare with zero
  WRAPPER	// prefix for global address
};

} // end namespace My66000ISD

//===--------------------------------------------------------------------===//
// TargetLowering Implementation
//===--------------------------------------------------------------------===//
class My66000TargetLowering : public TargetLowering {
 public:
  explicit My66000TargetLowering(const TargetMachine &TM,
			const My66000Subtarget &Subtarget);

  /// Provide custom lowering hooks for some operations.
  SDValue LowerOperation(SDValue Op, SelectionDAG &DAG) const override;

  // getTargetNodeName - This method returns the name of a target specific DAG node.
  const char *getTargetNodeName(unsigned Opcode) const override;
  bool isLegalAddressingMode(const DataLayout &DL, const AddrMode &AM,
			Type *Ty, unsigned AS,
			Instruction *I = nullptr) const override;


 private:
  const My66000Subtarget &Subtarget;
  SDValue LowerBR_CC(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerGlobalAddress(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerFRAMEADDR(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSELECT_CC(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSETCC(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerJumpTable(SDValue Op, SelectionDAG &DAG) const;
  SDValue LowerSIGN_EXTEND_INREG(SDValue Op, SelectionDAG &DAG) const;

  SDValue PerformDAGCombine(SDNode *N, DAGCombinerInfo &DCI) const override;

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

//    bool mayBeEmittedAsTailCall(const CallInst *CI) const override;
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_MY66000_MY66000ISELLOWERING_H
