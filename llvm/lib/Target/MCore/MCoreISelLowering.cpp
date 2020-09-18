//===-- MCoreISelLowering.cpp - MCore DAG Lowering Implementation -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements the MCoreTargetLowering class.
//
//===----------------------------------------------------------------------===//

#include "MCore.h"
#include "MCoreTargetMachine.h"
#include "MCoreMachineFunctionInfo.h"
#include "MCoreISelLowering.h"
#include "MCoreTargetMachine.h"
#include "MCoreTargetObjectFile.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/KnownBits.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>

using namespace llvm;

#define DEBUG_TYPE "mcore-lower"

//===----------------------------------------------------------------------===//
// TargetLowering Implementation
//===----------------------------------------------------------------------===//

const char *MCoreTargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch (Opcode) {
  default: return 0;
  case MCoreISD::CMP:		return "MCoreISD::CMP";
  case MCoreISD::SET0:		return "MCoreISD::SET0";
  case MCoreISD::SET1:		return "MCoreISD::SET1";
  case MCoreISD::BRCOND:	return "MCoreISD::BRCOND";
  case MCoreISD::SETCC:		return "MCoreISD::SETCC";
  case MCoreISD::SELECT:	return "MCoreISD::SELECT";
  case MCoreISD::CALL:		return "MCoreISD::CALL";
  case MCoreISD::MCRET:		return "MCoreISD::RET";
  case MCoreISD::GA:		return "MCoreISD::GA";
  }
}

static SDValue lowerCallResult(SDValue Chain, SDValue InFlag,
                               const SmallVectorImpl<CCValAssign> &RVLocs,
                               SDLoc dl, SelectionDAG &DAG,
                               SmallVectorImpl<SDValue> &InVals);


MCoreTargetLowering::MCoreTargetLowering(const TargetMachine &TM,
                                     const MCoreSubtarget &Subtarget)
    : TargetLowering(TM), Subtarget(Subtarget) {

  // Set up the register classes.
  addRegisterClass(MVT::i32, &MCore::GRegsRegClass);
  computeRegisterProperties(Subtarget.getRegisterInfo());
  setStackPointerRegisterToSaveRestore(MCore::SP);
//  setSchedulingPreference(SchedulingForRegPressure);
  setBooleanContents(ZeroOrOneBooleanContent);
  // Expand all 64-bit operations
  for (unsigned Opc = 0; Opc < ISD::BUILTIN_OP_END; ++Opc)
    setOperationAction(Opc, MVT::i64, Expand);

  setOperationAction(ISD::ADD,		MVT::i32, Legal);
  setOperationAction(ISD::SUB,		MVT::i32, Legal);
  setOperationAction(ISD::MUL,		MVT::i32, Legal);
  setOperationAction(ISD::UDIV,		MVT::i32, Legal);
  setOperationAction(ISD::SDIV,		MVT::i32, Legal);
  setOperationAction(ISD::AND,		MVT::i32, Legal);
  setOperationAction(ISD::OR,		MVT::i32, Legal);
  setOperationAction(ISD::XOR,		MVT::i32, Legal);
  setOperationAction(ISD::ABS,		MVT::i32, Legal);
  setOperationAction(ISD::SHL,		MVT::i32, Legal);
  setOperationAction(ISD::SRA,		MVT::i32, Legal);
  setOperationAction(ISD::SRL,		MVT::i32, Legal);
  setOperationAction(ISD::ROTL,		MVT::i32, Expand);
  setOperationAction(ISD::ROTR,		MVT::i32, Expand);
  setOperationAction(ISD::SHL_PARTS,	MVT::i32, Expand);
  setOperationAction(ISD::SRL_PARTS,	MVT::i32, Expand);
  setOperationAction(ISD::SRA_PARTS,	MVT::i32, Expand);
  setOperationAction(ISD::ADDCARRY,	MVT::i32, Legal);
  setOperationAction(ISD::SUBCARRY,	MVT::i32, Legal);
  setOperationAction(ISD::ABS,		MVT::i32, Legal);
  setOperationAction(ISD::SREM,		MVT::i32, Expand);
  setOperationAction(ISD::UREM,		MVT::i32, Expand);
  setOperationAction(ISD::SDIVREM,	MVT::i32, Expand);
  setOperationAction(ISD::UDIVREM,	MVT::i32, Expand);
  setOperationAction(ISD::SMUL_LOHI,	MVT::i32, Expand);
  setOperationAction(ISD::UMUL_LOHI,	MVT::i32, Expand);
  setOperationAction(ISD::MULHU,	MVT::i32, Expand);
  setOperationAction(ISD::MULHS,	MVT::i32, Expand);
  setOperationAction(ISD::MULHU,	MVT::i32, Expand);
  setOperationAction(ISD::BSWAP,	MVT::i16, Expand);
  setOperationAction(ISD::BSWAP,	MVT::i32, Expand);
  setOperationAction(ISD::BITREVERSE,	MVT::i32, Legal);
  setOperationAction(ISD::CTLZ,		MVT::i32, Legal);
  setOperationAction(ISD::CTTZ,		MVT::i32, Expand);
  setOperationAction(ISD::CTPOP,	MVT::i32, Expand);


  for (MVT VT : MVT::integer_valuetypes()) {
    setLoadExtAction(ISD::SEXTLOAD, VT, MVT::i8,  Expand);
    setLoadExtAction(ISD::SEXTLOAD, VT, MVT::i16, Expand);
    setLoadExtAction(ISD::EXTLOAD, VT,  MVT::i1,  Promote);
    setLoadExtAction(ISD::ZEXTLOAD, VT, MVT::i1,  Promote);
    setLoadExtAction(ISD::SEXTLOAD, VT, MVT::i1,  Promote);
  }
  setOperationAction(ISD::GlobalAddress, MVT::i32,   Custom);
  setOperationAction(ISD::BR_CC,         MVT::i32,   Expand);	// makes illegal
  setOperationAction(ISD::BRCOND,        MVT::Other, Custom);	// use this
  setOperationAction(ISD::SELECT_CC,     MVT::i32,   Expand);	// makes illegal
  setOperationAction(ISD::SETCC,         MVT::i32,   Custom);
  setOperationAction(ISD::SELECT,        MVT::i32,   Custom);
  setOperationAction(ISD::BR_JT,	 MVT::Other, Expand);
  setOperationAction(ISD::JumpTable,	 MVT::i32,   Custom);
}


//===----------------------------------------------------------------------===//
//                  Call Calling Convention Implementation
//===----------------------------------------------------------------------===//

#include "MCoreGenCallingConv.inc"

SDValue MCoreTargetLowering::LowerCall(TargetLowering::CallLoweringInfo &CLI,
                                     SmallVectorImpl<SDValue> &InVals) const {
LLVM_DEBUG(dbgs() << "MCoreTargetLowering::LowerCall\n");
  SelectionDAG &DAG = CLI.DAG;
  SDLoc &dl = CLI.DL;
  SmallVectorImpl<ISD::OutputArg> &Outs = CLI.Outs;
  SmallVectorImpl<SDValue> &OutVals = CLI.OutVals;
  SmallVectorImpl<ISD::InputArg> &Ins = CLI.Ins;
  SDValue Chain = CLI.Chain;
  SDValue Callee = CLI.Callee;
  bool &IsTailCall = CLI.IsTailCall;
  CallingConv::ID CallConv = CLI.CallConv;
  bool IsVarArg = CLI.IsVarArg;

  MachineFunction &MF = DAG.getMachineFunction();

  IsTailCall = false; // Do not support tail calls yet.

  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), ArgLocs,
                 *DAG.getContext());

  CCInfo.AnalyzeCallOperands(Outs, CC_MCore);

  SmallVector<CCValAssign, 16> RVLocs;
  // Analyze return values to determine the number of bytes of stack required.
  CCState RetCCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), RVLocs,
                    *DAG.getContext());
  RetCCInfo.AllocateStack(CCInfo.getNextStackOffset(), 4);
  RetCCInfo.AnalyzeCallResult(Ins, RetCC_MCore);

  // Get a count of how many bytes are to be pushed on the stack.
  unsigned NumBytes = RetCCInfo.getNextStackOffset();
  auto PtrVT = getPointerTy(DAG.getDataLayout());

  Chain = DAG.getCALLSEQ_START(Chain, NumBytes, 0, dl);

  SmallVector<std::pair<unsigned, SDValue>, 4> RegsToPass;
  SmallVector<SDValue, 12> MemOpChains;

  SDValue StackPtr;
  // Walk the register/memloc assignments, inserting copies/loads.
  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];
    SDValue Arg = OutVals[i];

    // Promote the value if needed.
    switch (VA.getLocInfo()) {
    default:
      llvm_unreachable("Unknown loc info!");
    case CCValAssign::Full:
      break;
    case CCValAssign::SExt:
      Arg = DAG.getNode(ISD::SIGN_EXTEND, dl, VA.getLocVT(), Arg);
      break;
    case CCValAssign::ZExt:
      Arg = DAG.getNode(ISD::ZERO_EXTEND, dl, VA.getLocVT(), Arg);
      break;
    case CCValAssign::AExt:
      Arg = DAG.getNode(ISD::ANY_EXTEND, dl, VA.getLocVT(), Arg);
      break;
    }

    // Arguments that can be passed on register must be kept at
    // RegsToPass vector
    if (VA.isRegLoc()) {
      RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
    } else {
      assert(VA.isMemLoc() && "Must be register or memory argument.");
      if (!StackPtr.getNode())
        StackPtr = DAG.getCopyFromReg(Chain, dl, MCore::SP,
                                      getPointerTy(DAG.getDataLayout()));
      // Calculate the stack position.
      SDValue SOffset = DAG.getIntPtrConstant(VA.getLocMemOffset(), dl);
      SDValue PtrOff = DAG.getNode(
          ISD::ADD, dl, getPointerTy(DAG.getDataLayout()), StackPtr, SOffset);

      SDValue Store =
          DAG.getStore(Chain, dl, Arg, PtrOff, MachinePointerInfo());
      MemOpChains.push_back(Store);
      IsTailCall = false;
    }
  }

  // Transform all store nodes into one single node because
  // all store nodes are independent of each other.
  if (!MemOpChains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, MemOpChains);

  // Build a sequence of copy-to-reg nodes chained together with token
  // chain and flag operands which copy the outgoing args into registers.
  // The InFlag in necessary since all emitted instructions must be
  // stuck together.
  SDValue Glue;
  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i) {
    Chain = DAG.getCopyToReg(Chain, dl, RegsToPass[i].first,
                             RegsToPass[i].second, Glue);
    Glue = Chain.getValue(1);
  }

  // If the callee is a GlobalAddress node (quite common, every direct call is)
  // turn it into a TargetGlobalAddress node so that legalize doesn't hack it.
  // Likewise ExternalSymbol -> TargetExternalSymbol.
  bool IsDirect = true;
  if (auto *G = dyn_cast<GlobalAddressSDNode>(Callee))
    Callee = DAG.getTargetGlobalAddress(G->getGlobal(), dl, MVT::i32);
  else if (auto *E = dyn_cast<ExternalSymbolSDNode>(Callee))
    Callee = DAG.getTargetExternalSymbol(E->getSymbol(), MVT::i32);
  else
    IsDirect = false;
  // Branch + Link = #chain, #target_address, #opt_in_flags...
  //             = Chain, Callee, Reg#1, Reg#2, ...
  //
  // Returns a chain & a flag for retval copy to use.
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  SmallVector<SDValue, 8> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);

  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i)
    Ops.push_back(DAG.getRegister(RegsToPass[i].first,
                                  RegsToPass[i].second.getValueType()));

  if (!IsTailCall) {
    // Add a register mask operand representing the call-preserved registers.
    const TargetRegisterInfo *TRI = Subtarget.getRegisterInfo();
    const uint32_t *Mask = TRI->getCallPreservedMask(MF, CallConv);
    assert(Mask && "Missing call preserved mask for calling convention");
    Ops.push_back(DAG.getRegisterMask(Mask));
  }

  if (Glue.getNode())
    Ops.push_back(Glue);

  Chain = DAG.getNode(IsDirect ? MCoreISD::CALL : MCoreISD::CALLI, dl, NodeTys, Ops);
  Glue = Chain.getValue(1);

  // Create the CALLSEQ_END node.
  Chain = DAG.getCALLSEQ_END(Chain, DAG.getConstant(NumBytes, dl, PtrVT, true),
                             DAG.getConstant(0, dl, PtrVT, true), Glue, dl);
  Glue = Chain.getValue(1);

  // Handle result values, copying them out of physregs into vregs that we
  // return.
  if (IsTailCall)
    return Chain;
  return lowerCallResult(Chain, Glue, RVLocs, dl, DAG, InVals);
}

static SDValue lowerCallResult(SDValue Chain, SDValue Glue,
                               const SmallVectorImpl<CCValAssign> &RVLocs,
                               SDLoc dl, SelectionDAG &DAG,
                               SmallVectorImpl<SDValue> &InVals) {
  SmallVector<std::pair<int, unsigned>, 4> ResultMemLocs;
  // Copy results out of physical registers.
  for (unsigned i = 0, e = RVLocs.size(); i != e; ++i) {
    const CCValAssign &VA = RVLocs[i];
    if (VA.isRegLoc()) {
      SDValue RetValue;
      RetValue =
          DAG.getCopyFromReg(Chain, dl, VA.getLocReg(), VA.getValVT(), Glue);
      Chain = RetValue.getValue(1);
      Glue = RetValue.getValue(2);
      InVals.push_back(RetValue);
    } else {
      assert(VA.isMemLoc() && "Must be memory location.");
      ResultMemLocs.push_back(
          std::make_pair(VA.getLocMemOffset(), InVals.size()));

      // Reserve space for this result.
      InVals.push_back(SDValue());
    }
  }

  // Copy results out of memory.
  SmallVector<SDValue, 4> MemOpChains;
  for (unsigned i = 0, e = ResultMemLocs.size(); i != e; ++i) {
    int Offset = ResultMemLocs[i].first;
    unsigned Index = ResultMemLocs[i].second;
    SDValue StackPtr = DAG.getRegister(MCore::SP, MVT::i32);
    SDValue SpLoc = DAG.getNode(ISD::ADD, dl, MVT::i32, StackPtr,
                                DAG.getConstant(Offset, dl, MVT::i32));
    SDValue Load =
        DAG.getLoad(MVT::i32, dl, Chain, SpLoc, MachinePointerInfo());
    InVals[Index] = Load;
    MemOpChains.push_back(Load.getValue(1));
  }

  // Transform all loads nodes into one single node because
  // all load nodes are independent of each other.
  if (!MemOpChains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, MemOpChains);

  return Chain;
}

namespace {
struct ArgDataPair {
  SDValue SDV;
  ISD::ArgFlagsTy Flags;
};
} // end anonymous namespace

static const MCPhysReg ArgRegs[] = {
  MCore::R2, MCore::R3, MCore::R4, MCore::R5, MCore::R6, MCore::R7
};


SDValue MCoreTargetLowering::LowerFormalArguments(
    SDValue Chain, CallingConv::ID CallConv, bool IsVarArg,
    const SmallVectorImpl<ISD::InputArg> &Ins, const SDLoc &dl,
    SelectionDAG &DAG, SmallVectorImpl<SDValue> &InVals) const {
LLVM_DEBUG(dbgs() << "MCoreTargetLowering::LowerFormalArguments\n");

  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo &MFI = MF.getFrameInfo();
  MachineRegisterInfo &RegInfo = MF.getRegInfo();
  auto *AFI = MF.getInfo<MCoreFunctionInfo>();
  // Assign locations to all of the incoming arguments.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), ArgLocs,
                 *DAG.getContext());

  CCInfo.AnalyzeFormalArguments(Ins, CC_MCore);

  unsigned StackSlotSize = 4;

  if (!IsVarArg)
    AFI->setReturnStackOffset(CCInfo.getNextStackOffset());

  // All getCopyFromReg ops must precede any getMemcpys to prevent the
  // scheduler clobbering a register before it has been copied.
  // The stages are:
  // 1. CopyFromReg (and load) arg & vararg registers.
  // 2. Chain CopyFromReg nodes into a TokenFactor.
  // 3. Memcpy 'byVal' args & push final InVals.
  // 4. Chain mem ops nodes into a TokenFactor.
  SmallVector<SDValue, 4> CFRegNode;
  SmallVector<ArgDataPair, 4> ArgData;
  SmallVector<SDValue, 4> MemOps;

  // 1a. CopyFromReg (and load) arg registers.
  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];
    SDValue ArgIn;

    if (VA.isRegLoc()) {      // Arguments passed in registers
      EVT RegVT = VA.getLocVT();
      switch (RegVT.getSimpleVT().SimpleTy) {
      default: {
        LLVM_DEBUG(errs() << "LowerFormalArguments Unhandled argument type: "
                          << (unsigned)RegVT.getSimpleVT().SimpleTy << "\n");
        llvm_unreachable("Unhandled LowerFormalArguments type.");
      }
      case MVT::i32:
        unsigned VReg = RegInfo.createVirtualRegister(&MCore::GRegsRegClass);
        RegInfo.addLiveIn(VA.getLocReg(), VReg);
        ArgIn = DAG.getCopyFromReg(Chain, dl, VReg, RegVT);
        CFRegNode.push_back(ArgIn.getValue(ArgIn->getNumValues() - 1));
      }
    } else {      		// Arguments passed in memory
      assert(VA.isMemLoc());      // sanity check
      // Load the argument to a virtual register
      unsigned ObjSize = VA.getLocVT().getStoreSize();
      assert((ObjSize <= StackSlotSize) && "Unhandled argument");

      // Create the frame index object for this incoming parameter...
      int FI = MFI.CreateFixedObject(ObjSize, VA.getLocMemOffset(), true);

      // Create the SelectionDAG nodes corresponding to a load
      // from this parameter
      SDValue FIN = DAG.getFrameIndex(FI, MVT::i32);
      ArgIn = DAG.getLoad(VA.getLocVT(), dl, Chain, FIN,
                          MachinePointerInfo::getFixedStack(MF, FI));
    }
    const ArgDataPair ADP = {ArgIn, Ins[i].Flags};
    ArgData.push_back(ADP);
  }

  // CopyFromReg vararg registers.
  if (IsVarArg) {
    // Argument registers
    auto *XFI = MF.getInfo<MCoreFunctionInfo>();
    unsigned FirstVAReg = CCInfo.getFirstUnallocated(ArgRegs);
    if (FirstVAReg < array_lengthof(ArgRegs)) {
      // Save remaining registers, storing higher register numbers at a higher
      // address
      // There are (array_lengthof(ArgRegs) - FirstVAReg) registers which
      // need to be saved.
      int VaSaveSize = (array_lengthof(ArgRegs) - FirstVAReg) * 4;
      int Offset = -VaSaveSize;
    // Record the frame index of the first variable argument
    // which is a value necessary to VASTART.
      int VaFI = MFI.CreateFixedObject(4, Offset, true);
      XFI->setVarArgsFrameIndex(VaFI);
      for (unsigned i = FirstVAReg; i < array_lengthof(ArgRegs); i++) {
        // Move argument from phys reg -> virt reg
        unsigned VReg = RegInfo.createVirtualRegister(&MCore::GRegsRegClass);
        RegInfo.addLiveIn(ArgRegs[i], VReg);
        SDValue Val = DAG.getCopyFromReg(Chain, dl, VReg, MVT::i32);
	VaFI = MFI.CreateFixedObject(4, Offset, true);
	SDValue PtrOff = DAG.getFrameIndex(VaFI, MVT::i32);
        // Move argument from virt reg -> stack
        SDValue Store =
            DAG.getStore(Chain, dl, Val, PtrOff, MachinePointerInfo());
        cast<StoreSDNode>(Store.getNode())->getMemOperand()
	    ->setValue((Value *)nullptr);
        MemOps.push_back(Store);
        Offset += 4;
      }
      XFI->setVarArgsSaveSize(VaSaveSize);
    } else {
      llvm_unreachable("Too many var args parameters.");
    }
  }


// 2. Chain CopyFromReg nodes into a TokenFactor.
//  if (!CFRegNode.empty())
//    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, CFRegNode);

  // 3. Memcpy 'byVal' args & push final InVals.
  // Aggregates passed "byVal" need to be copied by the callee.
  // The callee will use a pointer to this copy, rather than the original
  // pointer.
  for (const auto &ArgDI : ArgData) {
    if (ArgDI.Flags.isByVal() && ArgDI.Flags.getByValSize()) {
      unsigned Size = ArgDI.Flags.getByValSize();
      unsigned Align = std::max(StackSlotSize, ArgDI.Flags.getByValAlign());
      // Create a new object on the stack and copy the pointee into it.
      int FI = MFI.CreateStackObject(Size, Align, false);
      SDValue FIN = DAG.getFrameIndex(FI, MVT::i32);
      InVals.push_back(FIN);
      MemOps.push_back(DAG.getMemcpy(
          Chain, dl, FIN, ArgDI.SDV, DAG.getConstant(Size, dl, MVT::i32), Align,
          false, false, false, MachinePointerInfo(), MachinePointerInfo()));
    } else {
      InVals.push_back(ArgDI.SDV);
    }
  }

  // 4. Chain mem ops nodes into a TokenFactor.
  if (!MemOps.empty()) {
    MemOps.push_back(Chain);
    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, MemOps);
  }
  return Chain;
}

bool MCoreTargetLowering::CanLowerReturn(
    CallingConv::ID CallConv, MachineFunction &MF, bool IsVarArg,
    const SmallVectorImpl<ISD::OutputArg> &Outs, LLVMContext &Context) const {
  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CallConv, IsVarArg, MF, RVLocs, Context);
  if (!CCInfo.CheckReturn(Outs, RetCC_MCore))
    return false;
  if (CCInfo.getNextStackOffset() != 0 && IsVarArg)
    return false;
  return true;
}

SDValue
MCoreTargetLowering::LowerReturn(SDValue Chain, CallingConv::ID CallConv,
                               bool IsVarArg,
                               const SmallVectorImpl<ISD::OutputArg> &Outs,
                               const SmallVectorImpl<SDValue> &OutVals,
                               const SDLoc &dl, SelectionDAG &DAG) const {
LLVM_DEBUG(dbgs() << "MCoreTargetLowering::LowerReturn\n");
  auto *AFI = DAG.getMachineFunction().getInfo<MCoreFunctionInfo>();
  MachineFrameInfo &MFI = DAG.getMachineFunction().getFrameInfo();

  // CCValAssign - represent the assignment of
  // the return value to a location
  SmallVector<CCValAssign, 16> RVLocs;

  // CCState - Info about the registers and stack slot.
  CCState CCInfo(CallConv, IsVarArg, DAG.getMachineFunction(), RVLocs,
                 *DAG.getContext());

  // Analyze return values.
  if (!IsVarArg)
    CCInfo.AllocateStack(AFI->getReturnStackOffset(), 4);

  CCInfo.AnalyzeReturn(Outs, RetCC_MCore);

  SDValue Flag;
  SmallVector<SDValue, 4> RetOps(1, Chain);
  SmallVector<SDValue, 4> MemOpChains;
  // Handle return values that must be copied to memory.
  for (unsigned i = 0, e = RVLocs.size(); i != e; ++i) {
    CCValAssign &VA = RVLocs[i];
    if (VA.isRegLoc())
      continue;
    assert(VA.isMemLoc());
    if (IsVarArg) {
      report_fatal_error("Can't return value from vararg function in memory");
    }

    int Offset = VA.getLocMemOffset();
    unsigned ObjSize = VA.getLocVT().getStoreSize();
    // Create the frame index object for the memory location.
    int FI = MFI.CreateFixedObject(ObjSize, Offset, false);

    // Create a SelectionDAG node corresponding to a store
    // to this memory location.
    SDValue FIN = DAG.getFrameIndex(FI, MVT::i32);
    MemOpChains.push_back(DAG.getStore(
        Chain, dl, OutVals[i], FIN,
        MachinePointerInfo::getFixedStack(DAG.getMachineFunction(), FI)));
  }

  // Transform all store nodes into one single node because
  // all stores are independent of each other.
  if (!MemOpChains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other, MemOpChains);

  // Now handle return values copied to registers.
  for (unsigned i = 0, e = RVLocs.size(); i != e; ++i) {
    CCValAssign &VA = RVLocs[i];
    if (!VA.isRegLoc())
      continue;
    // Copy the result values into the output registers.
    Chain = DAG.getCopyToReg(Chain, dl, VA.getLocReg(), OutVals[i], Flag);

    // guarantee that all emitted copies are
    // stuck together, avoiding something bad
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  RetOps[0] = Chain; // Update chain.

  // Add the flag if we have it.
  if (Flag.getNode())
    RetOps.push_back(Flag);

  // What to do with the RetOps?
  return DAG.getNode(MCoreISD::MCRET, dl, MVT::Other, RetOps);
}

static unsigned HandleCC(ISD::CondCode CC,
			 bool &reverse, 	// reverse arms of compare
			 bool &invert)		// invert sense of cond. branch
{
  unsigned TCC;
  switch (CC)
  {
    case ISD::SETUEQ:
    case ISD::SETEQ:
        TCC = MCCC::CC_NE;  invert = true;   reverse = false;
        break;
    case ISD::SETUNE:
    case ISD::SETNE:
        TCC = MCCC::CC_NE;  invert = false;  reverse = false;
        break;
    case ISD::SETGE:
        TCC = MCCC::CC_LT;  invert = true;   reverse = false;
        break;
    case ISD::SETLT:
        TCC = MCCC::CC_LT;  invert = false;  reverse = false;
        break;
    case ISD::SETGT:
        TCC = MCCC::CC_LT;  invert = false;  reverse = true;
        break;
    case ISD::SETLE:
        TCC = MCCC::CC_LT;  invert = true;   reverse = true;
        break;
    case ISD::SETUGE:
        TCC = MCCC::CC_HS;  invert = false;  reverse = false;
        break;
    case ISD::SETULT:
        TCC = MCCC::CC_HS;  invert = true;   reverse = false;
        break;
    case ISD::SETULE:
        TCC = MCCC::CC_HS;  invert = false;  reverse = true;
        break;
    case ISD::SETUGT:
        TCC = MCCC::CC_HS;  invert = true;   reverse = true;
        break;
    default:
dbgs() << "Unhandled CC=" << CC << "\n";
        TCC = MCCC::CC_NE;		// avoid uninitialized warning
        break;
  }
  return TCC;
}

SDValue MCoreTargetLowering::LowerSETCC(SDValue Op, SelectionDAG &DAG) const {
LLVM_DEBUG(dbgs() << "MCoreTargetLowering::LowerSETCC\n");
  SDValue NewOp;
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  SDLoc dl(Op);
  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(2))->get();
  unsigned TCC;		// target condition code
  bool reverse;		// true if we need to swap arms
  bool invert;		// true if mvcv otherwise mvc
  TCC = HandleCC(CC, reverse, invert);
LLVM_DEBUG(dbgs() << "\tTCC=" << TCC << " reverse=" << reverse << " invert=" << invert << '\n');
  if (reverse)
    std::swap(LHS, RHS);
  SDValue NewCC = DAG.getConstant(TCC, dl, MVT::i32);
  SDValue Cmp = DAG.getNode(MCoreISD::CMP, dl, MVT::Glue, LHS, RHS, NewCC);
  SDValue Inv = DAG.getConstant((unsigned)invert, dl, MVT::i32);
  NewOp = DAG.getNode(MCoreISD::SETCC, dl, MVT::i32, Inv, Cmp);
  return NewOp;
}

SDValue MCoreTargetLowering::LowerSELECT(SDValue Op, SelectionDAG &DAG) const {
LLVM_DEBUG(dbgs() << "MCoreTargetLowering::LowerSELECT\n");
  SDValue NewOp = Op;
  SDValue Cond = Op.getOperand(0);
  SDValue TVal  = Op.getOperand(1);
  SDValue FVal  = Op.getOperand(2);
  SDLoc dl(Op);
  if (Cond.getOpcode() == MCoreISD::SETCC) {
    SDValue Inv = Cond.getOperand(0);
    SDValue Cmp = Cond.getOperand(1);
LLVM_DEBUG(dbgs() << "\tOpd=" << Cond.getOpcode() << '\n');
    NewOp = DAG.getNode(MCoreISD::SELECT, dl, MVT::i32, FVal, TVal, Inv, Cmp);
  } else if (Cond.getOpcode() != ISD::SETCC) {
    // Cond is already a boolean?
    // Have to set condition flag
    SDValue TCC = DAG.getConstant(MCCC::CC_NE, dl, MVT::i32);
    SDValue RHS = DAG.getConstant(0, dl, MVT::i32);
    SDValue Cmp = DAG.getNode(MCoreISD::CMP, dl, MVT::Glue, Cond, RHS, TCC);
    SDValue Inv = DAG.getConstant(0, dl, MVT::i32);
    NewOp = DAG.getNode(MCoreISD::SELECT, dl, MVT::i32, TVal, FVal, Inv, Cmp);
  }
  return NewOp;
}

SDValue MCoreTargetLowering::LowerBRCOND(SDValue Op, SelectionDAG &DAG) const {
LLVM_DEBUG(dbgs() << "MCoreTargetLowering::LowerBRCOND\n");
  SDValue NewOp = Op;
  SDValue Chain = Op.getOperand(0);
  SDValue Cond = Op.getOperand(1);
  SDValue Dest = Op.getOperand(2);
  SDLoc dl(Op);
LLVM_DEBUG(dbgs() << "\tCondOpcode=" << Cond.getOpcode() << '\n');
  if (Cond.getOpcode() == MCoreISD::SETCC) {
    SDValue Inv = Cond.getOperand(0);
    SDValue Cmp = Cond.getOperand(1);
    NewOp = DAG.getNode(MCoreISD::BRCOND, dl, MVT::Other, Chain, Dest, Inv, Cmp);
  } else if (Cond.getOpcode() != ISD::SETCC) {
    // Cond is already a boolean?
    // Have to set condition flag
    SDValue TCC = DAG.getConstant(MCCC::CC_NE, dl, MVT::i32);
    SDValue RHS = DAG.getConstant(0, dl, MVT::i32);
    SDValue Cmp = DAG.getNode(MCoreISD::CMP, dl, MVT::Glue, Cond, RHS, TCC);
    SDValue Inv = DAG.getConstant(0, dl, MVT::i32);
    NewOp = DAG.getNode(MCoreISD::BRCOND, dl, MVT::Other, Chain, Dest, Inv, Cmp);
  }
  return NewOp;
}

SDValue MCoreTargetLowering::LowerGlobalAddress(SDValue Op, SelectionDAG &DAG) const {
LLVM_DEBUG(dbgs() << "MCoreTargetLowering::LowerGlobalAddress\n");
  const GlobalAddressSDNode *GN = cast<GlobalAddressSDNode>(Op);
  const GlobalValue *GV = GN->getGlobal();
  int64_t Offset = GN->getOffset();
  SDLoc dl(GN);
  auto PtrVT = getPointerTy(DAG.getDataLayout());
  SDValue TGA = DAG.getTargetGlobalAddress(GV, dl, PtrVT, Offset);
  return DAG.getNode(MCoreISD::GA, dl, PtrVT, TGA);
}

SDValue MCoreTargetLowering::LowerJumpTable(SDValue Op, SelectionDAG &DAG) const {
LLVM_DEBUG(dbgs() << "MCoreTargetLowering::LowerJumpTable\n");
  JumpTableSDNode *JT = cast<JumpTableSDNode>(Op);
  SDLoc dl(JT);

//  EVT PtrVT = getPointerTy(DAG.getDataLayout());
  SDValue Result = DAG.getTargetJumpTable(JT->getIndex(), MVT::i32);
  return DAG.getNode(MCoreISD::GA, dl, MVT::i32, Result);
}

SDValue MCoreTargetLowering::LowerOperation(SDValue Op, SelectionDAG &DAG) const {
LLVM_DEBUG(dbgs() << "MCoreTargetLowering::LowerOperation: "
     << Op.getOpcode() << '\n');
  switch (Op.getOpcode()) {
    case ISD::GlobalAddress:	return LowerGlobalAddress(Op, DAG);
    case ISD::JumpTable:	return LowerJumpTable(Op, DAG);
    case ISD::BRCOND:		return LowerBRCOND(Op, DAG);
    case ISD::SELECT:		return LowerSELECT(Op, DAG);
    case ISD::SETCC:		return LowerSETCC(Op, DAG);
  default:
    llvm_unreachable("unimplemented operand");
  }
}
