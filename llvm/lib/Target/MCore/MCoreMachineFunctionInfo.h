//=== MCoreMachineFunctionInfo.h - MCore machine function info -------*- C++ -*-==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares MCore-specific per-machine-function information.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MCORE_MCOREMACHINEFUNCTIONINFO_H
#define LLVM_LIB_TARGET_MCORE_MCOREMACHINEFUNCTIONINFO_H

#include "MCoreFrameLowering.h"
#include "MCoreISelLowering.h"
#include "MCoreInstrInfo.h"
#include "MCoreSelectionDAGInfo.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include <cassert>
#include <utility>
#include <vector>

namespace llvm {

/// MCoreFunctionInfo - This class is derived from MachineFunction private
/// MCore target-specific information for each MachineFunction.
class MCoreFunctionInfo : public MachineFunctionInfo {
  unsigned ReturnStackOffset;
  bool ReturnStackOffsetSet = false;
  int VarArgsFrameIndex = 0;
  int VarArgsSaveSize = 0;
  mutable int CachedEStackSize = -1;
  std::vector<std::pair<MachineBasicBlock::iterator, CalleeSavedInfo>>
  SpillLabels;
  unsigned LoSavedReg;
  unsigned HiSavedReg;
  bool SpillsCBit = false;

  virtual void anchor();

public:
  MCoreFunctionInfo() = default;

  explicit MCoreFunctionInfo(MachineFunction &MF) {}

  ~MCoreFunctionInfo() override = default;

  void setVarArgsFrameIndex(int off) { VarArgsFrameIndex = off; }
  int getVarArgsFrameIndex() const { return VarArgsFrameIndex; }

  unsigned getVarArgsSaveSize() const { return VarArgsSaveSize; }
  void setVarArgsSaveSize(int Size) { VarArgsSaveSize = Size; }

  void setReturnStackOffset(unsigned value) {
    assert(!ReturnStackOffsetSet && "Return stack offset set twice");
    ReturnStackOffset = value;
    ReturnStackOffsetSet = true;
  }

  unsigned getReturnStackOffset() const {
    assert(ReturnStackOffsetSet && "Return stack offset not set");
    return ReturnStackOffset;
  }

  void setSpillsCBit() { SpillsCBit = true; }
  bool isCBitSpilled() const { return SpillsCBit; }

  std::vector<std::pair<MachineBasicBlock::iterator, CalleeSavedInfo>> &
  getSpillLabels() {
    return SpillLabels;
  }

  // Get and set the first call-saved GPR that should be saved and restored
  // by this function.  This is 0 if no GPRs need to be saved or restored.
  unsigned getLoSavedReg() const { return LoSavedReg; }
  void setLoSavedReg(unsigned Reg) { LoSavedReg = Reg; }

  // Get and set the last call-saved GPR that should be saved and restored
  // by this function.
  unsigned getHiSavedReg() const { return HiSavedReg; }
  void setHiSavedReg(unsigned Reg) { HiSavedReg = Reg; }
};

} // end namespace llvm

#endif // LLVM_LIB_TARGET_MCORE_MCOREMACHINEFUNCTIONINFO_H
