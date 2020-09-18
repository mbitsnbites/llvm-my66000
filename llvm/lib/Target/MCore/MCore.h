//===-- MCore.h - Top-level interface for MCore representation --*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in the LLVM
// MCore back-end.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MCORE_MCORE_H
#define LLVM_LIB_TARGET_MCORE_MCORE_H

#include "MCTargetDesc/MCoreMCTargetDesc.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
  class FunctionPass;
  class TargetMachine;
  class MCoreTargetMachine;

  FunctionPass *createMCoreISelDag(MCoreTargetMachine &TM,
                                   CodeGenOpt::Level OptLevel);

  // Enums corresponding to MCore compare types.
  // These values must be kept in sync with the ones in the .td file.
  namespace MCCC {
    enum CondCodes {
      CC_NE  =  0,
      CC_LT  =  1,
      CC_HS  =  2
   };
  }

  inline static const char *MCoreCondCodeToString(MCCC::CondCodes CC) {
    switch (CC) {
    default: assert(0 && "Unknown condition code");
    case MCCC::CC_NE:  return "ne";
    case MCCC::CC_LT:  return "lt";
    case MCCC::CC_HS:  return "hs";
    }
  }
}  // end namespace llvm
#endif
