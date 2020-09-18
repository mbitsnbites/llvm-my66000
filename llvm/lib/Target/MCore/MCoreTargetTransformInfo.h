//===-- MCoreTargetTransformInfo.h - MCore specific TTI ---------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// This file a TargetTransformInfo::Concept conforming object specific to the
/// MCore target machine. It uses the target's detailed information to
/// provide more precise answers to certain TTI queries, while letting the
/// target independent and default TTI implementations handle the rest.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MCORE_MCORETARGETTRANSFORMINFO_H
#define LLVM_LIB_TARGET_MCORE_MCORETARGETTRANSFORMINFO_H

#include "MCore.h"
#include "MCoreTargetMachine.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/CodeGen/BasicTTIImpl.h"
#include "llvm/CodeGen/TargetLowering.h"

namespace llvm {

class MCoreTTIImpl : public BasicTTIImplBase<MCoreTTIImpl> {
  typedef BasicTTIImplBase<MCoreTTIImpl> BaseT;
  typedef TargetTransformInfo TTI;
  friend BaseT;

  const MCoreSubtarget *ST;
  const MCoreTargetLowering *TLI;

  const MCoreSubtarget *getST() const { return ST; }
  const MCoreTargetLowering *getTLI() const { return TLI; }

public:
  explicit MCoreTTIImpl(const MCoreTargetMachine *TM, const Function &F)
      : BaseT(TM, F.getParent()->getDataLayout()), ST(TM->getSubtargetImpl()),
        TLI(ST->getTargetLowering()) {}

};

} // end namespace llvm

#endif
