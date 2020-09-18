//===-- MCoreMCTargetDesc.h - MCore Target Descriptions ---------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file provides MCore specific target descriptions.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_MCORE_MCTARGETDESC_MCOREMCTARGETDESC_H
#define LLVM_LIB_TARGET_MCORE_MCTARGETDESC_MCOREMCTARGETDESC_H

namespace llvm {

class Target;

} // end namespace llvm

// Defines symbolic names for MCore registers.  This defines a mapping from
// register name to register number.
//
#define GET_REGINFO_ENUM
#include "MCoreGenRegisterInfo.inc"

// Defines symbolic names for the MCore instructions.
//
#define GET_INSTRINFO_ENUM
#include "MCoreGenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "MCoreGenSubtargetInfo.inc"

#endif // LLVM_LIB_TARGET_MCORE_MCTARGETDESC_MCOREMCTARGETDESC_H
