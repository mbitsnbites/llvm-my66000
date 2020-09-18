//===-- MCoreTargetInfo.cpp - MCore Target Implementation -------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "TargetInfo/MCoreTargetInfo.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

Target &llvm::getTheMCoreTarget() {
  static Target TheMCoreTarget;
  return TheMCoreTarget;
}

extern "C" void LLVMInitializeMCoreTargetInfo() {
  RegisterTarget<Triple::mcore> X(getTheMCoreTarget(), "mcore", "MCore",
				    "MCore");
}
