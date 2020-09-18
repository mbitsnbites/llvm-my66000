//===-- MCoreMCAsmInfo.cpp - MCore asm properties -------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "MCoreMCAsmInfo.h"
using namespace llvm;

void MCoreMCAsmInfo::anchor() { }

MCoreMCAsmInfo::MCoreMCAsmInfo(const Triple &TT) {
  SupportsDebugInformation = true;
  Data16bitsDirective = "\t.short\t";
  Data32bitsDirective = "\t.long\t";
  Data64bitsDirective = "\t.quad\t";
  ZeroDirective = "\t.space\t";
  AscizDirective = "\t.asciz\t";
  CommentString = "//";


  HiddenVisibilityAttr = MCSA_Invalid;
  HiddenDeclarationVisibilityAttr = MCSA_Invalid;
  ProtectedVisibilityAttr = MCSA_Invalid;

  // Debug
  ExceptionsType = ExceptionHandling::DwarfCFI;
  DwarfRegNumForCFI = true;

  UsesNonexecutableStackSection = false;
}

