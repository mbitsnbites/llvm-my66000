//==- MCoreCallingConv.td - Calling Conventions for MCore --------*- C++ -*-==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
// This describes the calling conventions for MCore architecture.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// MCore Return Value Calling Convention
//===----------------------------------------------------------------------===//
def RetCC_MCore : CallingConv<[
  // i32 are returned in registers
  CCIfType<[i32], CCAssignToReg<[R2, R3, R4, R5]>>,

  // Integer values get stored in stack slots that are 4 bytes in
  // size and 4-byte aligned.
  CCIfType<[i32], CCAssignToStack<4, 4>>
]>;


//===----------------------------------------------------------------------===//
// MCore Argument Calling Conventions
//===----------------------------------------------------------------------===//
def CC_MCore : CallingConv<[
  // Pass by value if the byval attribute is given
  CCIfByVal<CCPassByVal<4, 4>>,

  // Promote i8/i16 arguments to i32.
  CCIfType<[i8, i16], CCPromoteToType<i32>>,

  // The first 6 integer arguments are passed in integer registers.
  CCIfType<[i32], CCAssignToReg<[R2, R3, R4, R5, R6, R7]>>,

  // Integer values get stored in stack slots that are 4 bytes in
  // size and 4-byte aligned.
  CCIfType<[i32], CCAssignToStack<4, 4>>
]>;

def CSR_ABI : CalleeSavedRegs<(add R15, R14, R13, R12, R11, R10, R9, R8)>;
