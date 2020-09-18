//===- MCore.td - Describe the MCore Target Machine -------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Target-independent interfaces which we are implementing
//===----------------------------------------------------------------------===//

include "llvm/Target/Target.td"

include "MCoreRegisterInfo.td"
include "MCoreCallingConv.td"
include "MCoreInstrInfo.td"

def MCoreInstrInfo : InstrInfo;


//===----------------------------------------------------------------------===//
// MCore processors supported.
//===----------------------------------------------------------------------===//

class Proc<string Name, list<SubtargetFeature> Features>
 : Processor<Name, NoItineraries, Features>;

def : Proc<"generic",        []>;
def : Proc<"mcore",          []>;
//def : Proc<"M200",           [FeatureX]>;


//===----------------------------------------------------------------------===//
// Declare the target which we are implementing
//===----------------------------------------------------------------------===//

def MCore : Target {
  // Pull in Instruction Info:
  let InstructionSet = MCoreInstrInfo;
}

