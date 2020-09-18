//===- MCoreRegisterInfo.td - Describe the MCore Registers ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

class MCReg<bits<4> num, string n, list<Register> subregs = []> : Register<n> {
  field bits<4> Num;
  let Namespace = "MCore";
  let SubRegs = subregs;
}

def SP  : MCReg< 0, "sp">,  DwarfRegNum<[0]>;
def R1  : MCReg< 1, "r1">,  DwarfRegNum<[1]>;
def R2  : MCReg< 2, "r2">,  DwarfRegNum<[2]>;
def R3  : MCReg< 3, "r3">,  DwarfRegNum<[3]>;
def R4  : MCReg< 4, "r4">,  DwarfRegNum<[4]>;
def R5  : MCReg< 5, "r5">,  DwarfRegNum<[5]>;
def R6  : MCReg< 6, "r6">,  DwarfRegNum<[6]>;
def R7  : MCReg< 7, "r7">,  DwarfRegNum<[7]>;
def R8  : MCReg< 8, "r8">,  DwarfRegNum<[8]>;
def R9  : MCReg< 9, "r9">,  DwarfRegNum<[9]>;
def R10 : MCReg<10, "r10">, DwarfRegNum<[10]>;
def R11 : MCReg<11, "r11">, DwarfRegNum<[11]>;
def R12 : MCReg<12, "r12">, DwarfRegNum<[12]>;
def R13 : MCReg<13, "r13">, DwarfRegNum<[13]>;
def R14 : MCReg<14, "r14">, DwarfRegNum<[14]>;
def R15 : MCReg<15, "r15">, DwarfRegNum<[15]>;
def LR  : MCReg<15, "lr">,  DwarfRegNum<[15]>;
def R1D : MCReg< 1, "r1">,  DwarfRegNum<[1]>;
def PSR : MCReg<16, "psr">, DwarfRegNum<[16]>;

def GRegs : RegisterClass<"MCore", [i32], 32,
   (add (sequence "R%u", 2, 7),		// callee saved
    (sequence "R%u", 14, 8),	// caller saved
    R15,	// LR
    R1,	// really want to save that for fixups
    SP
   )>;

// This register class is used by the divide instructions
def GR1: RegisterClass<"MCore", [i32], 32, (add R1D)>;

let CopyCost = -1 in
def CBit: RegisterClass<"MCore", [i32], 32, (add PSR)>;

def SpillCBit: RegisterClass<"MCore", [i32], 32,
    (add GRegs, CBit)>;
