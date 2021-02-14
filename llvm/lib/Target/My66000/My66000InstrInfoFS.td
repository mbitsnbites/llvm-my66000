//===- My66000InstrInfoFS.td - FP Instr Description for My66000 ----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

//
// 32-bit floating point stuff
//
def fpimm32 : Operand<f32>, FPImmLeaf<f32, [{ return true; }]>
		{ let PrintMethod = "printFP64Operand"; }

multiclass SPUrr<string OpName, bits<6>OpC, bits<5>Flags,
		   SDPatternOperator OpNode> {
  def rr : ALU_RR<OpC, Flags,
	(outs GRegs:$rd), (ins GRegs:$rs1, GRegs:$rs2),
	!strconcat(OpName, "\t$rd,$rs1,$rs2"),
	[(set (f32 GRegs:$rd), (OpNode (f32 GRegs:$rs1), (f32 GRegs:$rs2)))]>;
  def rn : ALU_RR<OpC, Flags,
	(outs GRegs:$rd), (ins GRegs:$rs1, GRegs:$rs2),
	!strconcat(OpName, "\t$rd,$rs1,-$rs2"),
	[(set (f32 GRegs:$rd), (OpNode (f32 GRegs:$rs1), (fneg (f32 GRegs:$rs2))))]>;
}
defm FADDF : SPUrr<"faddf", 0b110001, 0b00010, fadd>;
defm FMULF : SPUrr<"fmulf", 0b110010, 0b00010, fmul>;
defm FDIVF : SPUrr<"fdivf", 0b110011, 0b00010, fdiv>;

let isCompare = 1 in {
def FCMPFrr : ALU_RR<0b110100, 0b00010,
	(outs GRegs:$rd), (ins GRegs:$rs1, GRegs:$rs2),
	"fcmpf\t$rd,$rs1,$rs2",
	[(set (i64 GRegs:$rd), (myFCMP (f32 GRegs:$rs1), (f32 GRegs:$rs2)))]>;
}

def FSUBF : Pat<(fsub (f32 GRegs:$rs1), (f32 GRegs:$rs2)),
		 (FADDFrn GRegs:$rs1, GRegs:$rs2)>;
def       : Pat<(fneg (fmul (f32 GRegs:$rs1), (f32 GRegs:$rs2))),
		(FMULFrn GRegs:$rs1, GRegs:$rs2)>;

multiclass SPUrs<string OpName, bits<6>OpC, bits<5>Flags,
		   SDPatternOperator OpNode> {
  def rs : ALU_RD<OpC, Flags,
	(outs GRegs:$rd), (ins GRegs:$rs1, fpimm32:$imm),
	!strconcat(OpName, "\t$rd,$rs1,#$imm"),
	[(set GRegs:$rd, (OpNode (f32 GRegs:$rs1), fpimm32:$imm))]>;
}
defm FADDF : SPUrs<"faddf", 0b110001, 0b10010, fadd>;
defm FMULF : SPUrs<"fmulf", 0b110010, 0b10010, fmul>;
defm FDIVF : SPUrs<"fdivf", 0b110011, 0b10010, fdiv>;

let isCompare = 1 in {
def FCMPFrs : ALU_RD<0b110100, 0b10001,
	(outs GRegs:$rd), (ins GRegs:$rs1, fpimm32:$imm),
	"fcmpf\t$rd,$rs1,#$imm",
	[(set (i64 GRegs:$rd), (myFCMP (f32 GRegs:$rs1), fpimm32:$imm))]>;
}

def FNEGF : ALU_DR<0b110001, 0b10111,
	(outs GRegs:$rd), (ins GRegs:$rs2),
	"faddf\t$rd,#0,-$rs2",
	[(set GRegs:$rd, (fneg (f32 GRegs:$rs2)))]>;

// Moves
def FMOVrs : ALU_RRR<0b000, 0b111,
	(outs GRegs:$rd), (ins fpimm32:$imm),
	"mov\t$rd,#$imm",
	[(set (f32 GRegs:$rd), fpimm32:$imm)]>
	{ let rs1 = 0b00000; let rs2 = 0b00000; let rs3 = 0b00000; }
def : Pat<(myCMOV (f32 GRegs:$rs1), (f32 GRegs:$rs2), GRegs:$rs3),
          (CMOVrrr GRegs:$rs1, GRegs:$rs2, GRegs:$rs3)>;
def : Pat<(myMUX (f32 GRegs:$rs1), (f32 GRegs:$rs2), GRegs:$rs3),
          (MUXrrr GRegs:$rs1, GRegs:$rs2, GRegs:$rs3)>;

// Converts
def CVTfd : CVT<0b010001, 0b00000,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"cvtfd\t$rd,$rs1",
	[(set (f64 GRegs:$rd), (fpextend (f32 GRegs:$rs1)))]>;
def CVTdf : CVT<0b110001, 0b00000,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"cvtdf\t$rd,$rs1",
	[(set (f32 GRegs:$rd), (fpround (f64 GRegs:$rs1)))]>;
def CVTuf : CVT<0b110000, 0b00000,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"cvtuf\t$rd,$rs1",
	[(set (f32 GRegs:$rd), (uint_to_fp GRegs:$rs1))]>;
def CVTsf : CVT<0b111000, 0b00000,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"cvtsf\t$rd,$rs1",
	[(set (f32 GRegs:$rd), (sint_to_fp GRegs:$rs1))]>;
def CVTfu : CVT<0b001000, 0b00000,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"cvtfu\t$rd,$rs1",
	[(set (i64 GRegs:$rd), (fp_to_uint (f32 GRegs:$rs1)))]>;
def CVTfs : CVT<0b101000, 0b00000,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"cvtfs\t$rd,$rs1",
	[(set (i64 GRegs:$rd), (fp_to_sint (f32 GRegs:$rs1)))]>;

// Loads and Stores
let mayLoad = 1 in {
  defm LDF : Load<"lduw", 0b100010, 0b000010, f32, load>;
}

let mayStore = 1 in {
  defm STF : Store<"stw", 0b101010, 0b001010, f32, store>;
}

