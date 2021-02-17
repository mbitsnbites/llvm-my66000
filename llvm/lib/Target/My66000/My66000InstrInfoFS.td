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
		{ let PrintMethod = "printFP32Operand"; }

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
// Caution: the next two semantics change with changes to IEEE-754
defm FMAXNF: SPUrr<"fmaxf", 0b110110, 0b00010, fmaxnum>;
defm FMINNF: SPUrr<"fminf", 0b110111, 0b00010, fminnum>;
defm FMAXF : SPUrr<"fmaxf", 0b110110, 0b00010, fmaximum>;
defm FMINF : SPUrr<"fminf", 0b110111, 0b00010, fminimum>;

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

// Transcendentals
def SQRTF :  TRAN<0b001101, 0b00010,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"fsqrtf\t$rd,$rs1",
	[(set (f32 GRegs:$rd), (fsqrt (f32 GRegs:$rs1)))]>;
def RSQRTF : TRAN<0b001100, 0b00010,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"frsqrtf\t$rd,$rs1",
	[]>;
def FLOG2F : TRAN<0b000000, 0b00010,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"fln2f\t$rd,$rs1",
	[(set (f32 GRegs:$rd), (flog2 (f32 GRegs:$rs1)))]>;
def FLOGF :  TRAN<0b000001, 0b00010,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"flnf\t$rd,$rs1",
	[(set (f32 GRegs:$rd), (flog (f32 GRegs:$rs1)))]>;
def FLOG10F: TRAN<0b000010, 0b00010,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"flogf\t$rd,$rs1",
	[(set (f32 GRegs:$rd), (flog10 (f32 GRegs:$rs1)))]>;
def FEXP2F : TRAN<0b000100, 0b00010,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"fexp2f\t$rd,$rs1",
	[(set (f32 GRegs:$rd), (fexp2 (f32 GRegs:$rs1)))]>;
def FEXPF :  TRAN<0b000101, 0b00010,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"fexpf\t$rd,$rs1",
	[(set (f32 GRegs:$rd), (fexp (f32 GRegs:$rs1)))]>;
def FEXP10F: TRAN<0b000110, 0b00010,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"fexp10f\t$rd,$rs1",
	[]>;				// no ISD opcode
def FSINF :  TRAN<0b011000, 0b00010,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"fsinf\t$rd,$rs1",
	[(set (f32 GRegs:$rd), (fsin (f32 GRegs:$rs1)))]>;
def FCOSF :  TRAN<0b011001, 0b00010,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"fcosf\t$rd,$rs1",
	[(set (f32 GRegs:$rd), (fcos (f32 GRegs:$rs1)))]>;
def FTANF :  TRAN<0b011010, 0b00010,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"ftanf\t$rd,$rs1",
	[]>;				// no ISD opcode
def FASINF : TRAN<0b011100, 0b00010,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"fasinf\t$rd,$rs1",
	[]>;				// no ISD opcode
def FACOSF : TRAN<0b011101, 0b00010,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"facosf\t$rd,$rs1",
	[]>;				// no ISD opcode
def FATANF : TRAN<0b011110, 0b00010,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"fatanf\t$rd,$rs1",
	[]>;				// no ISD opcode

// Loads and Stores
let mayLoad = 1 in {
  defm LDF : Load<"lduw", 0b100010, 0b000010, f32, load>;
}

let mayStore = 1 in {
  defm STF : Store<"stw", 0b101010, 0b001010, f32, store>;
}

