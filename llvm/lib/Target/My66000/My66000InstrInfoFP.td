//===- My66000InstrInfoFP.td - FP Instr Description for My66000 ----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

//
// Floating point stuff
//
def fpimm64 : Operand<f64>, FPImmLeaf<f64, [{ return true; }]>
		{ let PrintMethod = "printFP64Operand"; }

def myFCMP : SDNode<"My66000ISD::FCMP",
			SDTypeProfile<1, 2, [SDTCisVT<0, i64>,
			    SDTCisVT<1, f64>, SDTCisSameAs<1, 2>]>,
			[]>;
def myBRfcc :	    SDNode<"My66000ISD::BRfcc", SDT_BRcc,
			[SDNPHasChain]>;

defm FADD : ALUrr<"fadd", 0b110001, 0b00000, fadd>;
defm FMUL : ALUrr<"fmul", 0b110010, 0b00000, fmul>;
defm FDIV : ALUrr<"fdiv", 0b110011, 0b00000, fdiv>;
// Caution: the next two semantics change with changes to IEEE-754
defm FMAX : ALUrr<"fmax", 0b110110, 0b00000, fmaxnum>;
defm FMIN : ALUrr<"fmin", 0b110111, 0b00000, fminnum>;
// Can't use the standard pattern because the output type doesn't match inputs.
let isCompare = 1 in {
def FCMPrr : ALU_RR<0b110100, 0b00000,
	(outs GRegs:$rd), (ins GRegs:$rs1, GRegs:$rs2),
	"fcmp\t$rd,$rs1,$rs2",
	[(set (i64 GRegs:$rd), (myFCMP (f64 GRegs:$rs1), (f64 GRegs:$rs2)))]>;
}

def FSUB   : Pat<(fsub GRegs:$rs1, GRegs:$rs2),
		 (FADDrn GRegs:$rs1, GRegs:$rs2)>;
def        : Pat<(fmul GRegs:$rs1, (fneg GRegs:$rs2)),
		 (FMULrn GRegs:$rs1, GRegs:$rs2)>;

multiclass FPUrd<string OpName, bits<6>OpC, bits<5>Flags,
		   SDPatternOperator OpNode> {
  def rd : ALU_RD<OpC, Flags,
	(outs GRegs:$rd), (ins GRegs:$rs1, fpimm64:$imm),
	!strconcat(OpName, "\t$rd,$rs1,#$imm"),
	[(set GRegs:$rd, (OpNode GRegs:$rs1, fpimm64:$imm))]>;
}
defm FADD : FPUrd<"fadd", 0b110001, 0b10001, fadd>;
defm FMUL : FPUrd<"fmul", 0b110010, 0b10001, fmul>;
defm FDIV : FPUrd<"fdiv", 0b110011, 0b10001, fdiv>;
let isCompare = 1 in {
def FCMPrd : ALU_RD<0b110100, 0b10001,
	(outs GRegs:$rd), (ins GRegs:$rs1, fpimm64:$imm),
	"fcmp\t$rd,$rs1,#$imm",
	[(set (i64 GRegs:$rd), (myFCMP (f64 GRegs:$rs1), fpimm64:$imm))]>;
}

/*
multiclass FPUdr<string OpName, bits<6>OpC, bits<5>Flags,
		   SDPatternOperator OpNode> {
  def dr : ALU_DR<OpC, Flags,
	(outs GRegs:$rd), (ins fpimm64:$imm, fpimm64:$imm),
	!strconcat(OpName, "\t$rd,#$imm,$rs1"),
	[(set GRegs:$rd, (OpNode fpimm64:$imm, GRegs:$rs2))]>;
}
defm FDIV : FPUdr<"fdiv", 0b110011, 0b10001, fdiv>;
*/
def FDIVdr : ALU_DR<0b110011, 0b10101,
	(outs GRegs:$rd), (ins fpimm64:$imm, GRegs:$rs2),
	"fdiv\t$rd,#$imm,$rs2",
	[(set GRegs:$rd, (fdiv fpimm64:$imm, GRegs:$rs2))]>;
def FSUBdr : ALU_DR<0b110001, 0b10101,
	(outs GRegs:$rd), (ins fpimm64:$imm, GRegs:$rs2),
	"fadd\t$rd,#$imm,-$rs2",
	[(set GRegs:$rd, (fsub fpimm64:$imm, GRegs:$rs2))]>;

// FIXME - use 32-bit immediate when implemented
// Can this be a Pat<>?
def FNEG : ALU_DR<0b110001, 0b10101,
	(outs GRegs:$rd), (ins GRegs:$rs2),
	"fadd\t$rd,#0,-$rs2",
	[(set GRegs:$rd, (fneg GRegs:$rs2))]>;

// Moves
def FMOVrd : ALU_RRR<0b000, 0b111,
	(outs GRegs:$rd), (ins fpimm64:$imm),
	"mov\t$rd,#$imm",
	[(set GRegs:$rd, fpimm64:$imm)]>
	{ let rs1 = 0b00000; let rs2 = 0b00000; let rs3 = 0b00000; }
def : Pat<(myCMOV (f64 GRegs:$rs1), (f64 GRegs:$rs2), GRegs:$rs3),
          (CMOVrrr GRegs:$rs1, GRegs:$rs2, GRegs:$rs3)>;
def : Pat<(myMUX (f64 GRegs:$rs1), (f64 GRegs:$rs2), GRegs:$rs3),
          (MUXrrr GRegs:$rs1, GRegs:$rs2, GRegs:$rs3)>;

// Converts
def CVTud : CVT<0b010000, 0b00000,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"cvtud\t$rd,$rs1",
	[(set (f64 GRegs:$rd), (uint_to_fp GRegs:$rs1))]>;
def CVTsd : CVT<0b011000, 0b00000,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"cvtsd\t$rd,$rs1",
	[(set (f64 GRegs:$rd), (sint_to_fp GRegs:$rs1))]>;
def CVTdu : CVT<0b000000, 0b00000,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"cvtdu\t$rd,$rs1",
	[(set (i64 GRegs:$rd), (fp_to_uint (f64 GRegs:$rs1)))]>;
def CVTds : CVT<0b100000, 0b00000,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"cvtds\t$rd,$rs1",
	[(set (i64 GRegs:$rd), (fp_to_sint (f64 GRegs:$rs1)))]>;

// Transcendentals
def SQRT : TRAN<0b001101, 0b00000,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"fsqrt\t$rd,$rs1",
	[(set (f64 GRegs:$rd), (fsqrt (f64 GRegs:$rs1)))]>;

// These bitcasts are NOPs
def       : Pat<(bitconvert (f64 GRegs:$rs)),
		(i64 GRegs:$rs)>;
def       : Pat<(bitconvert (i64 GRegs:$rs)),
		(f64 GRegs:$rs)>;

// Loads and Stores
let mayLoad = 1 in {
  defm LDG : Load<"ldd", 0b100011, 0b000011, f64, load>;
}

let mayStore = 1 in {
  defm STG : Store<"std", 0b101011, 0b001011, f64, store>;
}

