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
def fpimm32 : Operand<f32>, FPImmLeaf<f32, [{ return true; }]>
		{ let PrintMethod = "printFP32Operand"; }
def fpimm1 : PatLeaf<(fpimm), [{ return N->isExactlyValue(+1.0); }]>;

def myFCMP : SDNode<"My66000ISD::FCMP",
			SDTypeProfile<1, 2, [SDTCisVT<0, i64>,
			    SDTCisSameAs<1, 2>]>,
			[]>;
def myBRfcc :	    SDNode<"My66000ISD::BRfcc", SDT_BRcc,
			[SDNPHasChain]>;

defm FADD :  ALUrr<"fadd",  0b110001, 0b00000, f64, fadd>;
defm FADDF : ALUrr<"faddf", 0b110001, 0b00010, f32, fadd>;
defm FMUL :  ALUrr<"fmul",  0b110010, 0b00000, f64, fmul>;
defm FMULF : ALUrr<"fmulf", 0b110010, 0b00010, f32, fmul>;
defm FDIV :  ALUrr<"fdiv",  0b110011, 0b00000, f64, fdiv>;
defm FDIVF : ALUrr<"fdivf", 0b110011, 0b00010, f32, fdiv>;
// Caution: the following semantics change with changes to IEEE-754
defm FMAXN:  ALUrr<"fmax",  0b110110, 0b00000, f64, fmaxnum>;
defm FMAXNF: ALUrr<"fmaxf", 0b110110, 0b00010, f32, fmaxnum>;
defm FMINN:  ALUrr<"fmin",  0b110111, 0b00000, f64, fminnum>;
defm FMINNF: ALUrr<"fminf", 0b110111, 0b00010, f32, fminnum>;
defm FMAX :  ALUrr<"fmax",  0b110110, 0b00000, f64, fmaximum>;
defm FMAXF : ALUrr<"fmaxf", 0b110110, 0b00010, f32, fmaximum>;
defm FMIN :  ALUrr<"fmin",  0b110111, 0b00000, f64, fminimum>;
defm FMINF : ALUrr<"fminf", 0b110111, 0b00010, f32, fminimum>;

// Can't use the standard pattern because the output type doesn't match inputs.
let isCompare = 1 in {
def FCMPrr : ALU_RR<0b110100, 0b00000,
	(outs GRegs:$rd), (ins GRegs:$rs1, GRegs:$rs2),
	"fcmp\t$rd,$rs1,$rs2",
	[(set (i64 GRegs:$rd), (myFCMP (f64 GRegs:$rs1), (f64 GRegs:$rs2)))]>;
def FCMPFrr : ALU_RR<0b110100, 0b00010,
	(outs GRegs:$rd), (ins GRegs:$rs1, GRegs:$rs2),
	"fcmpf\t$rd,$rs1,$rs2",
	[(set (i64 GRegs:$rd), (myFCMP (f32 GRegs:$rs1), (f32 GRegs:$rs2)))]>;
}

def FSUB  : Pat<(fsub (f64 GRegs:$rs1), (f64 GRegs:$rs2)),
		(FADDrn GRegs:$rs1, GRegs:$rs2)>;
def FSUBF : Pat<(fsub (f32 GRegs:$rs1), (f32 GRegs:$rs2)),
		(FADDFrn GRegs:$rs1, GRegs:$rs2)>;
def NFADD : Pat<(fsub (fneg (f64 GRegs:$rs1)), (f64 GRegs:$rs2)),
		(FADDnn GRegs:$rs1, GRegs:$rs2)>;
def NFADDF: Pat<(fsub (fneg (f32 GRegs:$rs1)), (f32 GRegs:$rs2)),
		(FADDFnn GRegs:$rs1, GRegs:$rs2)>;
def       : Pat<(fmul (f64 GRegs:$rs1), (fneg (f64 GRegs:$rs2))),
		(FMULrn GRegs:$rs1, GRegs:$rs2)>;
def       : Pat<(fneg (fmul (f32 GRegs:$rs1), (f32 GRegs:$rs2))),
		(FMULFrn GRegs:$rs1, GRegs:$rs2)>;
def       : Pat<(fdiv (f64 GRegs:$rs1), (fneg (f64 GRegs:$rs2))),
		(FDIVrn GRegs:$rs1, GRegs:$rs2)>;
def       : Pat<(fdiv (f32 GRegs:$rs1), (fneg (f32 GRegs:$rs2))),
		(FDIVFrn GRegs:$rs1, GRegs:$rs2)>;
def       : Pat<(fdiv (fneg (f64 GRegs:$rs1)), (f64 GRegs:$rs2)),
		(FDIVnr GRegs:$rs1, GRegs:$rs2)>;
def       : Pat<(fdiv (fneg (f32 GRegs:$rs1)), (f32 GRegs:$rs2)),
		(FDIVFnr GRegs:$rs1, GRegs:$rs2)>;

// There must be an integer version to do this, maybe a shift.
def FABS  : Pat<(fabs (f64 GRegs:$rs1)),
		(FMAXrn GRegs:$rs1, GRegs:$rs1)>;
def FABSF : Pat<(fabs (f32 GRegs:$rs1)),
		(FMAXFrn GRegs:$rs1, GRegs:$rs1)>;

multiclass FPUrd<string OpName, bits<6>OpC, bits<5>Flags,
		   SDPatternOperator OpNode> {
  def rd : ALU_RD<OpC, Flags,
	(outs GRegs:$rd), (ins GRegs:$rs1, fpimm64:$imm),
	!strconcat(OpName, "\t$rd,$rs1,#$imm"),
	[(set GRegs:$rd, (OpNode (f64 GRegs:$rs1), fpimm64:$imm))]>;
}
multiclass FPUrs<string OpName, bits<6>OpC, bits<5>Flags,
		   SDPatternOperator OpNode> {
  def rs : ALU_RW<OpC, Flags,
	(outs GRegs:$rd), (ins GRegs:$rs1, fpimm32:$imm),
	!strconcat(OpName, "\t$rd,$rs1,#$imm"),
	[(set GRegs:$rd, (OpNode (f32 GRegs:$rs1), fpimm32:$imm))]>;
}

defm FADD :  FPUrd<"fadd",  0b110001, 0b10001, fadd>;
defm FADDF : FPUrs<"faddf", 0b110001, 0b10010, fadd>;
defm FMUL :  FPUrd<"fmul",  0b110010, 0b10001, fmul>;
defm FMULF : FPUrs<"fmulf", 0b110010, 0b10010, fmul>;
defm FDIV :  FPUrd<"fdiv",  0b110011, 0b10001, fdiv>;
defm FDIVF : FPUrs<"fdivf", 0b110011, 0b10010, fdiv>;

let isCompare = 1 in {
def FCMPrd : ALU_RD<0b110100, 0b10001,
	(outs GRegs:$rd), (ins GRegs:$rs1, fpimm64:$imm),
	"fcmp\t$rd,$rs1,#$imm",
	[(set (i64 GRegs:$rd), (myFCMP (f64 GRegs:$rs1), fpimm64:$imm))]>;
def FCMPFrs : ALU_RD<0b110100, 0b10001,
	(outs GRegs:$rd), (ins GRegs:$rs1, fpimm32:$imm),
	"fcmpf\t$rd,$rs1,#$imm",
	[(set (i64 GRegs:$rd), (myFCMP (f32 GRegs:$rs1), fpimm32:$imm))]>;
}

def FDIVdr : ALU_DR<0b110011, 0b10101,
	(outs GRegs:$rd), (ins fpimm64:$imm, GRegs:$rs2),
	"fdiv\t$rd,#$imm,$rs2",
	[(set GRegs:$rd, (fdiv fpimm64:$imm, (f64 GRegs:$rs2)))]>;
def FDIVFsr: ALU_WR<0b110011, 0b10010,
	(outs GRegs:$rd), (ins fpimm32:$imm, GRegs:$rs1),
	"fdivf\t$rd,#$imm,$rs1",
	[(set GRegs:$rd, (fdiv fpimm32:$imm, (f32 GRegs:$rs1)))]>;

def FSUBdr : ALU_DR<0b110001, 0b10101,
	(outs GRegs:$rd), (ins fpimm64:$imm, GRegs:$rs2),
	"fadd\t$rd,#$imm,-$rs2",
	[(set GRegs:$rd, (fsub fpimm64:$imm, (f64 GRegs:$rs2)))]>;
def FNEG : ALU_DR<0b110001, 0b10101,
	(outs GRegs:$rd), (ins GRegs:$rs2),
	"fadd\t$rd,#0,-$rs2",
	[(set GRegs:$rd, (fneg (f64 GRegs:$rs2)))]>;
def FNEGF : ALU_DR<0b110001, 0b10111,
	(outs GRegs:$rd), (ins GRegs:$rs2),
	"faddf\t$rd,#0,-$rs2",
	[(set GRegs:$rd, (fneg (f32 GRegs:$rs2)))]>;

// FMA 64-bit FMA - not used. but what the hardware really does
def FMArr : ALU_RRR<0b000, 0b000,
	(outs GRegs:$rd), (ins GRegs:$rs1, GRegs:$rs2, GRegs:$rs3),
	"fmac\t$rd,$rs1,$rs2,$rs3",
	[(set GRegs:$rd, (fma (f64 GRegs:$rs1), (f64 GRegs:$rs2), (f64 GRegs:$rs3)))]>;
// FMA 64-bit FMAD - preferred by DAGCombiner
def FMADrr : ALU_RRR<0b000, 0b000,
	(outs GRegs:$rd), (ins GRegs:$rs1, GRegs:$rs2, GRegs:$rs3),
	"fmac\t$rd,$rs1,$rs2,$rs3",
	[(set GRegs:$rd, (fmad (f64 GRegs:$rs1), (f64 GRegs:$rs2), (f64 GRegs:$rs3)))]>;
def FMADrn : ALU_RRR<0b000, 0b001,
	(outs GRegs:$rd), (ins GRegs:$rs1, GRegs:$rs2, GRegs:$rs3),
	"fmac\t$rd,$rs1,$rs2,-$rs3",
	[(set GRegs:$rd, (fmad (f64 GRegs:$rs1), (f64 GRegs:$rs2), (fneg (f64 GRegs:$rs3))))]>;
def FMADnr : ALU_RRR<0b000, 0b010,
	(outs GRegs:$rd), (ins GRegs:$rs1, GRegs:$rs2, GRegs:$rs3),
	"fmac\t$rd,$rs2,-$rs1,$rs3",	// must swap rs1 and rs2
	[(set GRegs:$rd, (fmad (fneg GRegs:$rs1), (f64 GRegs:$rs2), (f64 GRegs:$rs3)))]>;
def FMADnn : ALU_RRR<0b000, 0b010,
	(outs GRegs:$rd), (ins GRegs:$rs1, GRegs:$rs2, GRegs:$rs3),
	"fmac\t$rd,$rs2,-$rs1,-$rs3",	// must swap rs1 and rs2
	[(set GRegs:$rd, (fmad (fneg GRegs:$rs1), (f64 GRegs:$rs2), (fneg (f64 GRegs:$rs3))))]>;
def FMADrd : ALU_RRR<0b000, 0b000,
	(outs GRegs:$rd), (ins GRegs:$rs1, GRegs:$rs2, fpimm64:$imm),
	"fmac\t$rd,$rs1,$rs2,#$imm",
	[(set GRegs:$rd, (fmad (f64 GRegs:$rs1), (f64 GRegs:$rs2), fpimm64:$imm))]>;
def FMADdr : ALU_RRR<0b000, 0b000,
	(outs GRegs:$rd), (ins GRegs:$rs1, fpimm64:$imm, GRegs:$rs3),
	"fmac\t$rd,$rs1,#$imm,$rs3",
	[(set GRegs:$rd, (fmad (f64 GRegs:$rs1), fpimm64:$imm, (f64 GRegs:$rs3)))]>;

// FMA 32-bit FMAD - preferred by DAGCombiner
def FMADFrr : ALU_RRR<0b000, 0b000,
	(outs GRegs:$rd), (ins GRegs:$rs1, GRegs:$rs2, GRegs:$rs3),
	"fmacf\t$rd,$rs1,$rs2,$rs3",
	[(set GRegs:$rd, (fmad (f32 GRegs:$rs1), (f32 GRegs:$rs2), (f32 GRegs:$rs3)))]>;
def FMADFrn : ALU_RRR<0b000, 0b001,
	(outs GRegs:$rd), (ins GRegs:$rs1, GRegs:$rs2, GRegs:$rs3),
	"fmacf\t$rd,$rs1,$rs2,-$rs3",
	[(set GRegs:$rd, (fmad (f32 GRegs:$rs1), (f32 GRegs:$rs2), (fneg (f32 GRegs:$rs3))))]>;
def FMADFnr : ALU_RRR<0b000, 0b010,
	(outs GRegs:$rd), (ins GRegs:$rs1, GRegs:$rs2, GRegs:$rs3),
	"fmacf\t$rd,$rs2,-$rs1,$rs3",	// must swap rs1 and rs2
	[(set GRegs:$rd, (fmad (fneg GRegs:$rs1), (f32 GRegs:$rs2), (f32 GRegs:$rs3)))]>;
def FMADFnn : ALU_RRR<0b000, 0b010,
	(outs GRegs:$rd), (ins GRegs:$rs1, GRegs:$rs2, GRegs:$rs3),
	"fmacf\t$rd,$rs2,-$rs1,-$rs3",	// must swap rs1 and rs2
	[(set GRegs:$rd, (fmad (fneg GRegs:$rs1), (f32 GRegs:$rs2), (fneg (f32 GRegs:$rs3))))]>;
def FMADFrd : ALU_RRR<0b000, 0b000,
	(outs GRegs:$rd), (ins GRegs:$rs1, GRegs:$rs2, fpimm32:$imm),
	"fmacf\t$rd,$rs1,$rs2,#$imm",
	[(set GRegs:$rd, (fmad (f32 GRegs:$rs1), (f32 GRegs:$rs2), fpimm32:$imm))]>;
def FMADFdr : ALU_RRR<0b000, 0b000,
	(outs GRegs:$rd), (ins GRegs:$rs1, fpimm32:$imm, GRegs:$rs3),
	"fmacf\t$rd,$rs1,#$imm,$rs3",
	[(set GRegs:$rd, (fmad (f32 GRegs:$rs1), fpimm32:$imm, (f32 GRegs:$rs3)))]>;

// Moves
def FMOVrd : ALU_RRR<0b000, 0b111,
	(outs GRegs:$rd), (ins fpimm64:$imm),
	"mov\t$rd,#$imm",
	[(set (f64 GRegs:$rd), fpimm64:$imm)]>
	{ let rs1 = 0b00000; let rs2 = 0b00000; let rs3 = 0b00000; }
def FMOVrs : ALU_RRR<0b000, 0b111,
	(outs GRegs:$rd), (ins fpimm32:$imm),
	"mov\t$rd,#$imm",
	[(set (f32 GRegs:$rd), fpimm32:$imm)]>
	{ let rs1 = 0b00000; let rs2 = 0b00000; let rs3 = 0b00000; }
def : Pat<(myCMOV (f64 GRegs:$rs1), (f64 GRegs:$rs2), GRegs:$rs3),
          (CMOVrrr GRegs:$rs1, GRegs:$rs2, GRegs:$rs3)>;
def : Pat<(myCMOV (f32 GRegs:$rs1), (f32 GRegs:$rs2), GRegs:$rs3),
          (CMOVrrr GRegs:$rs1, GRegs:$rs2, GRegs:$rs3)>;
def : Pat<(myMUX (f64 GRegs:$rs1), (f64 GRegs:$rs2), GRegs:$rs3),
          (MUXrrr GRegs:$rs1, GRegs:$rs2, GRegs:$rs3)>;
def : Pat<(myMUX (f32 GRegs:$rs1), (f32 GRegs:$rs2), GRegs:$rs3),
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

// Rounds
multiclass RNDrr<string OpName, bits<5>OpC, bits<5>OpF, ValueType VT,
	    SDPatternOperator OpNode> {
    def rr : RND<OpC, OpF,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	!strconcat(OpName, "\t$rd,$rs1"),
	[(set (VT GRegs:$rd), (OpNode (VT GRegs:$rs1)))]>;
}
defm RNDRM : RNDrr<"rndrm", 0b00001, 0b00000, f64, fnearbyint>;
defm RNDRMF: RNDrr<"rndrmf",0b00001, 0b00100, f32, fnearbyint>;
defm RNDAZ : RNDrr<"rndaz", 0b00100, 0b00000, f64, fround>;
defm RNDAZF: RNDrr<"rndazf",0b00100, 0b00100, f32, fround>;
defm RNDTZ : RNDrr<"rndtz", 0b00101, 0b00000, f64, ftrunc>;
defm RNDTZF: RNDrr<"rndtzf",0b00101, 0b00100, f32, ftrunc>;
defm RNDPI : RNDrr<"rndpi", 0b00110, 0b00000, f64, fceil>;
defm RNDPIF: RNDrr<"rndpif",0b00110, 0b00100, f32, fceil>;
defm RNDNI : RNDrr<"rndni", 0b00111, 0b00000, f64, ffloor>;
defm RNDNIF: RNDrr<"rndnif",0b00111, 0b00100, f32, ffloor>;
def RNDNE : RND<0b00000, 0b00000,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"rndne\t$rd,$rs1",
	[]>;
def RNDNEF: RND<0b00000, 0b00100,
	(outs GRegs:$rd), (ins GRegs:$rs1),
	"rndnef\t$rd,$rs1",
	[]>;

//
// Transcendentals
//
// For some reason some transcendentals are not pre-defined
def fexp :  SDNode<"ISD::FEXP", SDTFPUnaryOp>;
def flog :  SDNode<"ISD::FLOG", SDTFPUnaryOp>;
def flog10: SDNode<"ISD::FLOG10", SDTFPUnaryOp>;

// transcendentals defined in LLVM
multiclass FTRAN<string OpName,  bits<6>OpC, bits<5>Flags, ValueType VT,
		   SDPatternOperator OpNode> {
  def x : TRAN<OpC, Flags,
	    (outs GRegs:$rd), (ins GRegs:$rs1),
	    !strconcat(OpName, "\t$rd,$rs1"),
	    [(set (VT GRegs:$rd), (OpNode (VT GRegs:$rs1)))]>;
}

// transcendentals not defined in LLVM
multiclass UTRAN<string OpName,  bits<6>OpC, bits<5>Flags, ValueType VT> {
  def u : TRAN<OpC, Flags,
	    (outs GRegs:$rd), (ins GRegs:$rs1),
	    !strconcat(OpName, "\t$rd,$rs1"),
	    []>;
}

defm SQRT :   FTRAN<"fsqrt",  0b001101, 0b00000, f64, fsqrt>;
defm SQRTF :  FTRAN<"fsqrtf", 0b001101, 0b00010, f32, fsqrt>;
defm FLOG2 :  FTRAN<"fln2",   0b000000, 0b00000, f64, flog2>;
defm FLOG2F : FTRAN<"fln2f",  0b000000, 0b00010, f32, flog2>;
defm FLOG :   FTRAN<"fln",    0b000001, 0b00000, f64, flog>;
defm FLOGF :  FTRAN<"flnf",   0b000001, 0b00010, f32, flog>;
defm FLOG10 : FTRAN<"flog",   0b000010, 0b00000, f64, flog10>;
defm FLOG10F: FTRAN<"flogf",  0b000010, 0b00010, f32, flog10>;
defm FEXP2 :  FTRAN<"fexp2",  0b000100, 0b00000, f64, fexp2>;
defm FEXP2F : FTRAN<"fexp2f", 0b000100, 0b00010, f32, fexp2>;
defm FEXP :   FTRAN<"fexp",   0b000101, 0b00000, f64, fexp>;
defm FEXPF :  FTRAN<"fexpf",  0b000101, 0b00010, f32, fexp>;
defm FSIN :   FTRAN<"fsin",   0b011000, 0b00000, f64, fsin>;
defm FSINF :  FTRAN<"fsinf",  0b011000, 0b00010, f32, fsin>;
defm FCOS :   FTRAN<"fcos",   0b011001, 0b00000, f64, fcos>;
defm FCOSF :  FTRAN<"fcosf",  0b011001, 0b00010, f32, fcos>;
defm FTAN :   UTRAN<"ftan",   0b011010, 0b00000, f64>;
defm FTANF :  UTRAN<"ftanf",  0b011010, 0b00010, f32>;
defm RSQRT :  UTRAN<"frsqrt", 0b001100, 0b00000, f64>;
defm RSQRTF : UTRAN<"frsqrt", 0b001100, 0b00010, f32>;
defm FEXP10:  UTRAN<"fexp10", 0b000110, 0b00000, f64>;
defm FEXP10F: UTRAN<"fexp10f",0b000110, 0b00010, f32>;
defm FASIN :  UTRAN<"fasin",  0b011100, 0b00000, f64>;
defm FASINF : UTRAN<"fasinf", 0b011100, 0b00010, f32>;
defm FACOS :  UTRAN<"facos",  0b011101, 0b00000, f64>;
defm FACOSF : UTRAN<"facosf", 0b011101, 0b00010, f32>;
defm FATAN :  UTRAN<"fatan",  0b011110, 0b00000, f64>;
defm FATANF : UTRAN<"fatan",  0b011110, 0b00010, f32>;

// Patterns to use reciprocal square root
def : Pat<(fdiv fpimm1, (fsqrt (f64 GRegs:$bot))),
	(RSQRTu (f64 GRegs:$bot))>;
def : Pat<(fdiv fpimm64:$top, (fsqrt (f64 GRegs:$bot))),
	(FMULrd (RSQRTu (f64 GRegs:$bot)), fpimm64:$top)>;
def : Pat<(fdiv (f64 GRegs:$top), (fsqrt (f64 GRegs:$bot))),
	(FMULrr (f64 GRegs:$top), (RSQRTu (f64 GRegs:$bot)))>;
def : Pat<(fdiv fpimm1, (fsqrt (f32 GRegs:$bot))),
	(RSQRTFu (f32 GRegs:$bot))>;
def : Pat<(fdiv fpimm32:$top, (fsqrt (f32 GRegs:$bot))),
	(FMULFrs (RSQRTFu (f32 GRegs:$bot)), fpimm32:$top)>;
def : Pat<(fdiv (f32 GRegs:$top), (fsqrt (f32 GRegs:$bot))),
	(FMULFrr (f32 GRegs:$top), (RSQRTFu (f32 GRegs:$bot)))>;

// These bitcasts are NOPs
def       : Pat<(bitconvert (f64 GRegs:$rs)),
		(i64 GRegs:$rs)>;
def       : Pat<(bitconvert (i64 GRegs:$rs)),
		(f64 GRegs:$rs)>;

// Loads and Stores
let mayLoad = 1 in {
  defm LDG : Load<"ldd",  0b100011, 0b000011, f64, load>;
  defm LDF : Load<"lduw", 0b100010, 0b000010, f32, load>;
}

let mayStore = 1 in {
  defm STG : Store<"std", 0b101011, 0b001011, f64, store>;
  defm STF : Store<"stw", 0b101010, 0b001010, f32, store>;
}
