//===- MCoreInstrFormats.td - MCore Instruction Formats --*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===--------------------------------------------------------------------===//


class MCInst<dag outs, dag ins,
	     string asmstr,
	     list<dag> pattern,
	     string constraints >
   : Instruction {
  field bits<16> Inst = 0;	// no JIT
  let Namespace = "MCore";
  dag OutOperandList = outs;
  dag InOperandList = ins;
  let AsmString   = asmstr;
  let Pattern = pattern;
  let Constraints = constraints;
  let Size = 2;
}
class PseudoInst<dag outs, dag ins, string asmstr, list<dag> pattern> :
  MCInst<outs, ins, asmstr, pattern, "" >;

//
// Operand definitions
//
// Transformation function: -imm
def imm_neg  : SDNodeXForm<imm, [{
		uint32_t value = N->getZExtValue();
		return getI32Imm(-value, SDLoc(N)); }]>;
def iszero   : Operand<i32>, ImmLeaf<i32, [{ return Imm == 0; }]>;
def isone    : Operand<i32>, ImmLeaf<i32, [{ return Imm == 1; }]>;
def imm0_127 : Operand<i32>, ImmLeaf<i32,
		[{ return Imm >= 0 && Imm <= 127; }]>;
def imm0_31  : Operand<i32>, ImmLeaf<i32,
		[{ return Imm >= 0 && Imm <= 31; }]>;
def imm1_31  : Operand<i32>, ImmLeaf<i32,
		[{ return Imm > 0 && Imm <= 31; }]>;
def imm1_32  : Operand<i32>, ImmLeaf<i32,
		[{ return Imm > 0 && Imm <= 32; }]>;
def imm1_32n : PatLeaf<(imm), [{
		uint32_t value = -N->getZExtValue();
		return value > 0 && value <= 32; }]>;
def gbladr :   Operand<iPTR>;
def brtarget : Operand<OtherVT>;
def calltarget : Operand<OtherVT>;

// Power of 2 support
def immpow2  : PatLeaf<(imm), [{
		uint32_t value = N->getZExtValue();
		return isPowerOf2_32(value); }]>;
def immpow2n : PatLeaf<(imm), [{
		uint32_t value = N->getZExtValue();
		return isPowerOf2_32(~value); }]>;
def pow2_xfm : SDNodeXForm<imm, [{
		uint32_t value = N->getZExtValue();
		return getI32Imm(countTrailingZeros(value), SDLoc(N)); }]>;
def pow2n_xfm: SDNodeXForm<imm, [{
		uint32_t value = N->getZExtValue();
		return getI32Imm(countTrailingZeros(~value), SDLoc(N)); }]>;

//
// MCore specific DAG Nodes.
//
// These are target-independent nodes, but have target-specific formats.
def callseq_start : SDNode<"ISD::CALLSEQ_START",
			SDCallSeqStart<[ SDTCisVT<0, i32>,
                                         SDTCisVT<1, i32> ]>,
			[SDNPHasChain, SDNPOutGlue]>;
def callseq_end   : SDNode<"ISD::CALLSEQ_END",
			SDCallSeqEnd<[ SDTCisVT<0, i32>,
                                       SDTCisVT<1, i32> ]>,
			[SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;
def MCret :	    SDNode<"MCoreISD::MCRET",
			SDTNone,
			[SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
def MCcall :	    SDNode<"MCoreISD::CALL",
                        SDTypeProfile<0, -1, [SDTCisPtrTy<0>]>,
			[SDNPHasChain, SDNPOptInGlue, SDNPOutGlue, SDNPVariadic]>;
def MCcalli:	    SDNode<"MCoreISD::CALLI",
			SDTypeProfile<0, 1, [SDTCisPtrTy<0>]>,
			[SDNPHasChain, SDNPOptInGlue, SDNPOutGlue, SDNPVariadic]>;
def MCcmp : 	    SDNode<"MCoreISD::CMP",
			SDTypeProfile<0, 3, [SDTCisSameAs<0, 1>, SDTCisVT<2, i32>]>,
			[]>;
def MCset0:	    SDNode<"MCoreISD::SET0",
			SDTNone,
			[]>;
def MCset1:	    SDNode<"MCoreISD::SET1",
			SDTNone,
			[]>;
def MCscc :	    SDNode<"MCoreISD::SETCC",
			SDTypeProfile<1, 2, [SDTCisVT<0, i32>, SDTCisVT<1, i32>,
			    SDTCisVT<2, i32>]>,
			[]>;
def MCsel:	    SDNode<"MCoreISD::SELECT",
			SDTypeProfile<1, 4, [SDTCisSameAs<0, 1>,
			    SDTCisSameAs<0, 2>, SDTCisVT<3, i32>,
			    SDTCisVT<4, i32>]>,
			[]>;
def MCbcc :	    SDNode<"MCoreISD::BRCOND",
			SDTypeProfile<0, 3, [SDTCisVT<0, OtherVT>,
			    SDTCisVT<1, i32>, SDTCisVT<2, i32>]>,
			[SDNPHasChain]>;
def MCGA:	    SDNode<"MCoreISD::GA",
			SDTypeProfile<1, 1, [SDTCisSameAs<0, 1>,
			    SDTCisPtrTy<0>]>,
			[]>;

// Real Instructions
// register move
let isMoveReg = 1 in
def MOV : MCInst< (outs GRegs:$dst), (ins GRegs:$src),
	"mov\t$dst,$src",
	[], "">;

// BGENi, BMASKi, BCLRi, and BSETi will probably not select automatically
class MCIRC<string asmstr, list<dag> pattern> :
  MCInst< (outs GRegs:$dst), (ins i32imm:$src),
	  !strconcat(asmstr, "\t$dst,$src"),
	  pattern, "" >;
let isReMaterializable = 1, isAsCheapAsAMove = 1 in {
def MOVi :   MCIRC< "movi",   [(set GRegs:$dst, imm0_127:$src)]>;
def BGENi :  MCIRC< "bgeni",  [(set GRegs:$dst, (shl 1, imm0_31:$src))]>;
def BMASKi : MCIRC< "bmaski", [(set GRegs:$dst, (sub (shl 1, imm0_31:$src),1))]>;
}

let isReMaterializable = 1 in {
def LRW : MCInst< (outs GRegs:$dst), (ins i32imm:$src),
	"lrw\t$dst,$src",
	[(set GRegs:$dst, imm:$src)], "">;
def LDA : MCInst< (outs GRegs:$dst), (ins gbladr:$src),
	"lrw\t$dst,$src",
	[(set GRegs:$dst, (MCGA tglobaladdr:$src))], "">;
def LDJT: MCInst< (outs GRegs:$dst), (ins gbladr:$src),
	"lrw\t$dst,$src",
	[(set GRegs:$dst, (MCGA tjumptable:$src))], "">;
}

//
// loads, stores
//
def addrb : Operand<i32>,
  ComplexPattern<i32, 2, "SelectAddrB", [] > {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops GRegs:$base, i32imm:$offsimm);
}

def addrh : Operand<i32>,
  ComplexPattern<i32, 2, "SelectAddrH", [] > {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops GRegs:$base, i32imm:$offsimm);
}

def addrw : Operand<i32>,
  ComplexPattern<i32, 2, "SelectAddrW", [] > {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops GRegs:$base, i32imm:$offsimm);
}

class MCILD<string asmstr, PatFrag OpNode> :
  MCInst< (outs GRegs:$dst), (ins addrb:$src),
	  !strconcat(asmstr, "\t$dst,$src"),
	  [(set GRegs:$dst, (OpNode  addrb:$src))], "">;
class MCIST<string asmstr, PatFrag OpNode> :
  MCInst< (outs), (ins GRegs:$src, addrb:$dst),
	  !strconcat(asmstr, "\t$src,$dst"),
	  [(OpNode  GRegs:$src, addrb:$dst)], "">;

def LDW  : MCInst< (outs GRegs:$dst), (ins addrw:$src),
	  "ldw\t$dst,$src",
	  [(set GRegs:$dst, (load  addrw:$src))], "">;
def LDHz : MCInst< (outs GRegs:$dst), (ins addrh:$src),
	  "ldh\t$dst,$src",
	  [(set GRegs:$dst, (zextloadi16  addrh:$src))], "">;
def LDHx : MCInst< (outs GRegs:$dst), (ins addrh:$src),
	  "ldh\t$dst,$src",
	  [(set GRegs:$dst, (extloadi16  addrh:$src))], "">;
def LDBz : MCInst< (outs GRegs:$dst), (ins addrb:$src),
	  "ldb\t$dst,$src",
	  [(set GRegs:$dst, (zextloadi8  addrb:$src))], "">;
def LDBx : MCInst< (outs GRegs:$dst), (ins addrb:$src),
	  "ldb\t$dst,$src",
	  [(set GRegs:$dst, (extloadi8  addrb:$src))], "">;

def STW  : MCInst< (outs), (ins GRegs:$src, addrw:$dst),
		"stw\t$src,$dst",
		[(store GRegs:$src, addrw:$dst)], "" >;
def STH  : MCInst< (outs), (ins GRegs:$src, addrh:$dst),
		"sth\t$src,$dst",
		[(truncstorei16 GRegs:$src, addrh:$dst)], "" >;
def STB  : MCInst< (outs), (ins GRegs:$src, addrb:$dst),
		"stb\t$src,$dst",
		[(truncstorei8 GRegs:$src, addrb:$dst)], "" >;

// single register
class MCIRS<string asmstr, list<dag> pattern> :
  MCInst< (outs GRegs:$dst), (ins GRegs:$lhs),
	  !strconcat(asmstr, "\t$dst"),
	  pattern, "$lhs = $dst" >;
def ABS   : MCIRS< "abs",   [(set GRegs:$dst, (abs GRegs:$lhs))]>;
def NOT   : MCIRS< "not",   [(set GRegs:$dst, (not GRegs:$lhs))]>;
def BREV  : MCIRS< "brev",  [(set GRegs:$dst, (bitreverse GRegs:$lhs))]>;
def FF1   : MCIRS< "ff1",   [(set GRegs:$dst, (ctlz GRegs:$lhs))]>;
def ZEXTB : MCIRS< "zextb", [(set GRegs:$dst, (and GRegs:$lhs, 0x000000FF))]>;
def ZEXTH : MCIRS< "zexth", [(set GRegs:$dst, (and GRegs:$lhs, 0x0000FFFF))]>;
def SEXTB : MCIRS< "sextb", [(set GRegs:$dst, (sra (shl GRegs:$lhs, 24), 24))]>;
def SEXTH : MCIRS< "sexth", [(set GRegs:$dst, (sra (shl GRegs:$lhs, 16), 16))]>;

// register to register
class MCIRR<string asmstr, list<dag> pattern> :
  MCInst< (outs GRegs:$dst), (ins GRegs:$lhs, GRegs:$rhs),
	  !strconcat(asmstr, "\t$dst,$rhs"),
	  pattern, "$lhs = $dst">;
def ADD  : MCIRR< "add",  [(set GRegs:$dst, (add GRegs:$lhs, GRegs:$rhs))]>;
def MUL  : MCIRR< "mult", [(set GRegs:$dst, (mul GRegs:$lhs, GRegs:$rhs))]>;
def OR   : MCIRR< "or",   [(set GRegs:$dst, (or GRegs:$lhs, GRegs:$rhs))]>;
def AND  : MCIRR< "and",  [(set GRegs:$dst, (and GRegs:$lhs, GRegs:$rhs))]>;
def XOR  : MCIRR< "xor",  [(set GRegs:$dst, (xor GRegs:$lhs, GRegs:$rhs))]>;
def SUB  : MCIRR< "sub",  [(set GRegs:$dst, (sub GRegs:$lhs, GRegs:$rhs))]>;
def ANDn : MCIRR< "andn", [(set GRegs:$dst, (and GRegs:$lhs, (not GRegs:$rhs)))]>;
def LSL  : MCIRR< "lsl",  [(set GRegs:$dst, (shl GRegs:$lhs, GRegs:$rhs))]>;
def LSR  : MCIRR< "lsr",  [(set GRegs:$dst, (srl GRegs:$lhs, GRegs:$rhs))]>;
def ASR  : MCIRR< "asr",  [(set GRegs:$dst, (sra GRegs:$lhs, GRegs:$rhs))]>;
def IXH  : MCIRR< "ixh",  [(set GRegs:$dst, (add GRegs:$lhs, (shl GRegs:$rhs, 1)))]>;
def IXW  : MCIRR< "ixw",  [(set GRegs:$dst, (add GRegs:$lhs, (shl GRegs:$rhs, 2)))]>;

let DisableEncoding = "$cout,$cin" in {
  let isCommutable = 1 in
def ADDC : MCInst< (outs GRegs:$dst, CBit:$cout),
	(ins GRegs:$lhs, GRegs:$rhs, CBit:$cin),
	"addc\t$dst,$rhs",
	[], "$lhs = $dst">;
def SUBC : MCInst< (outs GRegs:$dst, CBit:$cout),
	(ins GRegs:$lhs, GRegs:$rhs, CBit:$cin),
	"subc\t$dst,$rhs",
	[], "$lhs = $dst">;
}

// FIXME - divide instructions divide by R1
class MCIRR1<string asmstr, list<dag> pattern> :
  MCInst< (outs GRegs:$dst), (ins GRegs:$lhs, GR1:$rhs),
	  !strconcat(asmstr, "\t$dst,$rhs"),
	  pattern, "$lhs = $dst">;
def DIVU : MCIRR1< "divu", [(set GRegs:$dst, (udiv GRegs:$lhs, GR1:$rhs))]>;
def DIVS : MCIRR1< "divs", [(set GRegs:$dst, (sdiv GRegs:$lhs, GR1:$rhs))]>;

let Defs = [SP], Uses =[SP] in {
def LDM : MCInst< (outs GRegs:$dst), (ins),
	"ldm\t$dst,(sp)",
	[], "">;
def STM : MCInst< (outs), (ins GRegs:$src),
	"stm\t$src,(sp)",
	[], "">;
}


// immediate to register
class MCIRI<string asmstr, list<dag> pattern> :
  MCInst< (outs GRegs:$dst), (ins GRegs:$lhs, i32imm:$rhs),
	  !strconcat(asmstr, "\t$dst,$rhs"),
	  pattern, "$lhs = $dst">;
def ADDi  : MCIRI< "addi",  [(set GRegs:$dst, (add GRegs:$lhs, imm1_32:$rhs))]>;
def ANDi  : MCIRI< "andi",  [(set GRegs:$dst, (and GRegs:$lhs, imm0_31:$rhs))]>;
def SUBi  : MCIRI< "subi",  [(set GRegs:$dst, (sub GRegs:$lhs, imm1_32:$rhs))]>;
def RSUBi : MCIRI< "rsubi", [(set GRegs:$dst, (sub imm0_31:$rhs, GRegs:$lhs))]>;
def LSLi  : MCIRI< "lsli",  [(set GRegs:$dst, (shl GRegs:$lhs, imm1_31:$rhs))]>;
def LSRi  : MCIRI< "lsri",  [(set GRegs:$dst, (srl GRegs:$lhs, imm1_31:$rhs))]>;
def ASRi  : MCIRI< "asri",  [(set GRegs:$dst, (sra GRegs:$lhs, imm1_31:$rhs))]>;
def ROTLi : MCIRI< "roli",  [(set GRegs:$dst, (rotl GRegs:$lhs, imm1_31:$rhs))]>;
def BSETi : MCIRI< "bseti", []>;
def BCLRi : MCIRI< "bclri", []>;

// (sub X, imm) gets canonicalized to (add X, -imm).  Match this form.
def : Pat<(add GRegs:$dst, imm1_32n:$src),
          (SUBi GRegs:$dst, (imm_neg imm1_32n:$src))>;

def : Pat<(or GRegs:$lhs, immpow2:$rhs),
	  (BSETi GRegs:$lhs, (pow2_xfm imm:$rhs))>;
def : Pat<(and GRegs:$lhs, immpow2n:$rhs),
	  (BCLRi GRegs:$lhs, (pow2n_xfm imm:$rhs))>;
//
// compares
//
def MC_COND_NE : PatLeaf<(i32 0)>;
def MC_COND_LT : PatLeaf<(i32 1)>;
def MC_COND_HS : PatLeaf<(i32 2)>;

class MCICP<string asmstr, PatLeaf Kind> :
  MCInst< (outs CBit:$cbit), (ins GRegs:$lhs, GRegs:$rhs),
	  !strconcat(asmstr, "\t$lhs,$rhs"),
	  [(set CBit:$cbit, (MCcmp GRegs:$lhs, GRegs:$rhs, Kind))], "">;
class MCICI<string asmstr, PatLeaf Kind, Operand ImmType> :
  MCInst< (outs CBit:$cbit), (ins GRegs:$lhs, ImmType:$rhs),
	  !strconcat(asmstr, "\t$lhs,$rhs"),
	  [(set CBit:$cbit, (MCcmp GRegs:$lhs, ImmType:$rhs, Kind))], "">;

let isCompare = 1 in {
def CMPNE : MCICP< "cmpne", MC_COND_NE>;
def CMPLT : MCICP< "cmplt", MC_COND_LT>;
def CMPHS : MCICP< "cmphs", MC_COND_HS>;
def CMPNEi : MCICI< "cmpnei", MC_COND_NE, imm0_31>;
def CMPLTi : MCICI< "cmplti", MC_COND_LT, imm1_32>;
// there is no CMPHSi
}

def CLRC : MCInst< (outs CBit:$cbit), (ins),
	"clrc",
	[(set CBit:$cbit, (MCset0))], "">;
def SETC : MCInst< (outs CBit:$cbit), (ins),
	"setc",
	[(set CBit:$cbit, (MCset1))], "">;
def BTSTi: MCInst<(outs CBit:$cbit), (ins GRegs:$lhs, imm0_31:$rhs),
	"btsti\t$lhs,$rhs",
	[], "">;


// Move condition flag to register

class MCISC<string asmstr, PatLeaf Kind> :
  MCInst< (outs GRegs:$dst), (ins CBit:$cbit),
	  !strconcat(asmstr, "\t$dst"),
	  [(set GRegs:$dst, (MCscc Kind, CBit:$cbit))], "">;

def MVC :  MCISC< "mvc", PatLeaf<(i32 0)>>;
def MVCN : MCISC< "mvcv", PatLeaf<(i32 1)>>;

// Conditional moves

class MCIMC<string asmstr, PatLeaf Kind> :
  MCInst< (outs GRegs:$dst), (ins GRegs:$lhs, GRegs:$rhs, CBit:$cbit),
	  !strconcat(asmstr, "\t$dst,$rhs"),
	  [(set GRegs:$dst, (MCsel GRegs:$lhs, GRegs:$rhs, Kind, CBit:$cbit))], "$lhs = $dst">;

def MOVT: MCIMC< "movt", PatLeaf<(i32 0)> >;
def MOVF: MCIMC< "movf", PatLeaf<(i32 1)> >;

//
// Flow control
//
let  isTerminator = 1, isReturn = 1, isBarrier = 1, Uses=[R15] in {
def RET : MCInst< (outs), (ins),
	"jmp\tr15",
	[(MCret)], "">;
} // isReturn

let isBranch = 1, isTerminator = 1, isBarrier = 1 in {
def JMP : MCInst< (outs), (ins brtarget:$tgt),
	"jbr\t$tgt",
	[(br bb:$tgt)], "">;
}
let isBranch = 1, isIndirectBranch = 1, isTerminator = 1, isBarrier = 1 in {
def BRI : MCInst< (outs), (ins GRegs:$rs),
       "jmp\t$rs",
       [(brind GRegs:$rs)], "">;
}

class MCIBC<string asmstr, PatLeaf Kind> :
  MCInst< (outs), (ins brtarget:$tgt, CBit:$cbit),
	  !strconcat(asmstr, "\t$tgt"),
	  [(MCbcc bb:$tgt, Kind, CBit:$cbit)], "">;

let isBranch = 1, isTerminator = 1 in {
def JT : MCIBC< "bt", PatLeaf<(i32 0)>>;
def JF : MCIBC< "bf", PatLeaf<(i32 1)>>;
}

let isCall = 1, Defs = [R15] in {
def CALL : MCInst< (outs), (ins calltarget:$tgt),
	"jbsr\t$tgt",
	[(MCcall tglobaladdr:$tgt)], "">;
def CALLI: MCInst< (outs), (ins GRegs:$reg),
	"bsr\t$reg",
	[(MCcalli GRegs:$reg)], "">;
} // isCall
def : Pat<(MCcall texternalsym:$adr), (CALL texternalsym:$adr)>;

//
// Pseudo Instructions
//
let Defs = [SP],  Uses =[SP] in {
def ADJCALLSTACKDOWN : PseudoInst< (outs), (ins i32imm:$amt1, i32imm:$amt2),
		    "; ADJCALLSTACKDOWN $amt1",
		    [(callseq_start timm:$amt1, timm:$amt2)]>;
def ADJCALLSTACKUP :   PseudoInst< (outs), (ins i32imm:$amt1, i32imm:$amt2),
		    "; ADJCALLSTACKUP $amt1",
		    [(callseq_end timm:$amt1, timm:$amt2)]>;
}
