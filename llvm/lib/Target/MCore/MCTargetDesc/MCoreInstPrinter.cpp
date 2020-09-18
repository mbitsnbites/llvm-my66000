//===-- MCoreInstPrinter.cpp - Convert MCore MCInst to assembly syntax ----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This class prints an MCore MCInst to a .s file.
//
//===----------------------------------------------------------------------===//

#include "MCoreInstPrinter.h"
#include "MCore.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>

using namespace llvm;

#define DEBUG_TYPE "asm-printer"

#include "MCoreGenAsmWriter.inc"



void MCoreInstPrinter::printRegName(raw_ostream &OS, unsigned RegNo) const {
  OS << StringRef(getRegisterName(RegNo)).lower();
}

void MCoreInstPrinter::printInst(const MCInst *MI, raw_ostream &O,
                                 StringRef Annot, const MCSubtargetInfo &STI) {
/*
  switch (MI->getOpcode()) {
  case MCore::BRC: {
    const MCOperand &Opcc = MI->getOperand(2);
    O << "\tb" << CondCodeString(Opcc.getImm()) << "\t";
    printOperand(MI, 1, O);
    O << ",";
    printOperand(MI, 0, O);
    }
    break;
  case MCore::BRIB: {
    const MCOperand &Opcc = MI->getOperand(2);
    O << "\tb" << CondBitString(Opcc.getImm()) << "\t";
    printOperand(MI, 1, O);
    O << ",";
    printOperand(MI, 0, O);
    }
    break;
  case MCore::BRFB: {
    const MCOperand &Opcc = MI->getOperand(2);
    O << "\tb" << FCondBitString(Opcc.getImm()) << "\t";
    printOperand(MI, 1, O);
    O << ",";
    printOperand(MI, 0, O);
    }
    break;
  default:
    printInstruction(MI, O);
  }
*/
  printInstruction(MI, O);	// FIXME

  printAnnotation(O, Annot);
}


static void printExpr(const MCExpr *Expr, const MCAsmInfo *MAI,
                      raw_ostream &OS) {
  int Offset = 0;
  const MCSymbolRefExpr *SRE;

  if (const MCBinaryExpr *BE = dyn_cast<MCBinaryExpr>(Expr)) {
    SRE = dyn_cast<MCSymbolRefExpr>(BE->getLHS());
    const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(BE->getRHS());
    assert(SRE && CE && "Binary expression must be sym+const.");
    Offset = CE->getValue();
  } else {
    SRE = dyn_cast<MCSymbolRefExpr>(Expr);
    assert(SRE && "Unexpected MCExpr type.");
  }
  assert(SRE->getKind() == MCSymbolRefExpr::VK_None);

  SRE->getSymbol().print(OS, MAI);

  if (Offset) {
    if (Offset > 0)
      OS << '+';
    OS << Offset;
  }
}

void MCoreInstPrinter::
printOperand(const MCInst *MI, unsigned OpNo, raw_ostream &O) {
  const MCOperand &Op = MI->getOperand(OpNo);
  if (Op.isReg()) {
    printRegName(O, Op.getReg());
    return;
  }
  if (Op.isImm()) {
    O << Op.getImm();
    return;
  }
  assert(Op.isExpr() && "unknown operand kind in printOperand");
  printExpr(Op.getExpr(), &MAI, O);
}

void MCoreInstPrinter::printMemOperand(const MCInst *MI, int opNum,
                                         raw_ostream &O) {
  O << "(";
  printOperand(MI, opNum, O);
  const MCOperand &MO = MI->getOperand(opNum+1);
  if (MO.isImm() && MO.getImm() != 0) {
    O << ",";
    printOperand(MI, opNum+1, O);
  }
  O << ")";
}
