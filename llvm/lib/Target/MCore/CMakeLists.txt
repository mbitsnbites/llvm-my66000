set(LLVM_TARGET_DEFINITIONS MCore.td)

tablegen(LLVM MCoreGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM MCoreGenCallingConv.inc -gen-callingconv)
tablegen(LLVM MCoreGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM MCoreGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM MCoreGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM MCoreGenSubtargetInfo.inc -gen-subtarget)

add_public_tablegen_target(MCoreCommonTableGen)

add_llvm_target(MCoreCodeGen
  MCoreAsmPrinter.cpp
  MCoreFrameLowering.cpp
  MCoreInstrInfo.cpp
  MCoreISelDAGToDAG.cpp
  MCoreISelLowering.cpp
  MCoreMachineFunctionInfo.cpp
  MCoreMCInstLower.cpp
  MCoreRegisterInfo.cpp
  MCoreSubtarget.cpp
  MCoreTargetMachine.cpp
  MCoreTargetObjectFile.cpp
  MCoreSelectionDAGInfo.cpp
  )

add_subdirectory(MCTargetDesc)
add_subdirectory(TargetInfo)
