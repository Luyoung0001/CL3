package cl3

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._

// ========== Branch Prediction Bundle ========== //

class BpInfo extends Bundle {
  val valid      = Bool()
  val target     = UInt(32.W)
  val isTaken    = Bool()
  val isNotTaken = Bool()
  val source     = UInt(32.W)
  val isCall     = Bool()
  val isRet      = Bool()
  val isJmp      = Bool()
}

class BrInfo extends Bundle {
  val valid = Bool()
  val pc    = UInt(32.W)
  val priv  = UInt(2.W)
}

// ===================  NPC Bundle =================== //

class NPCIO extends Bundle {
  val pc     = Input(UInt(32.W))
  val accept = Input(Bool())
  val npc    = Output(UInt(32.W))
  val taken  = Output(UInt(2.W))
}

class NPCFullIO extends Bundle {
  val bp      = Input(new BpInfo)
  val mispred = Input(Bool())
  val info    = new NPCIO
}

class FERawInfo extends Bundle {
  val pc   = UInt(32.W)
  val inst = UInt(64.W)
  val pred = UInt(2.W)
}

class FEInfo extends Bundle {
  val pc    = UInt(32.W)
  val inst  = UInt(32.W)
  val pred  = Bool()
}

class MicroOp extends Bundle {
  val op0 = UInt(4.W)
  val op1 = UInt(3.W)
  val op2 = UInt(3.W)
}

import OpConstant._
class OpInfo extends Bundle {
  val valid = Bool()
  val inst  = UInt(32.W)
  val pc    = UInt(32.W)
  val wen   = Bool()
  val uop   = new MicroOp
  val rs1   = UInt(32.W)
  val rs2   = UInt(32.W)

  def rdIdx:  UInt = inst(11, 7)
  def rs1Idx: UInt = Mux(uop.op1 === OP1_Z || uop.op1 === OP1_PC, 0.U(5.W), inst(19, 15)) // TODO:
  def rs2Idx: UInt = Mux(uop.op2 === OP2_REG, inst(24, 20), 0.U(5.W))                     // TODO:
}

object OpInfo {
  def fromDE(data: DEInfo): OpInfo = {
    val op = Wire(new OpInfo)
    op.inst  := data.inst
    op.pc    := data.pc
    op.uop   := data.uop
    op.wen   := data.wen
    op.rs1   := 0.U     // will be overwrite
    op.rs2   := 0.U     // will be overwrite
    op.valid := false.B // will be overwrite

    op
  }

  def fromPipe(data: PipeInfo): OpInfo = {
    val op = Wire(new OpInfo)
    op.inst  := data.info.inst
    op.pc    := data.info.pc
    op.uop   := data.info.uop
    op.wen   := data.info.wen
    op.rs1   := data.rs1
    op.rs2   := data.rs2
    op.valid := false.B // will be overrite
    op
  }
}

class DEInfo extends Bundle {
  val inst    = UInt(32.W)
  val pc      = UInt(32.W)
  val wen     = Bool()
  val isLSU   = Bool()
  val isMUL   = Bool()
  val isDIV   = Bool()
  val isCSR   = Bool()
  val isEXU   = Bool()
  val isBr    = Bool()
  val uop     = new MicroOp
  val illegal = Bool()
  val pred    = Bool()

  def rdIdx:  UInt = inst(11, 7)
  def rs1Idx: UInt = Mux(uop.op1 === OP1_Z || uop.op1 === OP1_PC, 0.U(5.W), inst(19, 15)) // TODO:
  def rs2Idx: UInt = Mux(uop.op2 === OP2_REG, inst(24, 20), 0.U(5.W))

  def inst_delay: UInt = {
    Mux1H(
      Seq(
        isLSU -> 1.U(2.W),
        isMUL -> 1.U(2.W),
        isDIV -> 2.U(2.W),
        isCSR -> 2.U(2.W),
        isEXU -> 0.U(2.W)
      )
    )
  }
}

class MMUCtrlInfo extends Bundle {
  val priv  = UInt(2.W)
  val sum   = Bool()
  val mxr   = Bool()
  val flush = Bool()
  val satp  = UInt(32.W)
}

// ==================== Pipe Bundle =================== //

class PipeISInput extends Bundle {
  val fire      = Input(Bool())
  val info      = Input(new DEInfo)
  val rs1       = Input(UInt(32.W))
  val rs2       = Input(UInt(32.W))
  val rs1_id    = Input(UInt(3.W))
  val rs2_id    = Input(UInt(3.W))
  val except    = Input(UInt(6.W))
  val rdy_stage = Input(UInt(2.W))

  def rdIdx: UInt = info.inst(11, 7)
}

class PipeLSUInput extends Bundle {
  val valid     = Input(Bool())
  val rdata     = Input(UInt(32.W))
  val except    = Input(UInt(6.W))
  val stall     = Input(Bool())
  val cacheable = Input(Bool())
}

class PipeCSRInput extends Bundle {
  val wen    = Input(Bool())
  val rdata  = Input(UInt(32.W))
  val wdata  = Input(UInt(32.W))
  val except = Input(UInt(6.W))
}

class PipeDIVInput extends Bundle {
  val valid  = Input(Bool())
  val result = Input(UInt(32.W))
}

class PipeMULInput extends Bundle {
  val result = Input(UInt(32.W))
}

class PipeEXUInput extends Bundle {
  val result = Input(UInt(32.W))
  val br     = Input(new BrInfo)
}

class PipeInfo extends Bundle {
  val valid     = Bool()
  val info      = new DEInfo
  val npc       = UInt(32.W)
  val rs1       = UInt(32.W)
  val rs2       = UInt(32.W)
  val rs1_id    = UInt(3.W)
  val rs2_id    = UInt(3.W)
  val result    = UInt(32.W)
  val rdy_stage = UInt(2.W)
  val except    = UInt(6.W)
  val commit    = Bool()
  val stall     = Bool()

  val csr = new Bundle {
    val waddr = UInt(12.W)
    val wdata = UInt(32.W)
    val wen   = Bool()
  }

  val mem = new Bundle {
    val cacheable = Bool()
  }

  def isLd:  Bool = valid && info.isLSU && info.wen
  def isBr:  Bool = valid && info.isBr && !info.wen
  def isSt:  Bool = valid && info.isLSU && !info.wen
  def isMul: Bool = valid && info.isMUL
  def isJmp: Bool = valid && info.isBr && info.wen
  def isALU: Bool = valid && info.isEXU && !info.isBr
  def isMem: Bool = isLd || isSt

  def rdIdx: UInt = info.inst(11, 7)

  // def commit: Bool = valid
}

class ISCSRInput extends Bundle {
  val br     = Input(new BrInfo)
  val wen    = Input(Bool())
  val rdata  = Input(UInt(32.W))
  val wdata  = Input(UInt(32.W))
  val except = Input(UInt(6.W))
}

class ISEXUInput extends Bundle {
  val br     = Input(new BrInfo)
  val bp     = Input(new BpInfo)
  val result = Input(UInt(32.W))
}

class ISCSROutput extends Bundle {
  val wen    = Output(Bool())
  val waddr  = Output(UInt(12.W))
  val wdata  = Output(UInt(32.W))
  val except = Output(UInt(6.W))
}

class BypassISInfo extends Bundle {
  val rs1Idx = UInt(5.W)
  val rs2Idx = UInt(5.W)
  val rs1    = UInt(32.W)
  val rs2    = UInt(32.W)
}
