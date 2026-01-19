package cl3

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import CL3InstInfo._
import dataclass.data

class CSRInput extends Bundle {
  val info        = Input(new OpInfo)
  val wb          = Flipped(new ISCSROutput)
  val irq_e       = Input(Bool())
  val irq_t       = Input(Bool())
  val baddr       = Input(UInt(32.W))
  val hold        = Input(Bool())

  val irq_inhibit = Input(Bool())
}

class CSROutput extends Bundle {
  val info   = Flipped(new ISCSRInput)
  val irq    = Output(Bool())

  /*  Used for difftest to verify the Next PC (NPC). When an exception occurs,
    the NPC matches the tvec register. This signal is required for that
    comparison and will be optimized away when difftest is disabled. */
  val tvec = Output(UInt(32.W))
  val epc  = Output(UInt(32.W))

}

class CSRIO extends Bundle {
  val in  = new CSRInput
  val out = new CSROutput
}

class CL3CSR extends Module with CSRConstant {
  val io = IO(new CSRIO)

  val table = new DecodeTable(instPatterns, Seq(CSROPField))
  val res   = table.decode(io.in.info.inst)
  val op    = res(CSROPField)

  //TODO: change decode logic
  val is_ecall  = io.in.info.valid && op(4, 3) === "b00".U(2.W) && op(0)
  val is_ebreak = io.in.info.valid && op(4, 3) === "b00".U(2.W) && op(1)
  val is_eret   = io.in.info.valid && op(4, 3) === "b00".U(2.W) && op(2)

  val is_csrrw  = io.in.info.valid && op(4, 3) === "b01".U(2.W) && op(0)
  val is_csrrs  = io.in.info.valid && op(4, 3) === "b01".U(2.W) && op(1)
  val is_csrrc  = io.in.info.valid && op(4, 3) === "b01".U(2.W) && op(2)

  val is_csrrwi = io.in.info.valid && op(4, 3) === "b11".U(2.W) && op(0)
  val is_csrrsi = io.in.info.valid && op(4, 3) === "b11".U(2.W) && op(1)
  val is_csrrci = io.in.info.valid && op(4, 3) === "b11".U(2.W) && op(2)

  val eret_priv = io.in.info.inst(29, 28)

  val set   = isSet(op)
  val clear = isClear(op)

  val raddr = io.in.info.inst(31, 20)

  val current_priv = Wire(UInt(2.W))
  val csr_priv     = io.in.info.inst(29, 28)
  val csr_readonly = (io.in.info.inst(31, 30) === 3.U) // Machine-mode read-only CSRs
  val csr_write    = (io.in.info.rs1Idx =/= 0.U) | is_csrrw | is_csrrwi
  val csr_wdata    = Mux((is_csrrwi | is_csrrsi | is_csrrci), Cat(0.U(27.W), io.in.info.rs1Idx), io.in.info.rs1)

  val csr_fault =
    (io.in.info.valid & (set | clear)) && ((csr_write && csr_readonly) || (current_priv < csr_priv))

  val satp_update = (io.in.info.valid && (set || clear) && csr_write && (io.in.info.inst(31, 20) === CSR_SATP))

//-----------------------------------------------------------------
// CSR register file
//-----------------------------------------------------------------
  val misa = "h40001100".U(32.W) // RV32IM

  val csr_rdata  = Wire(UInt(32.W))
  val csr_branch = Wire(Bool())
  val csr_target = Wire(UInt(32.W))
  val interrupt  = Wire(UInt(32.W))
  val status_reg = Wire(UInt(32.W))

  val csr_rf = Module(new CL3CSRRF())
  csr_rf.io.in.irq_e := io.in.irq_e
  csr_rf.io.in.irq_t := io.in.irq_t

  csr_rf.io.in.raddr    := raddr
  csr_rf.io.in.ren      := io.in.info.valid // TODO:
  csr_rf.io.in.baddr    := io.in.baddr

  csr_rf.io.in.waddr := io.in.wb.waddr
  csr_rf.io.in.wdata := io.in.wb.wdata
  csr_rf.io.in.wen   := io.in.wb.wen

  csr_rf.io.in.trap  := io.in.wb.except.code
  csr_rf.io.in.tval  := io.in.wb.except.tval
  csr_rf.io.in.pc    := io.in.wb.except.pc
  csr_rf.io.in.inst  := io.in.info.inst

  csr_rdata  := csr_rf.io.out.rdata
  csr_branch := csr_rf.io.out.br.valid
  csr_target := csr_rf.io.out.br.pc
  interrupt  := csr_rf.io.out.irq
  status_reg := csr_rf.io.status

  current_priv := csr_rf.io.out.br.priv

  val rd_valid_e1_q  = RegInit(false.B)
  val rd_result_e1_q = RegInit(0.U(32.W))
  val csr_wdata_e1_q = RegInit(0.U(32.W))
  val exception_e1_q = RegInit(0.U((EXCEPTION_W).W))
  val pc_e1_q        = RegInit(0.U(32.W))

  val eret_fault = is_eret & (current_priv < eret_priv)

  when(io.in.info.valid && !io.in.hold) {
    rd_valid_e1_q := (set || clear) && ~csr_fault

    pc_e1_q := io.in.info.pc

    // Invalid instruction / CSR access fault?
    // Record opcode for writing to csr_xtval later.
    rd_result_e1_q := Mux(csr_fault || eret_fault, io.in.info.inst, csr_rdata)

    csr_wdata_e1_q := MuxCase(csr_wdata_e1_q, Seq(
      (set && clear) -> csr_wdata,
      set -> (csr_wdata | csr_rdata),
      clear -> (~csr_wdata & csr_rdata) 
    ))
  }.elsewhen(!io.in.info.valid && !io.in.hold) {
    rd_valid_e1_q  := false.B
    csr_wdata_e1_q := 0.U
    rd_result_e1_q := 0.U
    pc_e1_q        := 0.U

  }

  exception_e1_q := MuxCase(exception_e1_q, Seq(
    (is_ecall && !io.in.hold) -> (EXCEPTION_ECALL + current_priv),
    (eret_fault && !io.in.hold) -> EXCEPTION_ILLEGAL_INSTRUCTION,
    (is_eret && !io.in.hold) -> (EXCEPTION_ERET_U + eret_priv),
    (is_ebreak && !io.in.hold) -> EXCEPTION_BREAKPOINT,
    (!io.in.info.valid && !io.in.hold) -> 0.U
  ))



  io.out.info.rdata       := rd_result_e1_q
  io.out.info.except.code := exception_e1_q
  io.out.info.except.pc   := pc_e1_q

  // ecall and ebreak will set xtval to zero
  io.out.info.except.tval := Mux( exception_e1_q === EXCEPTION_ILLEGAL_INSTRUCTION, pc_e1_q, 0.U)
  io.out.info.wdata       := csr_wdata_e1_q
  io.out.info.wen         := rd_valid_e1_q

// Interrupt launch enable
  io.out.irq    := RegNext(interrupt.orR & !io.in.irq_inhibit)

  io.out.tvec := csr_rf.io.tvec
  io.out.epc  := csr_rf.io.epc

//-----------------------------------------------------------------
// Execute - Branch operations
//-----------------------------------------------------------------
  val branch_q        = RegInit(false.B)
  val branch_target_q = RegInit(0.U(32.W))
  val reset_q         = RegInit(true.B)

  when(reset_q) {
    branch_target_q := io.in.baddr
    branch_q        := true.B
    reset_q         := false.B
  }.otherwise {
    branch_q        := csr_branch
    branch_target_q := csr_target
  }

  io.out.info.br.valid := branch_q
  io.out.info.br.pc    := branch_target_q
  io.out.info.br.priv  := csr_rf.io.out.br.priv

}

object CSROPField extends DecodeField[InstructionPattern, UInt] {
  def name: String = "CSR OP"

  import CSRConstant._
  def chiselType: UInt = UInt(CSR_OP_WIDTH.W)

  def genTable(op: InstructionPattern): BitPat = {

    op.name match {
      case "csrrw"  => BitPat(OP_CSRRW)
      case "csrrs"  => BitPat(OP_CSRRS)
      case "csrrc"  => BitPat(OP_CSRRC)
      case "csrrwi" => BitPat(OP_CSRRWI)
      case "csrrsi" => BitPat(OP_CSRRSI)
      case "csrrci" => BitPat(OP_CSRRCI)
      case "ecall"  => BitPat(OP_ECALL)
      case "ebreak" => BitPat(OP_EBREAK)
      case "mret"   => BitPat(OP_ERET)
      case _        => BitPat.dontCare(CSR_OP_WIDTH)
    }
  }
}
