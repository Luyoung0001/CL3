package cl3

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import CL3InstInfo._

class CSRInput extends Bundle {
  val info        = Input(new OpInfo)
  val wb          = Flipped(new ISCSROutput)
  val ext_irq     = Input(Bool())
  val timer_irq   = Input(Bool())
  val irq_inhibit = Input(Bool())
  
  val bootAddr = Input(UInt(32.W))
}

class CSROutput extends Bundle {
  val info   = Flipped(new ISCSRInput)
  val irq    = Output(Bool())
  val ifence = Output(Bool()) // TODO:
  val mmu    = Output(new MMUCtrlInfo)
}

class CSRIO extends Bundle {
  val in  = new CSRInput
  val out = new CSROutput
}

class CL3CSR extends Module with CSRConstant {
  val SUPPORT_MULDIV   = 1
  val SUPPORT_SUPER    = 0

  val io = IO(new CSRIO)

  val table = new DecodeTable(instPatterns, Seq(CSROPField))
  val res   = table.decode(io.in.info.inst)
  val op    = res(CSROPField)

  val ecall_w  = (op === OP_ECALL)  & io.in.info.valid 
  val ebreak_w = (op === OP_EBREAK) & io.in.info.valid  
  val eret_w   = (op === OP_ERET)   & io.in.info.valid
  
  val csrrw_w  = (op === OP_CSRRW)  & io.in.info.valid 
  val csrrs_w  = (op === OP_CSRRS)  & io.in.info.valid 
  val csrrc_w  = (op === OP_CSRRC)  & io.in.info.valid 
  val csrrwi_w = (op === OP_CSRRWI) & io.in.info.valid  
  val csrrsi_w = (op === OP_CSRRSI) & io.in.info.valid  
  val csrrci_w = (op === OP_CSRRCI) & io.in.info.valid  
  val fence_w  = (op === OP_FENCE)  & io.in.info.valid
  val sfence_w = (op === OP_SFENCE) & io.in.info.valid 
  val ifence_w = (op === OP_IFENCE) & io.in.info.valid 
  val wfi_w    = (op === OP_WFI)    & io.in.info.valid
  val eret_priv_w = io.in.info.inst(29, 28)


  val set   = isSet(op)
  val clear = isClear(op)

  val raddr = io.in.info.inst(31, 20)


  val current_priv_w = Wire(UInt(2.W))
  val csr_priv_r     = io.in.info.inst(29, 28)
  val csr_readonly_r = (io.in.info.inst(31, 30) === 3.U) // Machine-mode read-only CSRs
  val csr_write_r    = (io.in.info.rs1Idx =/= 0.U) | csrrw_w | csrrwi_w 
  val data_r        = Mux((csrrwi_w | csrrsi_w | csrrci_w), Cat(0.U(27.W), io.in.info.rs1Idx), io.in.info.rs1)

  val csr_fault_r    = SUPPORT_SUPER.B & (io.in.info.valid & (set | clear)) && ( (csr_write_r && csr_readonly_r) || (current_priv_w < csr_priv_r) )

  val satp_update = (io.in.info.valid && (set || clear) && csr_write_r && (io.in.info.inst(31, 20) === CSR_SATP))


//-----------------------------------------------------------------
// CSR register file
//-----------------------------------------------------------------                        
  val misa_w = "h40001100".U(32.W) // RV32IM

  val csr_rdata_w  = Wire(UInt(32.W))
  val csr_branch_w = Wire(Bool())
  val csr_target_w = Wire(UInt(32.W))
  val interrupt_w  = Wire(UInt(32.W))
  val status_reg_w = Wire(UInt(32.W))
  val satp_reg_w   = Wire(UInt(32.W))

  val csr_rf = Module(new CL3CSRRF())
  csr_rf.io.ext_intr   := io.in.ext_irq
  csr_rf.io.timer_intr := io.in.timer_irq

  csr_rf.io.raddr    := raddr
  csr_rf.io.ren      := io.in.info.valid // TODO:
  csr_rf.io.bootAddr := io.in.bootAddr

  csr_rf.io.waddr := io.in.wb.waddr
  csr_rf.io.wdata := io.in.wb.wdata
  csr_rf.io.wen   := io.in.wb.wen
  csr_rf.io.trap  := io.in.wb.except.code
  csr_rf.io.addr  := io.in.wb.except.addr
  csr_rf.io.pc    := io.in.wb.except.pc
  // csr_writeback_exception_addr_i
  csr_rf.io.inst  := io.in.info.inst

  csr_rdata_w  := csr_rf.io.rdata
  csr_branch_w := csr_rf.io.br.valid
  csr_target_w := csr_rf.io.br.pc
  interrupt_w  := csr_rf.io.irq
  status_reg_w := csr_rf.io.status
  satp_reg_w   := csr_rf.io.satp
  
  current_priv_w := csr_rf.io.br.priv
//-----------------------------------------------------------------
// CSR Read Result (E1) / Early exceptions
//-----------------------------------------------------------------

  val rd_valid_e1_q  = RegInit(false.B)
  val rd_result_e1_q = RegInit(0.U(32.W))
  val csr_wdata_e1_q = RegInit(0.U(32.W))
  val exception_e1_q = RegInit(0.U((EXCEPTION_W).W))

  val eret_fault_w   = eret_w & (current_priv_w < eret_priv_w)

  when (io.in.info.valid) {
    rd_valid_e1_q := (set || clear ) && ~csr_fault_r

    // Invalid instruction / CSR access fault?
    // Record opcode for writing to csr_xtval later.
    when ( csr_fault_r || eret_fault_w || io.in.wb.invalid) {
      rd_result_e1_q := io.in.info.inst
    } .otherwise {
      rd_result_e1_q := csr_rdata_w
    }

    when(set && clear){
      csr_wdata_e1_q := data_r
    } .elsewhen(set){
      csr_wdata_e1_q := data_r | csr_rdata_w
    } .elsewhen(clear){
      csr_wdata_e1_q := (~data_r).asUInt & csr_rdata_w
    }
  }

  when(!io.in.info.valid) {
    rd_valid_e1_q := false.B
    exception_e1_q := 0.U
    csr_wdata_e1_q := 0.U
    rd_result_e1_q := 0.U
  }
  
  when(ecall_w) {
    exception_e1_q := EXCEPTION_ECALL + current_priv_w
  } .elsewhen(eret_fault_w) {
    exception_e1_q := EXCEPTION_ILLEGAL_INSTRUCTION
  } .elsewhen(eret_w){
    exception_e1_q := EXCEPTION_ERET_U + eret_priv_w
  }.elsewhen(ebreak_w) {
    exception_e1_q := EXCEPTION_BREAKPOINT
  } .elsewhen( io.in.wb.invalid || csr_fault_r ) {
    exception_e1_q := EXCEPTION_ILLEGAL_INSTRUCTION
  } .elsewhen( satp_update | ifence_w | sfence_w ) {
    exception_e1_q := EXCEPTION_FENCE
  } .otherwise {
    exception_e1_q := 0.U
  }


  io.out.info.rdata  := rd_result_e1_q
  io.out.info.except.code := exception_e1_q
  io.out.info.except.pc   := RegNext(io.in.info.pc)
  io.out.info.except.addr := 0.U
  io.out.info.wdata  := csr_wdata_e1_q
  io.out.info.wen    := rd_valid_e1_q

// Interrupt launch enable
  io.out.irq     := RegNext(interrupt_w.orR & !io.in.irq_inhibit)

// ifence
  io.out.ifence  := RegNext(ifence_w)



//-----------------------------------------------------------------
// Execute - Branch operations
//-----------------------------------------------------------------
  val branch_q        = RegInit(false.B)
  val branch_target_q = RegInit(0.U(32.W))
  val reset_q         = RegInit(true.B)

  when(reset_q) {
    branch_target_q := io.in.bootAddr
    branch_q        := true.B
    reset_q         := false.B
  } .otherwise {
    branch_q        := csr_branch_w
    branch_target_q := csr_target_w
  }

  io.out.info.br.valid := branch_q
  io.out.info.br.pc    := branch_target_q
  io.out.info.br.priv  := csr_rf.io.br.priv

// MMU TODO
  io.out.mmu     := 0.U.asTypeOf(new MMUCtrlInfo)


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
      case "fence"  => BitPat(OP_FENCE)
      case _        => BitPat.dontCare(CSR_OP_WIDTH)
    }
  }
}
