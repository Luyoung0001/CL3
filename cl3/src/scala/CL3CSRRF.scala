package cl3

import chisel3._
import chisel3.util._
import os.makeDir

class CSRRFInput extends Bundle {
  val irq_e = Input(Bool()) // external interrupt
  val irq_t = Input(Bool()) // timer interrupt

  val ren   = Input(Bool())
  val raddr = Input(UInt(12.W))

  val wen   = Input(Bool())
  val waddr = Input(UInt(12.W))
  val wdata = Input(UInt(32.W))

  val trap = Input(UInt(6.W))

  val pc   = Input(UInt(32.W))
  val tval = Input(UInt(32.W))
  val inst = Input(UInt(32.W))

  val baddr = Input(UInt(32.W)) // boot address

}

class CSRRFOutput extends Bundle {

  val rdata = Output(UInt(32.W))
  val br    = Output(new BrInfo)
  val irq   = Output(UInt(32.W))

}

class CSRRFIO() extends Bundle {

  val in  = new CSRRFInput
  val out = new CSRRFOutput

  val status = Output(UInt(32.W))
  /*  Used for difftest to verify the Next PC (NPC). When an exception occurs,
    the NPC matches the tvec register. This signal is required for that
    comparison and will be optimized away when difftest is disabled. */
  val tvec = Output(UInt(32.W))
  val epc  = Output(UInt(32.W))

}

class CL3CSRRF() extends Module with CSRConstant {

  val io = IO(new CSRRFIO())

  val exception = io.in.trap
  val waddr     = io.in.waddr
  val wdata     = io.in.wdata

  val csr_mepc_q     = RegInit(0.U(32.W))
  val csr_sr_q       = RegInit("h1800".U(32.W))
  val csr_mcause_q   = RegInit(0.U(32.W))
  val csr_mtvec_q    = RegInit(0.U(32.W))
  val csr_mip_q      = RegInit(0.U(32.W))
  val csr_mie_q      = RegInit(0.U(32.W))
  val csr_mpriv_q    = RegInit(PRIV_MACHINE)
  val csr_mscratch_q = RegInit(0.U(32.W))
  val csr_mtval_q    = RegInit(0.U(32.W))

  val csr_mip_next_q = RegInit(0.U(32.W))

  val irq_pending = csr_mip_q & csr_mie_q
  val irq_masked  = Mux(csr_sr_q(SR_MIE_R), irq_pending, 0.U)
  val irq_priv    = PRIV_MACHINE

  val irq_priv_q = RegEnable(irq_priv, PRIV_MACHINE, irq_masked.orR)
  io.out.irq := irq_masked

  val csr_mip_upd_q = RegInit(false.B)
  when(io.in.ren && io.in.raddr === CSR_MIP || io.in.ren && io.in.raddr === CSR_SIP) {
    csr_mip_upd_q := true.B
  }.elsewhen(io.in.waddr === CSR_MIP || io.in.waddr === CSR_SIP || exception.orR) {
    csr_mip_upd_q := false.B
  }
  val buffer_mip    = (io.in.ren && io.in.raddr === CSR_MIP) | (io.in.ren && io.in.raddr === CSR_SIP) | csr_mip_upd_q

  val csr_writable_table = Seq(
    (CSR_MSCRATCH, csr_mscratch_q, CSR_MSCRATCH_MASK),
    (CSR_MEPC, csr_mepc_q, CSR_MEPC_MASK),
    (CSR_MTVEC, csr_mtvec_q, CSR_MTVEC_MASK),
    (CSR_MCAUSE, csr_mcause_q, CSR_MCAUSE_MASK),
    (CSR_MTVAL, csr_mtval_q, CSR_MTVAL_MASK),
    (CSR_MSTATUS, csr_sr_q, CSR_MSTATUS_MASK),
    (CSR_MIP, csr_mip_q, CSR_MIP_MASK),
    (CSR_MIE, csr_mie_q, CSR_MIE_MASK)
  )

  val cpu_id_i = 0.U(32.W)           // TODO: get from outside
  val misa_i   = "h40001100".U(32.W) // TODO: get from outside

  val csr_readonly_table = Seq(
    CSR_MHARTID -> cpu_id_i,
    CSR_MISA    -> misa_i
  )

  val rw_mapping = csr_writable_table.map { case (addr, reg, _) => addr -> reg }

  val full_read_mapping = rw_mapping ++ csr_readonly_table

  val csr_rd_cases = full_read_mapping.map { case (k, v) => (io.in.raddr === k) -> v }
  val csr_rd_hit   = csr_rd_cases.map(_._1).reduce(_ || _)

  io.out.rdata := Mux(csr_rd_hit, Mux1H(csr_rd_cases), 0.U)

  io.out.br.priv := csr_mpriv_q
  io.status  := csr_sr_q

  io.tvec := csr_mtvec_q
  io.epc  := csr_mepc_q
  val is_exception = exception(5, 4) === 1.U

  // Exception return
  when((exception & EXCEPTION_TYPE_MASK) === EXCEPTION_INTERRUPT) {
    csr_sr_q := Cat(
      csr_sr_q(31, 13),
      // SR_MPP_U, // Set next MPP to user mode
      csr_mpriv_q, // Keep MPP bits
      csr_sr_q(10, 8),
      csr_sr_q(SR_MIE_R),
      csr_sr_q(6, 4),
      0.U(1.W),
      csr_sr_q(2, 0)
    )

    csr_mpriv_q := PRIV_MACHINE
    csr_mepc_q  := io.in.pc
    csr_mtval_q := 0.U

    val csr_mcause_next = MuxCase(
      csr_mcause_q,
      Seq(
        io.out.irq(IRQ_MSIP) -> (MCAUSE_INTERRUPT + IRQ_MSIP.U),
        io.out.irq(IRQ_MTIP) -> (MCAUSE_INTERRUPT + IRQ_MTIP.U),
        io.out.irq(IRQ_MEIP) -> (MCAUSE_INTERRUPT + IRQ_MEIP.U)
      )
    )
    csr_mcause_q := csr_mcause_next

  }.elsewhen(exception === EXCEPTION_ERET_M) {
    csr_mpriv_q := csr_sr_q(12, 11) // MPP bits
    csr_sr_q    := Cat(
      csr_sr_q(31, 13),
      // SR_MPP_U, // Set next MPP to user mode
      csr_sr_q(12, 11), // Keep MPP bits
      csr_sr_q(10, 8),
      1.U(1.W),         // Set MPIE to 1
      csr_sr_q(6, 4),
      csr_sr_q(SR_MPIE_R),
      csr_sr_q(2, 0)
    )
  }.elsewhen(is_exception) {
    csr_sr_q    := Cat(
      csr_sr_q(31, 13),
      csr_mpriv_q, // Save Interrupts / machine mode
      csr_sr_q(10, 8),
      csr_sr_q(1),
      csr_sr_q(6, 4),
      0.U(1.W),    // Disable interrupts and enter machine mode
      csr_sr_q(2, 0)
    )
    csr_mpriv_q := PRIV_MACHINE

    csr_mepc_q   := io.in.pc
    csr_mtval_q  := io.in.tval
    csr_mcause_q := exception(3, 0)

  }.elsewhen(io.in.wen) {
    csr_writable_table.foreach { case (addr, reg, mask) =>
      when(io.in.waddr === addr) {
        reg := wdata & mask
      }
    }
  }

  val csr_mip_next = MuxCase(
    csr_mip_next_q,
    Seq(
      io.in.irq_e -> (csr_mip_next_q | (1.U(32.W) << SR_IP_MEIP_R)),
      io.in.irq_t -> (csr_mip_next_q | (1.U(32.W) << SR_IP_MTIP_R))
    )
  )

  val hwTimerBit = (1.U(32.W) << SR_IP_MTIP_R)
  val hwExtBit   = (1.U(32.W) << SR_IP_MEIP_R)
  val hwMask     = (1.U(32.W) << SR_IP_MTIP_R) |
    (1.U(32.W) << SR_IP_MEIP_R)
  val hwPending  = Mux(io.in.irq_t, hwTimerBit, 0.U) |
    Mux(io.in.irq_e, hwExtBit, 0.U)

  csr_mip_q := ((csr_mip_q | csr_mip_next_q) & ~hwMask) | hwPending

//-----------------------------------------------------------------
// Sequential
//-----------------------------------------------------------------
  csr_mip_next_q := Mux(buffer_mip, csr_mip_next_q, 0.U)

  io.out.br.valid := Mux(exception(5, 4).orR, true.B, false.B)
  io.out.br.pc    := Mux(exception === EXCEPTION_ERET_M, csr_mepc_q, csr_mtvec_q)

}
