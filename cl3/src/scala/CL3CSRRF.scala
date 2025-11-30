package cl3

import chisel3._
import chisel3.util._

class CSRRFIO() extends Bundle {

  val ext_intr   = Input(Bool())
  val timer_intr = Input(Bool())
  val ren   = Input(Bool()) // Reading CSR also has side effects
  val raddr = Input(UInt(12.W))
  val rdata = Output(UInt(32.W))

  val wen   = Input(Bool())
  val waddr = Input(UInt(12.W))
  val wdata = Input(UInt(32.W))

  val trap = Input(UInt(6.W))

  val pc       = Input(UInt(32.W))
  val addr     = Input(UInt(32.W))
  val inst     = Input(UInt(32.W))
  val bootAddr = Input(UInt(32.W))

  val br = Output(new BrInfo)

  val irq = Output(UInt(32.W))

  val status = Output(UInt(32.W))
  val satp   = Output(UInt(32.W))


}

class CL3CSRRF() extends Module with CSRConstant {
  val SUPPORT_SUPER = false
  val SUPPORT_MTIMECMP = false
  val io = IO(new CSRRFIO())
  
  val exception      = io.trap
  val waddr          = io.waddr
  val wdata          = io.wdata
  val csr_mepc_q     = RegInit(0.U(32.W))  
  val csr_sr_q       = RegInit("h1800".U(32.W)) 
  val csr_mcause_q   = RegInit(0.U(32.W))    
  val csr_mtvec_q    = RegInit(0.U(32.W))   
  val csr_mip_q      = RegInit(0.U(32.W)) 
  val csr_mie_q      = RegInit(0.U(32.W)) 
  val csr_mpriv_q    = RegInit(PRIV_MACHINE)   
  val csr_mcycle_q   = RegInit(0.U(32.W))    
  val csr_mcycle_h_q = RegInit(0.U(32.W))      
  val csr_mscratch_q = RegInit(0.U(32.W))      
  val csr_mtval_q    = RegInit(0.U(32.W)) 
  val csr_mtimecmp_q = RegInit(0.U(32.W))
  val csr_mtime_ie_q = RegInit(false.B)
  val csr_medeleg_q  = RegInit(0.U(32.W))
  val csr_mideleg_q  = RegInit(0.U(32.W))

  val csr_mip_next_q = RegInit(0.U(32.W))

  // CSR S-Mode
  val csr_sepc_q     = RegInit(0.U(32.W))
  val csr_stvec_q    = RegInit(0.U(32.W)) 
  val csr_scause_q   = RegInit(0.U(32.W))  
  val csr_stval_q    = RegInit(0.U(32.W)) 
  val csr_satp_q     = RegInit(0.U(32.W))
  val csr_sscratch_q = RegInit(0.U(32.W))    

  // Interrupts
  val irq_pending_r  = RegInit(0.U(32.W))   
  val irq_masked_r   = RegInit(0.U(32.W))  
  val irq_priv_r     = RegInit(0.U(2.W))
  val m_enabled_r    = RegInit(false.B) 
  val m_interrupts_r = RegInit(0.U(32.W))    
  val s_enabled_r    = RegInit(false.B) 
  val s_interrupts_r = RegInit(0.U(32.W))
  
  // TODO
  if(SUPPORT_SUPER) {
    
  } else {
    irq_pending_r := csr_mip_q & csr_mie_q
    irq_masked_r  := Mux(csr_sr_q(SR_MIE_R), irq_pending_r, 0.U)
    irq_priv_r    := PRIV_MACHINE
  }

  val irq_priv_q = RegEnable(irq_priv_r, PRIV_MACHINE, irq_masked_r.orR)
  io.irq := irq_masked_r


  val csr_mip_upd_q = RegInit(false.B)
  when( io.ren && io.raddr === CSR_MIP || io.ren && io.raddr === CSR_SIP) {
    csr_mip_upd_q := true.B
  } .elsewhen(io.waddr === CSR_MIP || io.waddr === CSR_SIP || exception.orR) {
    csr_mip_upd_q := false.B
  }
  val buffer_mip_w = (io.ren && io.raddr === CSR_MIP) | (io.ren && io.raddr === CSR_SIP) | csr_mip_upd_q
//-----------------------------------------------------------------
// CSR Read Port
//-----------------------------------------------------------------

  val cpu_id_i = 0.U(32.W) // TODO: get from outside
  val misa_i   = "h40001100".U(32.W) // TODO: get from outside

  val rdata_r = MuxLookup(io.raddr, 0.U)(Seq(
    CSR_MSCRATCH -> csr_mscratch_q,
    CSR_MEPC     -> csr_mepc_q,
    CSR_MTVEC    -> csr_mtvec_q,
    CSR_MCAUSE   -> csr_mcause_q,
    CSR_MTVAL    -> csr_mtval_q,
    CSR_MSTATUS  -> csr_sr_q,
    CSR_MIP      -> csr_mip_q,
    CSR_MIE      -> csr_mie_q,
    CSR_MCYCLE   -> csr_mcycle_q,
    CSR_MTIME    -> csr_mcycle_q,
    CSR_MTIMEH   -> csr_mcycle_h_q,
    CSR_MHARTID  -> cpu_id_i,
    CSR_MISA     -> misa_i,
    CSR_MEDELEG  -> (if (SUPPORT_SUPER) (csr_medeleg_q & CSR_MEDELEG_MASK) else 0.U),
    CSR_MIDELEG  -> (if (SUPPORT_SUPER) (csr_mideleg_q & CSR_MIDELEG_MASK) else 0.U),
    CSR_MTIMECMP -> (if (SUPPORT_MTIMECMP) csr_mtimecmp_q else 0.U),
    CSR_SSTATUS  -> (if (SUPPORT_SUPER) (csr_sr_q & CSR_SSTATUS_MASK) else 0.U),
    CSR_SIP      -> (if (SUPPORT_SUPER) (csr_mip_q & CSR_SIP_MASK) else 0.U),
    CSR_SIE      -> (if (SUPPORT_SUPER) (csr_mie_q & CSR_SIE_MASK) else 0.U),
    CSR_SEPC     -> (if (SUPPORT_SUPER) (csr_sepc_q & CSR_SEPC_MASK) else 0.U),
    CSR_STVEC    -> (if (SUPPORT_SUPER) (csr_stvec_q & CSR_STVEC_MASK) else 0.U),
    CSR_SCAUSE   -> (if (SUPPORT_SUPER) (csr_scause_q & CSR_SCAUSE_MASK) else 0.U),
    CSR_STVAL    -> (if (SUPPORT_SUPER) (csr_stval_q & CSR_STVAL_MASK) else 0.U),
    CSR_SATP     -> (if (SUPPORT_SUPER) (csr_satp_q & CSR_SATP_MASK) else 0.U),
    CSR_SSCRATCH -> (if (SUPPORT_SUPER) (csr_sscratch_q & CSR_SSCRATCH_MASK) else 0.U)
  ))
  io.rdata := rdata_r
  io.br.priv := csr_mpriv_q
  io.status  := csr_sr_q
  io.satp    := csr_satp_q

//-----------------------------------------------------------------
// CSR register next state
//-----------------------------------------------------------------
  // CSR M-mode
  val csr_mip_next_r = WireDefault(csr_mip_next_q)
  val csr_mepc_r     = WireDefault(csr_mepc_q)
  val csr_mcause_r   = WireDefault(csr_mcause_q)
  val csr_mtval_r    = WireDefault(csr_mtval_q)
  val csr_sr_r       = WireDefault(csr_sr_q)
  val csr_mtvec_r    = WireDefault(csr_mtvec_q)
  val csr_mip_r      = WireDefault(csr_mip_q)
  val csr_mie_r      = WireDefault(csr_mie_q)
  val csr_mpriv_r    = WireDefault(csr_mpriv_q)
  val csr_mcycle_r   = WireDefault((csr_mcycle_q + 1.U))
  val csr_mscratch_r = WireDefault(csr_mscratch_q)
  val csr_mtimecmp_r = WireDefault(csr_mtimecmp_q)
  val csr_mtime_ie_r = WireDefault(csr_mtime_ie_q)
  val csr_medeleg_r  = WireDefault(csr_medeleg_q)
  val csr_mideleg_r  = WireDefault(csr_mideleg_q)

  // CSR S-mode
  val csr_sepc_r     = WireDefault(csr_sepc_q)
  val csr_stvec_r    = WireDefault(csr_stvec_q)
  val csr_scause_r   = WireDefault(csr_scause_q)
  val csr_stval_r    = WireDefault(csr_stval_q)
  val csr_satp_r     = WireDefault(csr_satp_q)
  val csr_sscratch_r = WireDefault(csr_sscratch_q)

  val is_exception_w = exception(5, 4) === 1.U
  val exception_s_w  = SUPPORT_SUPER.B && (csr_mpriv_q <= PRIV_SUPER) && is_exception_w && csr_medeleg_q(exception(3, 0))

  // Exception return
  when ((exception & EXCEPTION_TYPE_MASK)  === EXCEPTION_INTERRUPT) {
    when (irq_priv_q === PRIV_MACHINE) {
      csr_sr_r := Cat( csr_sr_q(31,13), 
                  // SR_MPP_U, // Set next MPP to user mode
                  csr_mpriv_q, // Keep MPP bits 
                  csr_sr_q(10, 8), 
                  csr_sr_q(SR_MIE_R),
                  csr_sr_q(6, 4), 
                  0.U(1.W), 
                  csr_sr_q(2, 0))

      csr_mpriv_r := PRIV_MACHINE
      csr_mepc_r  := io.pc
      csr_mtval_r := 0.U

    val csr_mcause_next = MuxCase(csr_mcause_q, Seq(
      io.irq(IRQ_MSIP) -> (MCAUSE_INTERRUPT + IRQ_MSIP.U),
      io.irq(IRQ_MTIP) -> (MCAUSE_INTERRUPT + IRQ_MTIP.U),
      io.irq(IRQ_MEIP) -> (MCAUSE_INTERRUPT + IRQ_MEIP.U)
    ))
    csr_mcause_r := csr_mcause_next

    } .otherwise { } // todo Supervisor mode interrupts
  
  } .elsewhen ( exception >= EXCEPTION_ERET_U && exception <= EXCEPTION_ERET_M ) {
    when( exception(1, 0) === PRIV_MACHINE) {
      csr_mpriv_r := csr_sr_q(12, 11) // MPP bits

      csr_sr_r := Cat( csr_sr_q(31,13), 
                        // SR_MPP_U, // Set next MPP to user mode
                        csr_sr_q(12, 11), // Keep MPP bits 
                        csr_sr_q(10, 8), 
                        1.U(1.W),  // Set MPIE to 1
                        csr_sr_q(6, 4), 
                        csr_sr_q(SR_MPIE_R), 
                        csr_sr_q(2, 0))
    } .otherwise{
      csr_mpriv_r := Mux(csr_sr_r(SR_SPP_R), PRIV_SUPER, PRIV_USER)
      csr_sr_r := Cat( csr_sr_q(31, 9), 
                        0.U(1.W), // Set next SPP to user mode
                        csr_sr_q(7, 6), 
                        1.U(1.W),  // Set MPIE to 1
                        csr_sr_q(4, 2), 
                        csr_sr_q(SR_SPIE_R), 
                        csr_sr_q(0))
    }

  // Exception - handled in super mode
  } .elsewhen(is_exception_w && exception_s_w){
      csr_sr_r := Cat( csr_sr_q(31, 9), 
                    csr_mpriv_q === PRIV_SUPER, // Save Interrupts / supervisor mode
                    csr_sr_q(7, 6), 
                    csr_sr_q(1),
                    csr_sr_q(4, 2), 
                    0.U(1.W), // Disable interrupts and enter supervisor mode
                    csr_sr_q(0))
      csr_mpriv_r := PRIV_SUPER

      // record exception pc
      csr_sepc_r  := io.pc

      csr_stval_r := Mux(isPcStval(exception), io.pc,
                        Mux(isAddrStval(exception), io.addr, 0.U(32.W)))
      csr_scause_r := exception(3, 0)
  // Exception - handled in machine mode
  } .elsewhen(is_exception_w){
      csr_sr_r := Cat( csr_sr_q(31, 13), 
                    csr_mpriv_q, // Save Interrupts / machine mode
                    csr_sr_q(10, 8), 
                    csr_sr_q(1),
                    csr_sr_q(6, 4), 
                    0.U(1.W), // Disable interrupts and enter machine mode
                    csr_sr_q(2, 0))
      csr_mpriv_r := PRIV_MACHINE

      // record exception pc
      csr_mepc_r  := io.pc

      csr_mtval_r := Mux(isPcStval(exception), io.pc,
                        Mux(isAddrStval(exception), io.addr, 0.U(32.W)))
      csr_mcause_r := exception(3, 0)
  } .otherwise{
    switch (io.waddr) {
      // Machine
      is (CSR_MSCRATCH) { csr_mscratch_r := wdata & CSR_MSCRATCH_MASK }
      is (CSR_MEPC)     { csr_mepc_r     := wdata & CSR_MEPC_MASK     }
      is (CSR_MTVEC)    { csr_mtvec_r    := wdata & CSR_MTVEC_MASK    }
      is (CSR_MCAUSE)   { csr_mcause_r   := wdata & CSR_MCAUSE_MASK   }
      is (CSR_MTVAL)    { csr_mtval_r    := wdata & CSR_MTVAL_MASK    }
      is (CSR_MSTATUS)  { csr_sr_r       := wdata & CSR_MSTATUS_MASK  }
      is (CSR_MIP)      { csr_mip_r      := wdata & CSR_MIP_MASK      }
      is (CSR_MIE)      { csr_mie_r      := wdata & CSR_MIE_MASK      }
      is (CSR_MEDELEG)  { csr_medeleg_r  := wdata & CSR_MEDELEG_MASK  }
      is (CSR_MIDELEG)  { csr_mideleg_r  := wdata & CSR_MIDELEG_MASK  }

      // Non-std behaviour
      is (CSR_MTIMECMP) {
        csr_mtimecmp_r := wdata & CSR_MTIMECMP_MASK
        csr_mtime_ie_r  := true.B
      }

      // Supervisor
      is (CSR_SEPC)     { csr_sepc_r     := wdata & CSR_SEPC_MASK     }
      is (CSR_STVEC)    { csr_stvec_r    := wdata & CSR_STVEC_MASK    }
      is (CSR_SCAUSE)   { csr_scause_r   := wdata & CSR_SCAUSE_MASK   }
      is (CSR_STVAL)    { csr_stval_r    := wdata & CSR_STVAL_MASK    }
      is (CSR_SATP)     { csr_satp_r     := wdata & CSR_SATP_MASK     }
      is (CSR_SSCRATCH) { csr_sscratch_r := wdata & CSR_SSCRATCH_MASK }

      is (CSR_SSTATUS) { csr_sr_r  := csr_sr_q & !CSR_SSTATUS_MASK | (wdata & CSR_SSTATUS_MASK) }
      is (CSR_SIP)     { csr_mip_r := csr_mip_q & !CSR_SIP_MASK | (wdata & CSR_SIP_MASK) }
      is (CSR_SIE)     { csr_mie_r := csr_mie_q & !CSR_SIE_MASK | (wdata & CSR_SIE_MASK) }
    }
  }

  when(io.ext_intr   && csr_mideleg_q(SR_IP_MEIP_R))  { csr_mip_next_r := csr_mip_next_q | (1.U(32.W) << SR_IP_SEIP_R) }
  when(io.ext_intr   && !csr_mideleg_q(SR_IP_MEIP_R)) { csr_mip_next_r := csr_mip_next_q | (1.U(32.W) << SR_IP_MEIP_R) }
  when(io.timer_intr && csr_mideleg_q(SR_IP_MTIP_R))  { csr_mip_next_r := csr_mip_next_q | (1.U(32.W) << SR_IP_STIP_R) }
  when(io.timer_intr && !csr_mideleg_q(SR_IP_MTIP_R)) { csr_mip_next_r := csr_mip_next_q | (1.U(32.W) << SR_IP_MTIP_R) }

  val hwTimerBit = Mux(csr_mideleg_q(SR_IP_MTIP_R),
                      (1.U(32.W) << SR_IP_STIP_R),
                      (1.U(32.W) << SR_IP_MTIP_R))
  val hwExtBit   = Mux(csr_mideleg_q(SR_IP_MEIP_R),
                      (1.U(32.W) << SR_IP_SEIP_R),
                      (1.U(32.W) << SR_IP_MEIP_R))
  val hwMask     = ((1.U(32.W) << SR_IP_STIP_R) |
                    (1.U(32.W) << SR_IP_MTIP_R) |
                    (1.U(32.W) << SR_IP_SEIP_R) |
                    (1.U(32.W) << SR_IP_MEIP_R))
  val hwPending  = Mux(io.timer_intr, hwTimerBit, 0.U) |
                   Mux(io.ext_intr,   hwExtBit,   0.U)

  csr_mip_r := ((csr_mip_q | csr_mip_next_r) & ~hwMask) | hwPending

  // Optional: Internal timer compare interrupt TODO

//-----------------------------------------------------------------
// Sequential
//-----------------------------------------------------------------
  csr_mepc_q     := csr_mepc_r
  csr_sr_q       := csr_sr_r
  csr_mcause_q   := csr_mcause_r
  csr_mtval_q    := csr_mtval_r
  csr_mtvec_q    := csr_mtvec_r
  csr_mip_q      := csr_mip_r
  csr_mie_q      := csr_mie_r
  csr_mpriv_q    := (if(SUPPORT_SUPER) csr_mpriv_r else PRIV_MACHINE)
  csr_mcycle_q   := csr_mcycle_r
  csr_mscratch_q := csr_mscratch_r
  csr_mtimecmp_q := (if(SUPPORT_MTIMECMP) csr_mtimecmp_r else 0.U)
  csr_mtime_ie_q := (if(SUPPORT_MTIMECMP) csr_mtime_ie_r else 0.U)
  csr_medeleg_q  := (if(SUPPORT_SUPER) csr_medeleg_r & CSR_MEDELEG_MASK  else 0.U)
  csr_mideleg_q  := (if(SUPPORT_SUPER) csr_mideleg_r & CSR_MIDELEG_MASK else 0.U)

  csr_sepc_q     := (if(SUPPORT_SUPER) csr_sepc_r     & CSR_SEPC_MASK     else 0.U)
  csr_stvec_q    := (if(SUPPORT_SUPER) csr_stvec_r    & CSR_STVEC_MASK else 0.U)
  csr_scause_q   := (if(SUPPORT_SUPER) csr_scause_r   & CSR_SCAUSE_MASK else 0.U)
  csr_stval_q    := (if(SUPPORT_SUPER) csr_stval_r    & CSR_STVAL_MASK else 0.U)
  csr_satp_q     := (if(SUPPORT_SUPER) csr_satp_r     & CSR_SATP_MASK else 0.U)
  csr_sscratch_q := (if(SUPPORT_SUPER) csr_sscratch_r & CSR_SSCRATCH_MASK else 0.U)

  csr_mip_next_q := Mux(buffer_mip_w, csr_mip_next_r, 0.U)
  when(csr_mcycle_q === "hffffffff".U) {
    csr_mcycle_h_q := csr_mcycle_h_q + 1.U
  }

//-----------------------------------------------------------------
// CSR branch
//-----------------------------------------------------------------
  val branch_r = WireDefault(false.B)
  val branch_target_r = WireDefault(0.U(32.W))

  // Interrupts
  when(exception === EXCEPTION_INTERRUPT){
    branch_r := true.B
    branch_target_r := Mux(irq_priv_r === PRIV_MACHINE, csr_mtvec_q, csr_stvec_q)

  // Exception return
  } .elsewhen(exception >= EXCEPTION_ERET_U && exception <= EXCEPTION_ERET_M){
    // MRET (return from machine mode)
    when( exception(1, 0) === PRIV_MACHINE) {
      branch_r := true.B
      branch_target_r := csr_mepc_q
    // SRET (return from supervisor mode)
    } .otherwise{
      branch_r := true.B
      branch_target_r := csr_sepc_q
    }
  // Exception - handled in super mode
  } .elsewhen(is_exception_w && exception_s_w){
      branch_r := true.B
      branch_target_r := csr_stvec_q
  // Exception - handled in machine mode
  } .elsewhen (is_exception_w) {
      branch_r := true.B
      branch_target_r := csr_mtvec_q
  // Fence / SATP register writes cause pipeline flush
  } .elsewhen (exception === EXCEPTION_FENCE) {
      branch_r := true.B
      branch_target_r := io.pc + 4.U
  }

  io.br.valid := branch_r
  io.br.pc    := branch_target_r

}

// class CL3CSRRF() extends Module with CSRConstant {
//   val io = IO(new CSRRFIO())

//   val csr_waddr = io.waddr
//   val csr_raddr = io.raddr
//   val csr_wen   = io.wen
//   val csr_ren   = io.ren
//   val csr_wdat  = io.wdata

//   // from debug module
//   val csr_update_en = false.B
//   val dpc_update    = false.B
//   val dbg_cause     = false.B

//   val wen_dcsr      = Wire(Bool())
//   val wen_dpc       = Wire(Bool())
//   val wen_dscratch0 = Wire(Bool())
//   val wen_dscratch1 = Wire(Bool())
//   val wen_mtvec     = Wire(Bool())
//   val wen_mepc      = Wire(Bool())
//   val wen_mcause    = Wire(Bool())
//   val wen_mscratch  = Wire(Bool())

//   class DCSRBundle extends Bundle {
//     val xdebugver = UInt(4.W)
//     val reserved1 = UInt(12.W)
//     val ebreakm   = UInt(1.W)
//     val reserved2 = UInt(1.W)
//     val ebreaks   = UInt(1.W)
//     val ebreaku   = UInt(1.W)
//     val stepie    = UInt(1.W)
//     val stopcount = UInt(1.W)
//     val stoptime  = UInt(1.W)
//     val cause     = UInt(3.W)
//     val reserved3 = UInt(1.W)
//     val mprven    = UInt(1.W)
//     val nmip      = UInt(3.W)
//     val step      = UInt(1.W)
//     val prv       = UInt(2.W)
//   }

//   val dcsr_wdat = csr_wdat.asTypeOf(new DCSRBundle)

//   val xdebugver = WireDefault(4.U(4.W))                          // debug support exists as it is described in this document
//   val ebreakm   = RegEnable(dcsr_wdat.ebreakm, 0.U(1.W), wen_dcsr)
//   val ebreaks   = RegEnable(dcsr_wdat.ebreaks, 0.U(1.W), wen_dcsr)
//   val ebreaku   = RegEnable(dcsr_wdat.ebreaku, 0.U(1.W), wen_dcsr)
//   val stepie    = WireDefault(0.U(1.W))                          // Interrupts are disabled during single stepping
//   val stopcount = WireDefault(0.U(1.W))                          // Stop counting when the debug module is halted
//   val stoptime  = WireDefault(0.U(1.W))                          // Don't incrment any hart-loacal timers while in Debug Mode
//   val cause     = RegEnable(dcsr_wdat.cause, 0.U(3.W), wen_dcsr) // debug cause
//   val mprven    = WireDefault(0.U(1.W))                          // MPRV in mstatus is ignored
//   val nmip      = RegEnable(dcsr_wdat.nmip, 0.U(3.W), wen_dcsr)  // Non-maskable interrupt pending
//   val step      = RegEnable(dcsr_wdat.step, 0.U(1.W), wen_dcsr)  // Single step
//   val prv       = RegEnable(dcsr_wdat.prv, 0.U(3.W), wen_dcsr)   // privilege level when Debug Mode was entered

// //TODO:
//   val dcsr = Cat(
//     xdebugver,
//     0.U(12.W),
//     ebreakm,
//     0.U(1.W),
//     ebreaks,
//     ebreaku,
//     stepie,
//     stopcount,
//     stoptime,
//     cause,
//     0.U(1.W),
//     mprven,
//     nmip,
//     step,
//     prv
//   )

//   // Debug PC (dpc, at 0x7b1)
//   val dpc_wdat = Mux1H(
//     Seq(
//       csr_update_en -> dpc_update,
//       wen_dpc       -> csr_wdat
//     )
//   )
//   val dpc      = RegEnable(dpc_wdat, 0.U(32.W), wen_dpc) // Debug PC

//   // Debug Scratch (dscratch0, at 0x7b2)
//   val dscratch0 = RegEnable(csr_wdat, 0.U(32.W), wen_dscratch0) // Debug Scratch 0

//   // Debug Scratch (dscratch1, at 0x7b3)
//   val dscratch1 = RegEnable(csr_wdat, 0.U(32.W), wen_dscratch1) // Debug Scratch 1

//   val mstatus  = RegInit("h1800".U(32.W))
//   val mtvec    = RegEnable(csr_wdat, 0.U(32.W), wen_mtvec)
//   val mscratch = RegEnable(csr_wdat, 0.U(32.W), wen_mscratch)

//   val mepc_wdata = Mux(io.trap.orR, io.pc, csr_wdat)
//   val mepc       = RegEnable(mepc_wdata, 0.U(32.W), wen_mepc)
//   val mcause     = RegEnable(io.trap, 0.U(32.W), wen_mcause)
//   // val mhartid    = RegInit(0.U)

//   val mip        = RegInit(0.U(32.W))
//   val mie        = RegInit(0.U(32.W))
//   val mtval      = RegInit(0.U(32.W))


//   val allCSRs = Seq(
//     MSTATUS_ADDR   -> mstatus,
//     MTVEC_ADDR     -> mtvec,
//     MSCRATCH_ADDR  -> mscratch,
//     MEPC_ADDR      -> mepc,
//     MCAUSE_ADDR    -> mcause,
//     DCSR_ADDR      -> dcsr,
//     DPC_ADDR       -> dpc,
//     DSCRATCH0_ADDR -> dscratch0,
//     DSCRATCH1_ADDR -> dscratch1,
//     MHARTID_ADDR   -> mhartid
//   )

//   val csr_rselOh = VecInit(allCSRs.map { case (addr, reg) => csr_raddr === addr })
//   val csr_wselOh = VecInit(allCSRs.map { case (addr, reg) => csr_waddr === addr })
//   val csr_renOh  = csr_rselOh.map(_ && csr_ren)
//   val csr_wenOh  = csr_wselOh.map(_ && csr_wen)

//   def getCSRIndex(targetAddr: UInt): Int = {
//     val index = allCSRs.indexWhere { case (addr, _) =>
//       addr.litValue == targetAddr.litValue
//     }
//     require(index >= 0, s"CSR ${targetAddr.litValue} is undefined")
//     index
//   }

//   def getCSRWen(targetAddr: UInt): Bool = {
//     val staticIndex = getCSRIndex(targetAddr)
//     csr_wenOh(staticIndex)
//   }

//   wen_dcsr      := getCSRWen(DCSR_ADDR)
//   wen_dpc       := getCSRWen(DPC_ADDR)
//   wen_dscratch0 := getCSRWen(DSCRATCH0_ADDR)
//   wen_dscratch1 := getCSRWen(DSCRATCH1_ADDR)
//   wen_mtvec     := getCSRWen(MTVEC_ADDR)
//   wen_mepc      := getCSRWen(MEPC_ADDR) || io.trap.orR
//   wen_mcause    := io.trap.orR
//   wen_mscratch  := getCSRWen(MSCRATCH_ADDR)

//   val csr_regs = allCSRs.map { case (addr, reg) => reg }
//   val csr_rdat = Mux1H(csr_renOh, csr_regs)

//   io.rdata := csr_rdat

//   io.irq := false.B // TODO:

//   val reset_q = RegInit(true.B)
//   val br_q    = RegInit(0.U.asTypeOf(new BrInfo))

//   when(reset_q) {
//     reset_q    := false.B
//     br_q.pc    := io.bootAddr
//     br_q.valid := true.B
//   }.otherwise { // TODO:  trap
//     br_q.valid := false.B
//   }

//   br_q.priv := 0.U // TODO:

//   io.br := br_q

//   io.status := 0.U
//   io.satp   := 0.U
// }
