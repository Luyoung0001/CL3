package cl3

import chisel3._
import chisel3.util._

trait MMUConstant {

//TODO:
  val CacheableAddrMin = "h00000000".U
  val CacheableAddrMax = "h80000000".U

  val PAGE_PRESENT  = 0
  val PAGE_READ     = 1
  val PAGE_WRITE    = 2
  val PAGE_EXEC     = 3
  val PAGE_USER     = 4
  val PAGE_GLOBAL   = 5
  val PAGE_ACCESSED = 6
  val PAGE_DIRTY    = 7
}

object MMUConstant extends MMUConstant {}

trait PrivConstant {

  val PRIV_STATES     = Enum(4)
  val PRIV_USER       = PRIV_STATES(0)
  val PRIV_SUPER      = PRIV_STATES(1)
  val PRIV_HYPERVISOR = PRIV_STATES(2)
  val PRIV_MACHINE    = PRIV_STATES(3)

}

trait IRQConstant {
  val IRQ_USIP = 0; val IRQ_SSIP = 1; val IRQ_MSIP = 3
  val IRQ_UTIP = 4; val IRQ_STIP = 5; val IRQ_MTIP = 7
  val IRQ_UEIP = 8; val IRQ_SEIP = 9; val IRQ_MEIP = 11
  val IRQ_MASK: UInt = (
    (1 << IRQ_USIP) | (1 << IRQ_SSIP) | (1 << IRQ_MSIP) |
      (1 << IRQ_UTIP) | (1 << IRQ_STIP) | (1 << IRQ_MTIP) |
      (1 << IRQ_UEIP) | (1 << IRQ_SEIP) | (1 << IRQ_MEIP)
  ).U(32.W)

}

trait CSRMstatusConstant extends PrivConstant with IRQConstant {

  val SR_UIE_R = 0
  val SR_UIE   = (1 << SR_UIE_R)
  val SR_SIE_R = 1
  val SR_SIE   = (1 << SR_SIE_R)

  val SR_MIE_R  = 3
  val SR_MIE    = (1 << SR_MIE_R)
  val SR_UPIE_R = 4
  val SR_UPIE   = (1 << SR_UPIE_R)
  val SR_SPIE_R = 5
  val SR_SPIE   = (1 << SR_SPIE_R)
  val SR_MPIE_R = 7
  val SR_MPIE   = (1 << SR_MPIE_R)
  val SR_SPP_R  = 8
  val SR_SPP    = (1 << SR_SPP_R)

  val SR_MPP_SHIFT = 11
  val SR_MPP_MASK  = 3
  val SR_MPP_R     = 0
  val SR_MPP_U     = PRIV_USER
  val SR_MPP_S     = PRIV_SUPER
  val SR_MPP_M     = PRIV_MACHINE
  val SR_SUM_R     = 18
  val SR_SUM       = (1 << SR_SUM_R)
  val SR_MPRV_R    = 17
  val SR_MPRV      = (1 << SR_MPRV_R)
  val SR_MXR_R     = 19
  val SR_MXR       = (1 << SR_MXR_R)

  val SR_SMODE_MASK = (SR_UIE | SR_SIE | SR_UPIE | SR_SPIE | SR_SPP | SR_SUM)

  val SR_IP_MSIP_R = IRQ_MSIP
  val SR_IP_MTIP_R = IRQ_MTIP
  val SR_IP_MEIP_R = IRQ_MEIP
  val SR_IP_SSIP_R = IRQ_SSIP
  val SR_IP_STIP_R = IRQ_STIP
  val SR_IP_SEIP_R = IRQ_SEIP
}

trait CSROpConstant {

  val CSR_OP_WIDTH = 5

  val OP_ECALL  = "b00001".U(CSR_OP_WIDTH.W)
  val OP_EBREAK = "b00010".U(CSR_OP_WIDTH.W)
  val OP_ERET   = "b00100".U(CSR_OP_WIDTH.W)

  val OP_CSRRW  = "b01001".U(CSR_OP_WIDTH.W)
  val OP_CSRRS  = "b01010".U(CSR_OP_WIDTH.W)
  val OP_CSRRC  = "b01100".U(CSR_OP_WIDTH.W)
  val OP_CSRRWI = "b11001".U(CSR_OP_WIDTH.W)
  val OP_CSRRSI = "b11010".U(CSR_OP_WIDTH.W)
  val OP_CSRRCI = "b11100".U(CSR_OP_WIDTH.W)

  val OP_FENCE  = "b10001".U(CSR_OP_WIDTH.W)
  val OP_SFENCE = "b10010".U(CSR_OP_WIDTH.W)
  val OP_IFENCE = "b10100".U(CSR_OP_WIDTH.W)

  val OP_WFI = "b11111".U(CSR_OP_WIDTH.W)

  def isSet(op: UInt): Bool = (op === OP_CSRRS) || (op === OP_CSRRSI) || (op === OP_CSRRW) || (op === OP_CSRRWI)


  def isClear(op: UInt): Bool = (op === OP_CSRRC) || (op === OP_CSRRCI) || (op === OP_CSRRW) || (op === OP_CSRRWI)

}

trait CSRTrapNO {
  val EXCEPTION_W                   = 6
  val EXCEPTION_MISALIGNED_FETCH    = "h10".U(EXCEPTION_W.W)
  val EXCEPTION_FAULT_FETCH         = "h11".U(EXCEPTION_W.W)
  val EXCEPTION_ILLEGAL_INSTRUCTION = "h12".U(EXCEPTION_W.W)
  val EXCEPTION_BREAKPOINT          = "h13".U(EXCEPTION_W.W)
  val EXCEPTION_MISALIGNED_LOAD     = "h14".U(EXCEPTION_W.W)
  val EXCEPTION_FAULT_LOAD          = "h15".U(EXCEPTION_W.W)
  val EXCEPTION_MISALIGNED_STORE    = "h16".U(EXCEPTION_W.W)
  val EXCEPTION_FAULT_STORE         = "h17".U(EXCEPTION_W.W)

  val EXCEPTION_ECALL               = "h18".U(EXCEPTION_W.W)
  val EXCEPTION_ECALL_U             = "h18".U(EXCEPTION_W.W)
  val EXCEPTION_ECALL_S             = "h19".U(EXCEPTION_W.W)
  val EXCEPTION_ECALL_H             = "h1a".U(EXCEPTION_W.W)
  val EXCEPTION_ECALL_M             = "h1b".U(EXCEPTION_W.W)
  val EXCEPTION_PAGE_FAULT_INST     = "h1c".U(EXCEPTION_W.W)
  val EXCEPTION_PAGE_FAULT_LOAD     = "h1d".U(EXCEPTION_W.W)
  val EXCEPTION_PAGE_FAULT_STORE    = "h1f".U(EXCEPTION_W.W)
  val EXCEPTION_EXCEPTION           = "h10".U(EXCEPTION_W.W)
  val EXCEPTION_INTERRUPT           = "h20".U(EXCEPTION_W.W)

  val EXCEPTION_ERET_U              = "h30".U(EXCEPTION_W.W)
  val EXCEPTION_ERET_S              = "h31".U(EXCEPTION_W.W)
  val EXCEPTION_ERET_H              = "h32".U(EXCEPTION_W.W)
  val EXCEPTION_ERET_M              = "h33".U(EXCEPTION_W.W)
  
  val EXCEPTION_FENCE               = "h34".U(EXCEPTION_W.W)
  val EXCEPTION_TYPE_MASK           = "h30".U(EXCEPTION_W.W)

  private val pcSet   = Seq(EXCEPTION_MISALIGNED_FETCH, EXCEPTION_FAULT_FETCH, EXCEPTION_PAGE_FAULT_INST)
  private val addrSet = Seq(
    EXCEPTION_ILLEGAL_INSTRUCTION,
    EXCEPTION_MISALIGNED_LOAD,
    EXCEPTION_FAULT_LOAD,
    EXCEPTION_MISALIGNED_STORE,
    EXCEPTION_FAULT_STORE,
    EXCEPTION_PAGE_FAULT_LOAD,
    EXCEPTION_PAGE_FAULT_STORE
  )
  def isPcStval(exception: UInt): Bool =
    pcSet.map(exception === _).reduce(_ || _)

  def isAddrStval(exception: UInt): Bool =
    addrSet.map(exception === _).reduce(_ || _)

  val MCAUSE_INT                 = 31
  val MCAUSE_MISALIGNED_FETCH    = ((0.U << MCAUSE_INT) | 0.U)
  val MCAUSE_FAULT_FETCH         = ((0.U << MCAUSE_INT) | 1.U)
  val MCAUSE_ILLEGAL_INSTRUCTION = ((0.U << MCAUSE_INT) | 2.U)
  val MCAUSE_BREAKPOINT          = ((0.U << MCAUSE_INT) | 3.U)
  val MCAUSE_MISALIGNED_LOAD     = ((0.U << MCAUSE_INT) | 4.U)
  val MCAUSE_FAULT_LOAD          = ((0.U << MCAUSE_INT) | 5.U)
  val MCAUSE_MISALIGNED_STORE    = ((0.U << MCAUSE_INT) | 6.U)
  val MCAUSE_FAULT_STORE         = ((0.U << MCAUSE_INT) | 7.U)
  val MCAUSE_ECALL_U             = ((0.U << MCAUSE_INT) | 8.U)
  val MCAUSE_ECALL_S             = ((0.U << MCAUSE_INT) | 9.U)
  val MCAUSE_ECALL_H             = ((0.U << MCAUSE_INT) | 10.U)
  val MCAUSE_ECALL_M             = ((0.U << MCAUSE_INT) | 11.U)
  val MCAUSE_PAGE_FAULT_INST     = ((0.U << MCAUSE_INT) | 12.U)
  val MCAUSE_PAGE_FAULT_LOAD     = ((0.U << MCAUSE_INT) | 13.U)
  val MCAUSE_PAGE_FAULT_STORE    = ((0.U << MCAUSE_INT) | 15.U)
  val MCAUSE_INTERRUPT           = (1.U << MCAUSE_INT)

}

object CSRTrapNO extends CSRTrapNO {}

trait CSRRFMask extends IRQConstant {
  val XLEN             = 32
  val CSR_MSTATUS_MASK = "hFFFFFFFF".U(XLEN.W)
  val CSR_MISA_MASK    = "hFFFFFFFF".U(XLEN.W)
  val CSR_MEDELEG_MASK = "h0000FFFF".U(XLEN.W)
  val CSR_MIDELEG_MASK = "h0000FFFF".U(XLEN.W)
  // IRQ_MASK
  // MIP/MIE: USIP(0) SSIP(1) MSIP(3)  UTIP(4) STIP(5) MTIP(7)  UEIP(8) SEIP(9) MEIP(11)

  val CSR_MIE_MASK = IRQ_MASK
  val CSR_MIP_MASK = IRQ_MASK

  val CSR_MTVEC_MASK    = "hFFFFFFFF".U(XLEN.W)
  val CSR_MSCRATCH_MASK = "hFFFFFFFF".U(XLEN.W)
  val CSR_MEPC_MASK     = "hFFFFFFFF".U(XLEN.W)
  val CSR_MCAUSE_MASK   = "h8000000F".U(XLEN.W) // [31]=interrupt；[3:0]=code
  val CSR_MTVAL_MASK    = "hFFFFFFFF".U(XLEN.W)
  val CSR_MCYCLE_MASK   = "hFFFFFFFF".U(XLEN.W)
  val CSR_MTIME_MASK    = "hFFFFFFFF".U(XLEN.W)
  val CSR_MTIMEH_MASK   = "hFFFFFFFF".U(XLEN.W)
  val CSR_MHARTID_MASK  = "hFFFFFFFF".U(XLEN.W)
  val CSR_MTIMECMP_MASK = "hFFFFFFFF".U(XLEN.W)

  // -------- s-mode csrs --------
  // ：SD(31), MXR(19), SUM(18), XS(16..15), FS(14..13), SPP(8), SPIE(5), SIE(1)
  val CSR_SSTATUS_MASK = (
    (1.U << 31) | (1.U << 19) | (1.U << 18) | (3.U << 15) | (3.U << 13) | (1.U << 8) | (1.U << 5) | (1.U << 1)
  )

  // SIE/SIP （SEIP/STIP/SSIP）
  val CSR_SIE_MASK: UInt = ((1 << IRQ_SEIP) | (1 << IRQ_STIP) | (1 << IRQ_SSIP)).U(XLEN.W)
  val CSR_STVEC_MASK    = "hFFFFFFFF".U(XLEN.W)
  val CSR_SSCRATCH_MASK = "hFFFFFFFF".U(XLEN.W)
  val CSR_SEPC_MASK     = "hFFFFFFFF".U(XLEN.W)
  val CSR_SCAUSE_MASK   = "h8000000F".U(XLEN.W)
  val CSR_STVAL_MASK    = "hFFFFFFFF".U(XLEN.W)
  val CSR_SIP_MASK: UInt = CSR_SIE_MASK
  val CSR_SATP_MASK = "hFFFFFFFF".U(XLEN.W)

  // -------- misa --------
  val MISA_RV32 = "h40000000".U(XLEN.W)
  val MISA_RVI  = "h00000100".U(XLEN.W)
  val MISA_RVE  = "h00000010".U(XLEN.W)
  val MISA_RVM  = "h00001000".U(XLEN.W)
  val MISA_RVA  = "h00000001".U(XLEN.W)
  val MISA_RVF  = "h00000020".U(XLEN.W)
  val MISA_RVD  = "h00000008".U(XLEN.W)
  val MISA_RVC  = "h00000004".U(XLEN.W)
  val MISA_RVS  = "h00040000".U(XLEN.W)
  val MISA_RVU  = "h00100000".U(XLEN.W)
}

trait CSRConstant extends CSRMstatusConstant with CSROpConstant with CSRTrapNO with CSRRFMask {

  // Machine
  val CSR_MSTATUS   = "h300".U(12.W)
  val CSR_MISA      = "h301".U(12.W)
  val CSR_MEDELEG   = "h302".U(12.W)
  val CSR_MIDELEG   = "h303".U(12.W)
  val CSR_MIE       = "h304".U(12.W)
  val CSR_MTVEC     = "h305".U(12.W)
  val CSR_MSCRATCH  = "h340".U(12.W)
  val CSR_MEPC      = "h341".U(12.W)
  val CSR_MCAUSE    = "h342".U(12.W)
  val CSR_MTVAL     = "h343".U(12.W)
  val CSR_MIP       = "h344".U(12.W)
  val CSR_MVENDORID = "hf11".U(12.W)
  val CSR_MARCHID   = "hf12".U(12.W)
  val CSR_MIMPID    = "hf13".U(12.W)
  val CSR_MHARTID   = "hf14".U(12.W)

  // Non-standard
  val CSR_MTIMECMP = "h7c0".U(12.W)

  // Supervisor
  val CSR_SSTATUS  = "h100".U(12.W)
  val CSR_SIE      = "h104".U(12.W)
  val CSR_STVEC    = "h105".U(12.W)
  val CSR_SSCRATCH = "h140".U(12.W)
  val CSR_SEPC     = "h141".U(12.W)
  val CSR_SCAUSE   = "h142".U(12.W)
  val CSR_STVAL    = "h143".U(12.W)
  val CSR_SIP      = "h144".U(12.W)
  val CSR_SATP     = "h180".U(12.W)

  val DCSR_ADDR      = 0x7b0.U(12.W)
  val DPC_ADDR       = 0x7b1.U(12.W)
  val DSCRATCH0_ADDR = 0x7b2.U(12.W)
  val DSCRATCH1_ADDR = 0x7b3.U(12.W)

}

object CSRConstant extends CSRConstant {}

//TODO: use a more elegant way
trait OpConstant {

  val OP0_WIDTH = 4
  val OP1_WIDTH = 3
  val OP2_WIDTH = 3

  val OP0_ADD  = "b0001".U(OP0_WIDTH.W)
  val OP0_SUB  = "b0101".U(OP0_WIDTH.W)
  val OP0_AND  = "b0010".U(OP0_WIDTH.W)
  val OP0_OR   = "b0011".U(OP0_WIDTH.W)
  val OP0_XOR  = "b0100".U(OP0_WIDTH.W)
  val OP0_SLT  = "b0111".U(OP0_WIDTH.W)
  val OP0_SLTU = "b0110".U(OP0_WIDTH.W)
  val OP0_SLL  = "b1000".U(OP0_WIDTH.W)
  val OP0_SRL  = "b1100".U(OP0_WIDTH.W)
  val OP0_SRA  = "b1110".U(OP0_WIDTH.W)
  val OP0_NONE = "b0000".U(OP0_WIDTH.W)

  val OP1_Z   = "b000".U(OP1_WIDTH.W)
  val OP1_REG = "b001".U(OP1_WIDTH.W)
  val OP1_PC  = "b010".U(OP1_WIDTH.W)
  val OP1_BEQ = "b100".U(OP1_WIDTH.W)
  val OP1_BGE = "b101".U(OP1_WIDTH.W)
  val OP1_BNE = "b110".U(OP1_WIDTH.W)
  val OP1_BLT = "b111".U(OP1_WIDTH.W)

  val OP2_REG  = "b001".U(OP2_WIDTH.W)
  val OP2_NONE = "b000".U(OP2_WIDTH.W)
  val OP2_IMMU = "b100".U(OP2_WIDTH.W)
  val OP2_IMMI = "b101".U(OP2_WIDTH.W)
  val OP2_IMMB = "b110".U(OP2_WIDTH.W)
  val OP2_IMMJ = "b111".U(OP2_WIDTH.W)

}

object OpConstant extends OpConstant {}

trait LSUConstant extends CSRTrapNO {

  val LSU_WIDTH = 5

  val LSU_A_LS_BIT = LSU_WIDTH - 1
  val LSU_LS_BIT   = LSU_WIDTH - 2
  val LSU_SIGN_BIT = 0
  val LSU_1B       = 1
  val LSU_2B       = 2
  val LSU_4B       = 3

// |---L/S---|---Size(2 bit)---|---sign---|
// signal[3]: 0 -> Load, 1 -> Store
// signal[2:1]: 0 -> invalid, 1 -> 1 byte, 2 -> 2 bytes, 3 -> 4 bytes
// signal[0]: 0 -> signed, 1 -> unsigned

//TODO: support xlen = 64
  val LSU_XXX   = "b00000".U(LSU_WIDTH.W)
  val LSU_SB    = "b01010".U(LSU_WIDTH.W)
  val LSU_SH    = "b01100".U(LSU_WIDTH.W)
  val LSU_SW    = "b01110".U(LSU_WIDTH.W)
  val LSU_LB    = "b00010".U(LSU_WIDTH.W)
  val LSU_LBU   = "b00011".U(LSU_WIDTH.W)
  val LSU_LH    = "b00100".U(LSU_WIDTH.W)
  val LSU_LHU   = "b00101".U(LSU_WIDTH.W)
  val LSU_LW    = "b00110".U(LSU_WIDTH.W)
  
  val ATO_LR   = "b10000".U(LSU_WIDTH.W)
  val ATO_SC   = "b10001".U(LSU_WIDTH.W)
  val ATO_SWAP = "b10010".U(LSU_WIDTH.W)
  val ATO_ADD  = "b10011".U(LSU_WIDTH.W)
  val ATO_XOR  = "b10100".U(LSU_WIDTH.W)
  val ATO_AND  = "b10101".U(LSU_WIDTH.W)
  val ATO_OR   = "b10110".U(LSU_WIDTH.W)
  val ATO_MIN  = "b10111".U(LSU_WIDTH.W)
  val ATO_MAX  = "b11000".U(LSU_WIDTH.W)
  val ATO_MINU = "b11001".U(LSU_WIDTH.W)
  val ATO_MAXU = "b11010".U(LSU_WIDTH.W)


  val MASK_Z   = "b0000".U(4.W)
  val MASK_ALL = "b1111".U(4.W)
  val MASK_HI  = "b1100".U(4.W)
  val MASK_LO  = "b0011".U(4.W)
  val MASK_B0  = "b0001".U(4.W)
  val MASK_B1  = "b0010".U(4.W)
  val MASK_B2  = "b0100".U(4.W)
  val MASK_B3  = "b1000".U(4.W)
}

object LSUConstant extends LSUConstant {}

trait AXIConstant {

  val AXI_FIXED = 0.U
  val AXI_INCR  = 1.U
  val AXI_WRAP  = 2.U

}

object AXIConstant extends AXIConstant {}

trait PipeConstant {
  val P0_E1 = "b000".U(3.W)
  val P0_E2 = "b010".U(3.W)
  val P0_WB = "b100".U(3.W)
  val P1_E1 = "b001".U(3.W)
  val P1_E2 = "b011".U(3.W)
  val P1_WB = "b101".U(3.W)
  val NONE  = "b111".U(3.W)
}

object PipeConstant extends PipeConstant {}
