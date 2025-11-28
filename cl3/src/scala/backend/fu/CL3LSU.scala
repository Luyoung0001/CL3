package cl3

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import cl3.CL3InstInfo._

class LSUInput extends Bundle {
  val info  = Input(new OpInfo)
  val mem   = Flipped(Valid(new CL3DCacheResp))
  val flush = Input(Bool())
}

class LSUOutput extends Bundle {
  val mem  = Decoupled(new CL3DCacheReq)
  val info = Flipped(new PipeLSUInput)
}

class LSUIO extends Bundle {
  val in  = new LSUInput
  val out = new LSUOutput
}

//TODO: check the timing path: issue -> lsu
//TODO: add exception support
class CL3LSU extends Module with LSUConstant {

  val io = IO(new LSUIO)

  val table = new DecodeTable(instPatterns, Seq(LSUOPField))
  val res   = table.decode(io.in.info.inst)
  val op    = res(LSUOPField)

  val inst = io.in.info.inst

  val Iimm = SignExt(inst(31, 20), 32)
  val Simm = SignExt(Cat(inst(31, 25), inst(11, 7)), 32)

  val is_load  = !op(LSU_LS_BIT)
  val is_store = op(LSU_LS_BIT)
  dontTouch(is_store)

  val complete_err_w = io.in.mem.bits.err.orR && io.in.mem.valid
  val complete_err_q = RegNext(complete_err_w)

  val addr = io.in.info.rs1 + Mux(is_load, Iimm, Simm)
  dontTouch(addr)
  val sizeSel = op(2, 1)
  val mask = MuxCase(MASK_Z, Seq(
    (sizeSel === LSU_1B.U(2.W)) -> UIntToOH(addr(1, 0), 4),
    (sizeSel === LSU_2B.U(2.W)) -> Mux(addr(1), MASK_HI, MASK_LO),
    (sizeSel === LSU_4B.U(2.W)) -> MASK_ALL
  ))

  val outstanding_q = RegInit(0.U(2.W))

  // when(io.out.mem.fire) {
  //   outstanding_q := outstanding_q + 1.U
  // }
  when(io.in.mem.valid && io.out.mem.fire) {
    outstanding_q := outstanding_q
  } .elsewhen(io.in.mem.valid) {
    outstanding_q := outstanding_q - 1.U
  } .elsewhen(io.out.mem.fire) {
    outstanding_q := outstanding_q + 1.U
  }

  val pending = outstanding_q.orR && complete_err_w

  val req_q       = RegInit(0.U.asTypeOf(new CL3DCacheReq))
  val op_q        = RegInit(0.U(4.W))
  val req_valid_q = RegInit(false.B)

  // when(io.in.flush) {
  //   req_valid_q := false.B
  // // }.elsewhen(io.in.info.valid && !(io.out.mem.valid && !io.out.mem.ready)) {
  // }.elsewhen(io.in.info.valid && !(req_valid_q && !io.out.mem.fire)) { //TODO:
  //   req_valid_q := true.B
  // }

  // excetion_detect
  val mem_unaligned_r    = WireDefault(false.B)  
  val mem_unaligned_e1_q = RegInit(false.B)
  val mem_unaligned_e2_q = RegInit(false.B)

  when(op(2, 1) === 3.U && io.in.info.valid) {
    mem_unaligned_r := (addr(1, 0).orR)
  } .elsewhen(op(2, 1) === 2.U && io.in.info.valid) {
    mem_unaligned_r := addr(0).asBool
  }

  mem_unaligned_e2_q := mem_unaligned_e1_q & !pending

  when(io.in.mem.valid){
    mem_unaligned_e1_q := false.B
  }
  when(mem_unaligned_e2_q || complete_err_w || io.in.flush) {
    req_valid_q := false.B
    req_q := 0.U.asTypeOf(new CL3DCacheReq)
  // }.elsewhen(io.in.info.valid && !(io.out.mem.valid && !io.out.mem.ready)) {

  } .elsewhen(io.in.info.valid && !(req_valid_q && !io.out.mem.fire)) {
    mem_unaligned_e1_q := mem_unaligned_r
    req_valid_q        := true.B
    req_q.addr         := addr
    req_q.wdata        := io.in.info.rs2
    req_q.wen          := is_store
    req_q.mask         := mask
    // req_q.cacheable := false.B // TODO:
    req_q.cacheable := (addr(31, 28) === "h8".U(4.W))
    req_q.pc           := io.in.info.pc

    op_q := op

  }.elsewhen(io.out.mem.fire && !io.in.info.valid) {
    req_valid_q := false.B
  }
  

  when(io.in.flush) {
    mem_unaligned_e1_q := false.B
    mem_unaligned_e2_q := false.B
  }

  io.out.mem.valid      := req_valid_q && !pending

  io.out.mem.valid      := req_valid_q && !pending && !io.in.flush
  io.out.mem.bits       := req_q
  io.out.mem.bits.mask  := Mux(req_q.wen, req_q.mask, 0.U)
  val is_word_mask = req_q.mask === MASK_ALL
  val is_half_mask = (req_q.mask === MASK_HI) || (req_q.mask === MASK_LO)
  val is_byte_mask = req_q.mask.orR && !is_half_mask && !is_word_mask

  val byte_data = Mux1H(Seq(
    req_q.mask(0) -> Cat(0.U(24.W), req_q.wdata(7, 0)),
    req_q.mask(1) -> Cat(0.U(16.W), req_q.wdata(7, 0), 0.U(8.W)),
    req_q.mask(2) -> Cat(0.U(8.W), req_q.wdata(7, 0), 0.U(16.W)),
    req_q.mask(3) -> Cat(req_q.wdata(7, 0), 0.U(24.W))
  ))
  val half_data = Mux(req_q.mask === MASK_HI, Cat(req_q.wdata(15, 0), 0.U(16.W)), Cat(0.U(16.W), req_q.wdata(15, 0)))

  val aligned_wdata = WireDefault(0.U(32.W))
  when(is_word_mask) {
    aligned_wdata := req_q.wdata
  } .elsewhen(is_half_mask) {
    aligned_wdata := half_data
  } .elsewhen(is_byte_mask) {
    aligned_wdata := byte_data
  }
  io.out.mem.bits.wdata := aligned_wdata

  class ReqRecord extends Bundle {
    val mask      = UInt(4.W)
    val op        = UInt(4.W)
    val cacheable = Bool()
    val addr      = UInt(32.W)
    val pc        = UInt(32.W)
  }

  val req_record_q = RegInit(0.U.asTypeOf(new ReqRecord))
  when(io.out.mem.fire) {
    req_record_q.mask      := req_q.mask
    req_record_q.op        := op_q
    req_record_q.cacheable := req_q.cacheable
    req_record_q.addr      := req_q.addr
    req_record_q.pc        := req_q.pc
  }

  val lb_data = Mux1H(
    Seq(
      req_record_q.mask(0) -> io.in.mem.bits.rdata(7, 0),
      req_record_q.mask(1) -> io.in.mem.bits.rdata(15, 8),
      req_record_q.mask(2) -> io.in.mem.bits.rdata(23, 16),
      req_record_q.mask(3) -> io.in.mem.bits.rdata(31, 24)
    )
  )

  val lh_data = Mux(req_record_q.mask(1, 0).andR, io.in.mem.bits.rdata(15, 0), io.in.mem.bits.rdata(31, 16))

  val lw_data = io.in.mem.bits.rdata

  val wb_data = WireDefault(0.U(32.W))
  when(mem_unaligned_e2_q) {
    wb_data := req_record_q.addr
  } .otherwise(
    wb_data := MuxLookup(req_record_q.op(2, 0), io.in.mem.bits.rdata)(
      Seq(
        "b010".U -> SignExt(lb_data, 32),
        "b011".U -> ZeroExt(lb_data, 32),
        "b100".U -> SignExt(lh_data, 32),
        "b101".U -> ZeroExt(lh_data, 32)
      )
    )
  )
  io.out.info.rdata := wb_data


  io.out.info.valid  := (io.in.mem.valid && outstanding_q.orR) | mem_unaligned_e2_q | complete_err_q

  val fault_load_align_w  = mem_unaligned_e2_q && !op_q(LSU_LS_BIT)
  val fault_store_align_w = mem_unaligned_e2_q && op_q(LSU_LS_BIT)
  val fault_load_bus_w    = (complete_err_w || complete_err_q) && !req_record_q.op(LSU_LS_BIT)
  val fault_store_bus_w   = (complete_err_w || complete_err_q) && req_record_q.op(LSU_LS_BIT)

  io.out.info.except.addr := req_record_q.addr
  io.out.info.except.pc := req_record_q.pc // TODO:
  io.out.info.except.code := MuxCase(0.U, Seq(
    ( fault_load_align_w) -> EXCEPTION_MISALIGNED_LOAD,
    ( fault_store_align_w) -> EXCEPTION_MISALIGNED_STORE,
    ( fault_load_bus_w) -> EXCEPTION_FAULT_LOAD,
    ( fault_store_bus_w) -> EXCEPTION_FAULT_STORE
  ))
  io.out.info.stall  := pending || io.out.mem.valid && !io.out.mem.ready || mem_unaligned_e1_q
  io.out.info.cacheable := req_record_q.cacheable

}

object LSUOPField extends DecodeField[InstructionPattern, UInt] {
  def name: String = "LSU OP"

  import LSUConstant._
  def chiselType: UInt = UInt(LSU_WIDTH.W)

  def genTable(op: InstructionPattern): BitPat = {

    op.name match {
      case "lw"  => BitPat(LSU_LW)
      case "lh"  => BitPat(LSU_LH)
      case "lhu" => BitPat(LSU_LHU)
      case "lb"  => BitPat(LSU_LB)
      case "lbu" => BitPat(LSU_LBU)
      case "sw"  => BitPat(LSU_SW)
      case "sh"  => BitPat(LSU_SH)
      case "sb"  => BitPat(LSU_SB)
      case _     => BitPat.dontCare(LSU_WIDTH)
    }
  }
}
