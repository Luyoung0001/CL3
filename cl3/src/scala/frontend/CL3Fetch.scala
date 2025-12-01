package cl3

import chisel3._
import chisel3.util._

class FEIO extends Bundle {
  val mem   = new CL3ICacheIO
  val br    = Input(new BrInfo)
  val de    = Decoupled(Output(new FERawInfo))
  val bp    = Flipped(new NPCIO)
  val flush = Input(Bool())

  val debug = new Bundle {
    val branch = Output(Bool())
    val drop_resp = Output(Bool())
  }
}

class CL3Fetch() extends Module with CL3Config {

  val io = IO(new FEIO)

  val outstanding_q = RegInit(false.B)
  when(io.mem.req.fire) {
    outstanding_q := true.B
  }.elsewhen(io.mem.resp.valid) {
    outstanding_q := false.B
  }

  val active_q    = RegInit(false.B)
  val mem_is_busy = outstanding_q && !io.mem.resp.valid
  val stall       = !io.de.ready || mem_is_busy || !io.mem.req.ready

  val branch_q    = RegInit(false.B)
  when(io.br.valid) {
    branch_q := true.B
  }.elsewhen(io.mem.resp.valid) {
    branch_q := false.B
  }

  val branch = io.br.valid || branch_q

  when(branch) {
    active_q := true.B
  }

  val stall_q = RegNext(stall, false.B)

  val pc_q        = RegInit(0.U(32.W))
  val last_pc_q   = RegInit(0.U(32.W))
  val last_pred_q = RegInit(0.U(2.W))

  when(branch && (stall || !active_q)) {
    pc_q := io.br.pc
  }.elsewhen(!stall) {
    pc_q := io.bp.npc
  }

  val icache_pc   = Wire(UInt(32.W))
  val icache_priv = Wire(UInt(2.W))
  val drop_resp   = Wire(Bool())

  icache_pc   := Mux(branch, io.br.pc, pc_q)
  icache_priv := 0.U
  drop_resp   := branch

  // Last fetch address
  when(io.mem.req.fire) {
    last_pc_q := icache_pc(31, 2) ## 0.U(2.W)
  }

  when(io.mem.req.fire) {
    last_pred_q := io.bp.taken
  }.elsewhen(io.mem.resp.valid) {
    last_pred_q := 0.U
  }

  io.debug.branch := branch
  io.debug.drop_resp := drop_resp
  dontTouch(io.debug)

  io.mem.req.valid           := active_q && io.de.ready && !mem_is_busy
  io.mem.req.bits.wdata      := 0.U
  io.mem.req.bits.mask       := "b1111".U
  io.mem.req.bits.cacheable  := true.B // TODO:
  io.mem.req.bits.size       := 3.U
  io.mem.req.bits.wen        := false.B
  // io.mem.req.bits.addr       := Cat(icache_pc(31, 3), 0.U(3.W))
  io.mem.req.bits.addr       := icache_pc(31, 2) ## 0.U(2.W)
  io.mem.req.bits.flush      := false.B //TODO:
  io.mem.req.bits.invalidate := false.B
    
  val skid_buffer_q = RegInit(0.U.asTypeOf(new FERawInfo))
  val skid_valid_q  = RegInit(false.B)

  // TODO:
  when(io.de.valid && !io.de.ready) {
    skid_valid_q  := true.B
    skid_buffer_q := io.de.bits
  }.otherwise {
    skid_valid_q := false.B
  }

  val fetch_valid = io.mem.resp.valid && !drop_resp
  val skid_valid  = skid_valid_q && !drop_resp
  // val fetch_pc    = Cat(last_pc_q(31, 3), 0.U(3.W))
  val fetch_pc = last_pc_q

  io.de.valid     := fetch_valid || skid_valid
  io.de.bits.pc   := Mux(skid_valid_q, skid_buffer_q.pc, fetch_pc)
  io.de.bits.inst := Mux(skid_valid_q, skid_buffer_q.inst, io.mem.resp.bits.rdata)
  io.de.bits.pred := Mux(skid_valid_q, skid_buffer_q.pred, last_pred_q)
  io.de.bits.fault_fetch := Mux(skid_valid_q, skid_buffer_q.fault_fetch, io.mem.resp.bits.err)
  io.de.bits.fault_page  := false.B

  // TODO: add trap support

  io.bp.pc     := icache_pc(31, 2) ## 0.U(2.W)
  io.bp.accept := !stall
}
