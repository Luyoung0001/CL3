package cl3

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

// Performance monitor for D-Cache (event-driven).
class DCachePerf(p: DCacheParams) extends Module {
  val io = IO(new Bundle {
    // Event pulses from DCache
    val ev_req_fire         = Input(Bool())                          // any request accepted
    val ev_read_req         = Input(Bool())                          // read request accepted
    val ev_write_req        = Input(Bool())                          // write request accepted
    val ev_miss             = Input(Bool())                          // miss detected
    val ev_refill_beat_fire = Input(Bool())                          // refill beat (r.valid)
    val ev_refill_line_last = Input(Bool())                          // last beat of line
    val ev_miss_penalty_cyc = Input(Bool())                          // cycles spent handling miss/refill
    val ev_writeback        = Input(Bool())                          // writeback to memory
    val ev_amo              = Input(Bool())                          // atomic op issued

  })

  val req_cnt           = RegInit(0.U(64.W))
  val read_req_cnt      = RegInit(0.U(64.W))
  val write_req_cnt     = RegInit(0.U(64.W))
  val miss_cnt          = RegInit(0.U(64.W))
  val refill_beat_cnt   = RegInit(0.U(64.W))
  val refill_line_cnt   = RegInit(0.U(64.W))
  val miss_penalty_cnt  = RegInit(0.U(64.W))
  val writeback_cnt     = RegInit(0.U(64.W))
  val amo_cnt           = RegInit(0.U(64.W))

  when(io.ev_req_fire)         { req_cnt          := req_cnt + 1.U }
  when(io.ev_read_req)         { read_req_cnt     := read_req_cnt + 1.U }
  when(io.ev_write_req)        { write_req_cnt    := write_req_cnt + 1.U }
  when(io.ev_miss)             { miss_cnt         := miss_cnt + 1.U }
  when(io.ev_refill_beat_fire) { refill_beat_cnt  := refill_beat_cnt + 1.U }
  when(io.ev_refill_line_last) { refill_line_cnt  := refill_line_cnt + 1.U }
  when(io.ev_miss_penalty_cyc) { miss_penalty_cnt := miss_penalty_cnt + 1.U }
  when(io.ev_writeback)        { writeback_cnt    := writeback_cnt + 1.U }
  when(io.ev_amo)              { amo_cnt          := amo_cnt + 1.U }

  val dcachePerf = Module(new DCachePerfHelper)

  dcachePerf.io.req_count           := req_cnt
  dcachePerf.io.read_req_count      := read_req_cnt
  dcachePerf.io.write_req_count     := write_req_cnt
  dcachePerf.io.miss_count          := miss_cnt
  dcachePerf.io.refill_beat_count   := refill_beat_cnt
  dcachePerf.io.refill_line_count   := refill_line_cnt
  dcachePerf.io.miss_penalty_cycles := miss_penalty_cnt
  dcachePerf.io.writeback_count     := writeback_cnt
  dcachePerf.io.amo_count           := amo_cnt

  // BoringUtils.addSource(io.req_count,           "dcache_perf_req_count")
  // BoringUtils.addSource(io.read_req_count,      "dcache_perf_read_req_count")
  // BoringUtils.addSource(io.write_req_count,     "dcache_perf_write_req_count")
  // BoringUtils.addSource(io.miss_count,          "dcache_perf_miss_count")
  // BoringUtils.addSource(io.refill_beat_count,   "dcache_perf_refill_beat_count")
  // BoringUtils.addSource(io.refill_line_count,   "dcache_perf_refill_line_count")
  // BoringUtils.addSource(io.miss_penalty_cycles, "dcache_perf_miss_penalty_cycles")
  // BoringUtils.addSource(io.writeback_count,     "dcache_perf_writeback_count")
  // BoringUtils.addSource(io.amo_count,           "dcache_perf_amo_count")
}


class DCachePerfHelper extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle {
      val req_count           = Input(UInt(64.W))
      val read_req_count      = Input(UInt(64.W))
      val write_req_count     = Input(UInt(64.W))
      val miss_count          = Input(UInt(64.W))
      val refill_beat_count   = Input(UInt(64.W))
      val refill_line_count   = Input(UInt(64.W))
      val miss_penalty_cycles = Input(UInt(64.W))
      val writeback_count     = Input(UInt(64.W))
      val amo_count           = Input(UInt(64.W))
    })
    setInline("dcache_perf.sv",
      s"""
        module DCachePerfHelper (
          input logic [63:0] req_count,             
          input logic [63:0] read_req_count,             
          input logic [63:0] write_req_count,             
          input logic [63:0] miss_count,             
          input logic [63:0] refill_beat_count,             
          input logic [63:0] refill_line_count,             
          input logic [63:0] miss_penalty_cycles,             
          input logic [63:0] writeback_count,
          input logic [63:0] amo_count
        );
          import "DPI-C" function void perf_dcache_show(
              input longint unsigned req_count,
              input longint unsigned read_req_count,
              input longint unsigned write_req_count,
              input longint unsigned miss_count,
              input longint unsigned refill_beat_count,
              input longint unsigned refill_line_count,
              input longint unsigned miss_penalty_cycles,
              input longint unsigned writeback_count,
              input longint unsigned amo_count
          );
          // Emit a single DPI update at end of simulation with I-Cache perf counters.
          final begin
              perf_dcache_show(
                req_count,
                read_req_count,
                write_req_count,
                miss_count,
                refill_beat_count,
                refill_line_count,
                miss_penalty_cycles,
                writeback_count,
                amo_count
              );
          end
        endmodule
      """.stripMargin

    )
}