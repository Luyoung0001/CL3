package cl3

import chisel3._
import chisel3.util._

// Performance monitor for I-Cache.
class ICachePerf(p: ICacheParams) extends Module with CL3Config {
  val io = IO(new Bundle {
    // Event pulses from ICache
    val ev_req_fire          = Input(Bool())                              // cpu.req_rd && cpu.resp_accept
    val ev_miss              = Input(Bool())                              // lookup_valid_q && !tag_hit_any_w
    val ev_refill_burst_fire = Input(Bool())                              // ar.valid && ar.ready
    val ev_refill_beat_fire  = Input(Bool())                              // r.valid
    val ev_refill_line_last  = Input(Bool())                              // r.valid && r.bits.last
    val ev_stall_cycle       = Input(Bool())                              // cpu.req_rd && !cpu.resp_accept
    val ev_miss_penalty_cyc  = Input(Bool())                              // state in REFILL/RELOOKUP
    val ev_flush             = Input(Bool())                              // cpu.req_flush
    val ev_invalidate        = Input(Bool())                              // cpu.req_invalidate
    val ev_axi_err           = Input(Bool())                              // r.bits.resp =/= 0.U

    // Counter/telemetry outputs (placeholder widths)

  })

  // Event-driven counters
  val req_cnt            = RegInit(0.U(64.W))
  val miss_cnt           = RegInit(0.U(64.W))
  val refill_burst_cnt   = RegInit(0.U(64.W))
  val refill_beat_cnt    = RegInit(0.U(64.W))
  val refill_line_cnt    = RegInit(0.U(64.W))
  val stall_cycle_cnt    = RegInit(0.U(64.W))
  val miss_penalty_cnt   = RegInit(0.U(64.W))
  val flush_cnt          = RegInit(0.U(64.W))
  val invalidate_cnt     = RegInit(0.U(64.W))
  val axi_err_cnt        = RegInit(0.U(64.W))

  when(io.ev_req_fire)          { req_cnt          := req_cnt + 1.U }
  when(io.ev_miss)              { miss_cnt         := miss_cnt + 1.U }
  when(io.ev_refill_burst_fire) { refill_burst_cnt := refill_burst_cnt + 1.U }
  when(io.ev_refill_beat_fire)  { refill_beat_cnt  := refill_beat_cnt + 1.U }
  when(io.ev_refill_line_last)  { refill_line_cnt  := refill_line_cnt + 1.U }
  when(io.ev_stall_cycle)       { stall_cycle_cnt  := stall_cycle_cnt + 1.U }
  when(io.ev_miss_penalty_cyc)  { miss_penalty_cnt := miss_penalty_cnt + 1.U }
  when(io.ev_flush)             { flush_cnt        := flush_cnt + 1.U }
  when(io.ev_invalidate)        { invalidate_cnt   := invalidate_cnt + 1.U }
  when(io.ev_axi_err)           { axi_err_cnt      := axi_err_cnt + 1.U }


  val icachePerf = Module(new ICachePerfHelper)
  icachePerf.io.req_count            := req_cnt
  icachePerf.io.miss_count           := miss_cnt
  icachePerf.io.refill_burst_count   := refill_burst_cnt
  icachePerf.io.refill_beat_count    := refill_beat_cnt
  icachePerf.io.refill_line_count    := refill_line_cnt
  icachePerf.io.stall_cycle_count    := stall_cycle_cnt
  icachePerf.io.miss_penalty_cycles  := miss_penalty_cnt
  icachePerf.io.flush_count          := flush_cnt
  icachePerf.io.invalidate_count     := invalidate_cnt
  icachePerf.io.axi_err_count        := axi_err_cnt

  // // Expose counters upstream for simulation tapping.
  // BoringUtils.addSource(io.req_count, "icache_perf_req_count")
  // BoringUtils.addSource(io.hit_count, "icache_perf_hit_count")
  // BoringUtils.addSource(io.miss_count, "icache_perf_miss_count")
  // BoringUtils.addSource(io.refill_burst_count, "icache_perf_refill_burst_count")
  // BoringUtils.addSource(io.refill_beat_count, "icache_perf_refill_beat_count")
  // BoringUtils.addSource(io.refill_line_count, "icache_perf_refill_line_count")
  // BoringUtils.addSource(io.stall_cycle_count, "icache_perf_stall_cycle_count")
  // BoringUtils.addSource(io.miss_penalty_cycles, "icache_perf_miss_penalty_cycles")
  // BoringUtils.addSource(io.axi_backpress_cycles, "icache_perf_axi_backpress_cycles")
  // BoringUtils.addSource(io.flush_count, "icache_perf_flush_count")
  // BoringUtils.addSource(io.invalidate_count, "icache_perf_invalidate_count")
  // BoringUtils.addSource(io.axi_err_count, "icache_perf_axi_err_count")
  // BoringUtils.addSource(io.way_replace_counts, "icache_perf_way_replace_counts")
}

class ICachePerfHelper extends BlackBox with HasBlackBoxInline {
    val io = IO(new Bundle {
      val req_count            = Input(UInt(64.W))
      val miss_count           = Input(UInt(64.W))
      val refill_burst_count   = Input(UInt(64.W))
      val refill_beat_count    = Input(UInt(64.W))
      val refill_line_count    = Input(UInt(64.W))
      val stall_cycle_count    = Input(UInt(64.W))
      val miss_penalty_cycles  = Input(UInt(64.W))
      val flush_count          = Input(UInt(64.W))
      val invalidate_count     = Input(UInt(64.W))
      val axi_err_count        = Input(UInt(64.W))
    })
    setInline("icache_perf.sv",
      s"""
        module ICachePerfHelper (
          input logic [63:0] req_count,             
          input logic [63:0] miss_count,             
          input logic [63:0] refill_burst_count,             
          input logic [63:0] refill_beat_count,             
          input logic [63:0] refill_line_count,             
          input logic [63:0] stall_cycle_count,             
          input logic [63:0] miss_penalty_cycles,             
          input logic [63:0] flush_count,
          input logic [63:0] invalidate_count,
          input logic [63:0] axi_err_count
        );
          import "DPI-C" function void perf_icache_show(
              input longint unsigned req_count,
              input longint unsigned miss_count,
              input longint unsigned refill_burst_count,
              input longint unsigned refill_beat_count,
              input longint unsigned refill_line_count,
              input longint unsigned stall_cycle_count,
              input longint unsigned miss_penalty_cycles,
              input longint unsigned flush_count,
              input longint unsigned invalidate_count,
              input longint unsigned axi_err_count
          );
          // Emit a single DPI update at end of simulation with I-Cache perf counters.
          final begin
              perf_icache_show(
                  req_count,
                  miss_count,
                  refill_burst_count,
                  refill_beat_count,
                  refill_line_count,
                  stall_cycle_count,
                  miss_penalty_cycles,
                  flush_count,
                  invalidate_count,
                  axi_err_count
              );
          end
        endmodule
      """.stripMargin

    )
}
