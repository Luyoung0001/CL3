package cl3

import chisel3._
import chisel3.util._

class CL3Top extends Module with CL3Config {

  val io = IO(new Bundle {
    val extIrq   = Input(Bool())
    val timerIrq = Input(Bool())
    val master   = new SimpleAXI4MasterBundle(AddrWidth, DataWidth, 4)
  })

  implicit val axiP: AXI4Params = AXI4Params()
  val dp: DCacheParams = DCacheParams()
  val ip: ICacheParams = ICacheParams()
  val core = Module(new CL3Core)

  if (SimMemOption == "SoC") {
    val icache = Module(new ICache(ip))
    icache.io.cpu.req_rd            := core.io.imem.req.valid
    core.io.imem.req.ready          := icache.io.cpu.resp_accept
    icache.io.cpu.req_pc            := core.io.imem.req.bits.addr
    icache.io.cpu.req_flush := core.io.imem.req.bits.flush
    dontTouch(core.io.imem.req.bits.flush)
    dontTouch(icache.io.cpu.req_flush)
    icache.io.cpu.req_invalidate := core.io.imem.req.bits.invalidate

    core.io.imem.resp.valid      := icache.io.cpu.resp_valid
    core.io.imem.resp.bits.err   := icache.io.cpu.resp_error
    core.io.imem.resp.bits.rdata := icache.io.cpu.resp_inst

    val dcache = Module(new DCache(dp))
    dcache.io.cpu.req.rd             := core.io.dmem.req.valid
    core.io.dmem.req.ready           := dcache.io.cpu.accept
    dcache.io.cpu.req.addr           := core.io.dmem.req.bits.addr
    dcache.io.cpu.req.cacheable      := core.io.dmem.req.bits.cacheable
    dcache.io.cpu.req.wr             := core.io.dmem.req.bits.mask
    dcache.io.cpu.req.dataWr    := core.io.dmem.req.bits.wdata
    dcache.io.cpu.req.invalidate := false.B
    dcache.io.cpu.req.writeback  := false.B
    dcache.io.cpu.req.flush      := false.B
    dcache.io.amo := DontCare
    dcache.io.cpu.req.reqTag := DontCare
    // dcache.io.cpu.resp.ready := true.B

    core.io.dmem.resp.valid      := dcache.io.cpu.resp.ack
    core.io.dmem.resp.bits.err   := dcache.io.cpu.resp.error
    core.io.dmem.resp.bits.rdata := dcache.io.cpu.resp.dataRd

    val u_arbiter = Module(new AxiArbiter())
    u_arbiter.io.icache_axi <> icache.io.axi
    u_arbiter.io.dcache_axi <> dcache.io.axi

    val clint = Module(new CL3CLINT)
    val xbar  = Module(new CL3Xbar)
    dontTouch(clint.io.irq)
    
    clint.io.axi <> xbar.io.clint
    io.master    <> xbar.io.top

    // Arbiter -> Xbar (Simple AXI master signals)
    xbar.io.xbar.aw.valid            := u_arbiter.io.mem_axi.aw.valid
    xbar.io.xbar.aw.bits.awaddr      := u_arbiter.io.mem_axi.aw.bits.addr
    xbar.io.xbar.aw.bits.awid        := u_arbiter.io.mem_axi.aw.bits.id
    xbar.io.xbar.aw.bits.awlen       := u_arbiter.io.mem_axi.aw.bits.len
    xbar.io.xbar.aw.bits.awsize      := u_arbiter.io.mem_axi.aw.bits.size
    xbar.io.xbar.aw.bits.awburst     := u_arbiter.io.mem_axi.aw.bits.burst
    xbar.io.xbar.aw.bits.awlock      := u_arbiter.io.mem_axi.aw.bits.lock
    xbar.io.xbar.aw.bits.awcache     := u_arbiter.io.mem_axi.aw.bits.cache
    xbar.io.xbar.aw.bits.awprot      := u_arbiter.io.mem_axi.aw.bits.prot
    u_arbiter.io.mem_axi.aw.ready    := xbar.io.xbar.aw.ready

    xbar.io.xbar.w.valid             := u_arbiter.io.mem_axi.w.valid
    xbar.io.xbar.w.bits.wdata        := u_arbiter.io.mem_axi.w.bits.data
    xbar.io.xbar.w.bits.wstrb        := u_arbiter.io.mem_axi.w.bits.strb
    xbar.io.xbar.w.bits.wlast        := u_arbiter.io.mem_axi.w.bits.last
    u_arbiter.io.mem_axi.w.ready     := xbar.io.xbar.w.ready

    u_arbiter.io.mem_axi.b.valid     := xbar.io.xbar.b.valid
    u_arbiter.io.mem_axi.b.bits.resp := xbar.io.xbar.b.bits.bresp
    u_arbiter.io.mem_axi.b.bits.id   := xbar.io.xbar.b.bits.bid
    xbar.io.xbar.b.ready             := u_arbiter.io.mem_axi.b.ready

    xbar.io.xbar.ar.valid            := u_arbiter.io.mem_axi.ar.valid
    xbar.io.xbar.ar.bits.araddr      := u_arbiter.io.mem_axi.ar.bits.addr
    xbar.io.xbar.ar.bits.arid        := u_arbiter.io.mem_axi.ar.bits.id
    xbar.io.xbar.ar.bits.arlen       := u_arbiter.io.mem_axi.ar.bits.len
    xbar.io.xbar.ar.bits.arsize      := u_arbiter.io.mem_axi.ar.bits.size
    xbar.io.xbar.ar.bits.arburst     := u_arbiter.io.mem_axi.ar.bits.burst
    xbar.io.xbar.ar.bits.arlock      := u_arbiter.io.mem_axi.ar.bits.lock
    xbar.io.xbar.ar.bits.arcache     := u_arbiter.io.mem_axi.ar.bits.cache
    xbar.io.xbar.ar.bits.arprot      := u_arbiter.io.mem_axi.ar.bits.prot
    u_arbiter.io.mem_axi.ar.ready    := xbar.io.xbar.ar.ready

    u_arbiter.io.mem_axi.r.valid     := xbar.io.xbar.r.valid
    u_arbiter.io.mem_axi.r.bits.data := xbar.io.xbar.r.bits.rdata
    u_arbiter.io.mem_axi.r.bits.resp := xbar.io.xbar.r.bits.rresp
    u_arbiter.io.mem_axi.r.bits.last := xbar.io.xbar.r.bits.rlast
    u_arbiter.io.mem_axi.r.bits.id   := xbar.io.xbar.r.bits.rid
    xbar.io.xbar.r.ready             := u_arbiter.io.mem_axi.r.ready


  }  else {

    val imem = Module(new MemHelper)
    imem.io.clock          := clock
    imem.io.reset          := reset
    imem.io.req.bits       := core.io.imem.req.bits
    imem.io.req.valid      := core.io.imem.req.valid
    core.io.imem.req.ready := imem.io.req.ready

    core.io.imem.resp.bits  := imem.io.resp.bits
    core.io.imem.resp.valid := imem.io.resp.valid
    imem.io.resp.ready      := true.B

    val dmem = Module(new MemHelper)
    dmem.io.clock := clock
    dmem.io.reset := reset
    dmem.io.req <> core.io.dmem.req

    core.io.dmem.resp.bits  := dmem.io.resp.bits
    core.io.dmem.resp.valid := dmem.io.resp.valid
    dmem.io.resp.ready      := true.B

    io.master <> DontCare

  }
}
