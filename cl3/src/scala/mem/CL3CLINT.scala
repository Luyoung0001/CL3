package cl3

import chisel3._
import chisel3.util._

class CL3CLINT extends Module {
  val io = IO(new Bundle { 
    val axi   = Flipped(new SimpleAXI4MasterBundle(ADDR_WIDTH = 32, DATA_WIDTH = 32, ID_WIDTH = 4))
    val irq = Output(Bool())
  })
  val mtimel = RegInit(0.U(32.W))
  val mtimeh = RegInit(0.U(32.W))

  val nextLo = mtimel + 1.U
  mtimel := nextLo
  mtimeh := Mux(nextLo === 0.U, mtimeh + 1.U, mtimeh)

  val mtimecmpl = RegInit(0.U(32.W))
  val mtimecmph = RegInit(0.U(32.W))

  val mtime     = Cat(mtimeh, mtimel)
  val mtimecmp  = Cat(mtimecmph, mtimecmpl)

  io.irq := (mtime > mtimecmp) && (mtimecmp =/= 0.U)
  

  val timerh: UInt = "hbffC".U(16.W)
  val timerl: UInt = "hbff8".U(16.W)

  val timercmph: UInt = "h4004".U(16.W)
  val timercmpl: UInt = "h4000".U(16.W)

  val readDataReg = RegInit(0.U(32.W))
  val readRespReg = RegInit(0.U(2.W))
  val readIdReg   = RegInit(0.U(4.W))
  val rvalidReg   = RegInit(false.B)

  io.axi.ar.ready       := !rvalidReg
  io.axi.r.valid        := rvalidReg
  io.axi.r.bits.rdata   := readDataReg
  io.axi.r.bits.rresp   := readRespReg
  io.axi.r.bits.rlast   := true.B
  io.axi.r.bits.rid     := readIdReg

  when(io.axi.ar.fire) {
    val dataSel = WireDefault(0.U(32.W))
    val addr    = io.axi.ar.bits.araddr
    when(addr(15, 0) === timerl) {
        dataSel := mtimel
    }.elsewhen(addr(15, 0) === timerh) {
        dataSel := mtimeh
    }
    readDataReg := dataSel
    readRespReg := 0.U
    readIdReg   := io.axi.ar.bits.arid
    rvalidReg   := true.B
  }

  when(io.axi.r.fire) {
    rvalidReg := false.B
  }

  val awPending   = RegInit(false.B)
  val wPending    = RegInit(false.B)
  val storedAddr  = RegInit(0.U(32.W))
  val storedId    = RegInit(0.U(4.W))
  val storedWdata = RegInit(0.U(32.W))
  val bvalidReg   = RegInit(false.B)
  val brespReg    = RegInit(0.U(2.W))
  val bidReg      = RegInit(0.U(4.W))

  io.axi.aw.ready      := !awPending && !bvalidReg
  io.axi.w.ready       := !wPending && !bvalidReg
  io.axi.b.valid       := bvalidReg
  io.axi.b.bits.bresp  := brespReg
  io.axi.b.bits.bid    := bidReg

  when(io.axi.aw.fire) {
    awPending  := true.B
    storedAddr := io.axi.aw.bits.awaddr
    storedId   := io.axi.aw.bits.awid
  }

  when(io.axi.w.fire) {
    wPending    := true.B
    storedWdata := io.axi.w.bits.wdata
  }

  val canWrite = awPending && wPending && !bvalidReg
  when(canWrite) {
    when(storedAddr(15, 0) === timercmph) {
        mtimecmph := storedWdata
    }.elsewhen(storedAddr(15, 0) === timercmpl) {
        mtimecmpl := storedWdata
    }
    bvalidReg := true.B
    brespReg  := 0.U
    bidReg    := storedId
    awPending := false.B
    wPending  := false.B
  }

  when(io.axi.b.fire) {
    bvalidReg := false.B
  }
}
