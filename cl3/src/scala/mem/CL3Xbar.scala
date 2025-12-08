package cl3

import chisel3._
import chisel3.util._

class CL3Xbar extends Module {
  val io = IO(new Bundle {
    val xbar  = Flipped(new SimpleAXI4MasterBundle(ADDR_WIDTH = 32, DATA_WIDTH = 32, ID_WIDTH = 4))
    val clint = new SimpleAXI4MasterBundle(ADDR_WIDTH = 32, DATA_WIDTH = 32, ID_WIDTH = 4)
    val top   = new SimpleAXI4MasterBundle(ADDR_WIDTH = 32, DATA_WIDTH = 32, ID_WIDTH = 4)
  })

  val CLINT_ADDR_PREFIX = "h02".U(8.W)

  // Default downstream payloads mirror upstream requests
  io.clint.ar.bits := io.xbar.ar.bits
  io.top.ar.bits   := io.xbar.ar.bits
  io.clint.aw.bits := io.xbar.aw.bits
  io.top.aw.bits   := io.xbar.aw.bits
  io.clint.w.bits  := io.xbar.w.bits
  io.top.w.bits    := io.xbar.w.bits

  // -------------------- READ CHANNEL --------------------
  val arToClint    = io.xbar.ar.bits.araddr(31, 24) === CLINT_ADDR_PREFIX
  val readActive   = RegInit(false.B)
  val readSelClint = RegInit(false.B)

  io.clint.ar.valid := io.xbar.ar.valid && arToClint && !readActive
  io.top.ar.valid   := io.xbar.ar.valid && !arToClint && !readActive
  io.xbar.ar.ready  := Mux(arToClint, io.clint.ar.ready, io.top.ar.ready) && !readActive

  when(io.xbar.ar.fire) {
    readActive   := true.B
    readSelClint := arToClint
  }

  when(io.xbar.r.fire && io.xbar.r.bits.rlast) {
    readActive := false.B
  }

  io.xbar.r.valid  := Mux(readSelClint, io.clint.r.valid, io.top.r.valid)
  io.xbar.r.bits   := Mux(readSelClint, io.clint.r.bits, io.top.r.bits)
  io.clint.r.ready := io.xbar.r.ready && readSelClint
  io.top.r.ready   := io.xbar.r.ready && !readSelClint

  // -------------------- WRITE CHANNEL --------------------
  val awToClint        = io.xbar.aw.bits.awaddr(31, 24) === CLINT_ADDR_PREFIX
  val writeActiveReg   = RegInit(false.B)
  val writeSelClintReg = RegInit(false.B)

  io.clint.aw.valid := io.xbar.aw.valid && awToClint && !writeActiveReg
  io.top.aw.valid   := io.xbar.aw.valid && !awToClint && !writeActiveReg
  io.xbar.aw.ready  := Mux(awToClint, io.clint.aw.ready, io.top.aw.ready) && !writeActiveReg

  when(io.xbar.aw.fire) {
    writeActiveReg   := true.B
    writeSelClintReg := awToClint
  }

  val haveWriteSel     = writeActiveReg || io.xbar.aw.fire
  val curWriteSelClint = Mux(writeActiveReg, writeSelClintReg, awToClint)

  io.clint.w.valid := io.xbar.w.valid && haveWriteSel && curWriteSelClint
  io.top.w.valid   := io.xbar.w.valid && haveWriteSel && !curWriteSelClint
  io.xbar.w.ready  := Mux(curWriteSelClint, io.clint.w.ready, io.top.w.ready) && haveWriteSel

  io.xbar.b.valid  := Mux(writeSelClintReg, io.clint.b.valid, io.top.b.valid)
  io.xbar.b.bits   := Mux(writeSelClintReg, io.clint.b.bits, io.top.b.bits)
  io.clint.b.ready := io.xbar.b.ready && writeSelClintReg
  io.top.b.ready   := io.xbar.b.ready && !writeSelClintReg

  when(io.xbar.b.fire) {
    writeActiveReg := false.B
  }
}

class SimpleReqArbiter extends Module {
  val io = IO(new Bundle {
    val ifuReq = Flipped(Decoupled(Input(new SimpleMemReq(32))))
    val lsuReq = Flipped(Decoupled(Input(new SimpleMemReq(32))))
    val outReq = Decoupled(new SimpleMemReq(32))
    val isIFU  = Output(Bool())
  })

  val sIdle :: sIfu :: sLsu :: Nil = Enum(3)
  val state                        = RegInit(sIdle)

  val stateIdle: Bool = state === sIdle
  val stateIFU:  Bool = state === sIfu
  val stateLSU:  Bool = state === sLsu

  val chooseIFU = io.ifuReq.valid && !io.lsuReq.valid
  val chooseLSU = io.lsuReq.valid

  when(stateIdle) {
    when(chooseIFU && !io.outReq.fire) {
      state := sIfu
    }.elsewhen(chooseLSU && !io.outReq.fire) {
      state := sLsu
    }
  }

  when(stateIFU) {
    when(io.outReq.fire) {
      state := sIdle
    }
  }

  when(stateLSU) {
    when(io.outReq.fire) {
      state := sIdle
    }
  }

  io.outReq.bits := Mux1H(
    Seq(
      stateIdle -> Mux(chooseIFU, io.ifuReq.bits, io.lsuReq.bits),
      stateIFU  -> io.ifuReq.bits,
      stateLSU  -> io.lsuReq.bits
    )
  )

  io.outReq.valid := Mux1H(
    Seq(
      stateIdle -> (chooseIFU || chooseLSU),
      stateIFU  -> true.B,
      stateLSU  -> true.B
    )
  )

  io.ifuReq.ready := Mux1H(
    Seq(
      stateIdle -> Mux(chooseIFU, io.outReq.ready, false.B),
      stateIFU  -> io.outReq.ready,
      stateLSU  -> false.B
    )
  )

  io.lsuReq.ready := Mux1H(
    Seq(
      stateIdle -> Mux(chooseLSU, io.outReq.ready, false.B),
      stateIFU  -> false.B,
      stateLSU  -> io.outReq.ready
    )
  )

  io.isIFU := Mux1H(
    Seq(
      stateIdle -> Mux(chooseIFU, true.B, false.B),
      stateIFU  -> true.B,
      stateLSU  -> false.B
    )
  )

}

class SimpleRespArbiter extends Module {
  val io = IO(new Bundle {
    val in    = Flipped(Decoupled(Input(new SimpleMemResp(32))))
    val ifu   = Decoupled(Output(new SimpleMemResp(32)))
    val lsu   = Decoupled(Output(new SimpleMemResp(32)))
    val isIFU = Input(Bool())
  })

  val chooseIFU = io.in.valid && io.isIFU
  val chooseLSU = io.in.valid && !io.isIFU

  io.ifu.valid := Mux(chooseIFU, io.in.valid, false.B)
  io.ifu.bits  := Mux(chooseIFU, io.in.bits, 0.U.asTypeOf(new SimpleMemResp(32)))

  io.lsu.valid := Mux(chooseLSU, io.in.valid, false.B)
  io.lsu.bits  := Mux(chooseLSU, io.in.bits, 0.U.asTypeOf(new SimpleMemResp(32)))

  io.in.ready := Mux(chooseIFU, io.ifu.ready, Mux(chooseLSU, io.lsu.ready, false.B))
}
