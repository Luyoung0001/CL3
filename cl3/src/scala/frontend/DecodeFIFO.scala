package cl3

import chisel3._
import chisel3.util._

trait DecodeFIFOConfig {
  val FIFODepth: Int = 8
}

class DecodeFIFO extends Module with DecodeFIFOConfig {
  val io = IO(new Bundle {
    val flush = Input(Bool())
    val in    = Flipped(Decoupled(Vec(2, Input(new DEInfo))))
    val out   = Vec(2, Decoupled(Output(new DEInfo)))

    val debug = new Bundle {
      val next_ptr = Output(UInt(log2Ceil(FIFODepth).W))
    }
  })

  dontTouch(io.debug)

  class FIFOEntry extends Bundle {
    val info  = new DEInfo
    val valid = Bool()
  }

  val entrys    = RegInit(VecInit(Seq.fill(FIFODepth)(0.U.asTypeOf(new FIFOEntry))))
  val rd_ptr_q  = RegInit(0.U(log2Ceil(FIFODepth).W))
  val wr_ptr_q  = RegInit(0.U(log2Ceil(FIFODepth).W))
  val count_q   = RegInit(0.U((log2Ceil(FIFODepth) + 1).W))

  io.in.ready := ((FIFODepth.U - count_q) >= 2.U)

  val head = entrys(rd_ptr_q)

  val next_ptr = rd_ptr_q +% 1.U

  io.debug.next_ptr := next_ptr

  io.out(0).valid := head.valid
  io.out(1).valid := entrys(next_ptr).valid

  val push = io.in.fire
  val pop0 = io.out(0).fire
  val pop1 = io.out(1).fire

  val push_num = Mux(push, Mux(io.in.bits(0).pred, 1.U, 2.U), 0.U)
  val pop_num  = Mux(pop1, 2.U, Mux(pop0, 1.U, 0.U))

  when(io.flush) {
    for (i <- 0 until FIFODepth) {
      entrys(i).valid := false.B
    }
  }.elsewhen(push) {
    entrys(wr_ptr_q).valid := true.B
  }

  when(push && !io.flush) {
    entrys(wr_ptr_q).info := io.in.bits(0)
    entrys(wr_ptr_q +% 1.U).info := io.in.bits(1) //TODO:
    entrys(wr_ptr_q +% 1.U).valid := !io.in.bits(0).pred //TODO:
  }


  when(io.flush) {
    wr_ptr_q := 0.U
  }.elsewhen(push) {
    wr_ptr_q := wr_ptr_q +% push_num
  }

  when(io.flush) {
    rd_ptr_q := 0.U
  }.elsewhen(pop0 && !pop1) {
    entrys(rd_ptr_q).valid := false.B
    rd_ptr_q := next_ptr
  }.elsewhen(pop1) {
    entrys(rd_ptr_q).valid := false.B
    entrys(next_ptr).valid := false.B
    rd_ptr_q := next_ptr +% 1.U
  }

  when(io.flush) {
    count_q := 0.U
  }.otherwise {
    count_q := count_q + push_num - pop_num
  }

  io.out(0).bits := entrys(rd_ptr_q).info
  io.out(1).bits := entrys(next_ptr).info

}
