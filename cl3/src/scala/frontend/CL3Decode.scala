package cl3

import chisel3._
import chisel3.util._

class DEIO extends Bundle {
  val in  = Flipped(Decoupled(Input(Vec(2, new FEInfo))))
  val out = Decoupled(Output(Vec(2, (new DEInfo))))
}

class CL3Decode extends Module {

  val io = IO(new DEIO)

  val decoder = Seq.fill(2)(Module(new CL3Decoder))

  for (i <- 0 until 2) {
    decoder(i).io.inst        := io.in.bits(i).inst
    decoder(i).io.pc          := io.in.bits(i).pc
    decoder(i).io.pred        := io.in.bits(i).pred
    decoder(i).io.fault_fetch := io.in.bits(i).fault_fetch
    decoder(i).io.fault_page  := io.in.bits(i).fault_page

    io.out.bits(i) := decoder(i).io.out
  }

  io.in.ready  := io.out.ready
  io.out.valid := io.in.valid

}
