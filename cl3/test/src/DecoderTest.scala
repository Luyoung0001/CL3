package cl3

import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec

class DecoderTest extends AnyFreeSpec {
  "docoder test" in {
    simulate(new CL3Decoder()) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()

      // dut.io.inst.poke("h00000413".U(32.W))
      // dut.io.out.illegal.expect(false.B)
      dut.io.inst.poke("h00000000".U(32.W))
      dut.io.out.illegal.expect(true.B)
    }
  }
}