package cl3

import chisel3._
import chisel3.util._

class CoreIO extends Bundle {
  val imem      = new CL3ICacheIO
  val dmem      = new CL3DCacheIO
  val ext_irq   = Input(Bool())
  val timer_irq = Input(Bool())

}

class CL3Core extends Module with CL3Config {

  val io = IO(new CoreIO)

  val frontend = Module(new CL3Frontend)

  val mmu = Module(new CL3MMU)
  mmu.io.fetchIn <> frontend.io.mem
  mmu.io.fetchOut <> io.imem
  mmu.io.lsuOut <> io.dmem

  val issue = Module(new CL3Issue)
  issue.io.in.fetch <> frontend.io.out
  frontend.io.bp := issue.io.out.bp
  frontend.io.br := issue.io.out.br

  val lsu = Module(new CL3LSU)
  lsu.io.in.mem   := mmu.io.lsuIn.resp
  mmu.io.lsuIn.req <> lsu.io.out.mem
  mmu.io.lsuIn.resp <> lsu.io.in.mem
  lsu.io.in.info <> issue.io.out.op(2)
  issue.io.in.lsu := lsu.io.out.info
  lsu.io.in.flush := issue.io.out.lsu_flush

  val csr = Module(new CL3CSR)
  csr.io.in.info     := issue.io.out.op(5)
  csr.io.in.wb       := issue.io.out.csr
  csr.io.in.baddr    := BOOT_ADDR
  issue.io.in.csr    := csr.io.out.info
  csr.io.in.hold     := issue.io.out.hold
  mmu.io.ctrl        := DontCare //TODO:

  csr.io.in.irq_e     := io.ext_irq // TODO:
  csr.io.in.irq_t   := io.timer_irq
  csr.io.in.irq_inhibit := false.B
  issue.io.in.irq       := csr.io.out.irq

  // for difftest
  issue.io.in.tvec := csr.io.out.tvec
  issue.io.in.epc  := csr.io.out.epc

  val mul = Module(new CL3MUL)
  mul.io.in.hold         := issue.io.out.hold
  mul.io.in.info         := issue.io.out.op(3)
  issue.io.in.mul.result := mul.io.out.result

  val div = Module(new CL3DIV)
  div.io.in.info  := issue.io.out.op(4)
  issue.io.in.div := div.io.out.wb

  val exec0 = Module(new CL3EXU)
  exec0.io.in.hold    := issue.io.out.hold
  exec0.io.in.info    := issue.io.out.op(0)
  issue.io.in.exec(0) := exec0.io.out.info

  val exec1 = Module(new CL3EXU)
  exec1.io.in.hold    := issue.io.out.hold
  exec1.io.in.info    := issue.io.out.op(1)
  issue.io.in.exec(1) := exec1.io.out.info

  val exec2 = Module(new CL3EXU)
  exec2.io.in.hold    := issue.io.out.hold // TODO
  exec2.io.in.info    := issue.io.out.op(6)
  issue.io.in.exec(2) := exec2.io.out.info

  val exec3 = Module(new CL3EXU)
  exec3.io.in.hold    := issue.io.out.hold // TODO
  exec3.io.in.info    := issue.io.out.op(7)
  issue.io.in.exec(3) := exec3.io.out.info

}
