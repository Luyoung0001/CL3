package cl3

import chisel3._
import chisel3.util._
import cl3.SramAddr._

//-----------------------------------------------------------------
// Single Port RAM 0KB
// Mode: Read First
//-----------------------------------------------------------------
class ICacheTagRamDev(p: ICacheParams) extends Module {
  val io = IO(new DualPortICTagRamIO(p))

  // Synchronous write
  val tagRamDataBits = p.tagRamDataBits
  val portNum        = 2
  val memBanks       = Seq.fill(p.banks) {
    SyncReadMem(1 << (p.tagRamIdxBits - 2), UInt(tagRamDataBits.W))
  }

  val port = Wire(Vec(portNum, new ICacheTagRamIO(p)))
  port(0) <> io.p0
  port(1) <> io.p1
  for (i <- 0 until portNum) {
    val bank     = bankSel(port(i).addr)
    val bank_reg = RegNext(bank)

    val row   = rowAddr(port(i).addr)
    val wen   = port(i).wr
    val wdata = port(i).din
    val rdata = WireDefault(0.U(tagRamDataBits.W))
    for (j <- 0 until p.banks) {
      when(j.U === bank) {
        when(wen) {
          memBanks(j).write(row, wdata)
        }
      }
    }
    val doutVec = Wire(Vec(memBanks.length, UInt(tagRamDataBits.W)))
    for (j <- 0 until memBanks.length) {
      doutVec(j) := memBanks(j).read(row, true.B)
    }
    rdata := doutVec(bank_reg)
    port(i).dout := rdata
  }
}

class ICacheValidRam(p: ICacheParams) extends Module {
  val io = IO(new DualPortICValidRamIO(p))

  val mem = RegInit(VecInit(Seq.fill(p.numLines)(false.B)))

  when (io.flush) {
    for (i <- 0 until p.numLines) {
      mem(i) := false.B
    }
  }
  
  io.p0.dout := RegNext(Mux(io.p0.wr, io.p0.din, mem(io.p0.addr)))
  io.p1.dout := RegNext(Mux(io.p1.wr, io.p1.din, mem(io.p1.addr)))

  when (io.p0.wr) { mem(io.p0.addr) := io.p0.din }
  when (io.p1.wr) { mem(io.p1.addr) := io.p1.din }
  
  when (io.p0.wr && io.p1.wr) {
    assert(io.p0.addr =/= io.p1.addr, "ValidRam: dual write same address")
  }
}


class ICacheTagRam(p: ICacheParams) extends Module {
  val io = IO(new DualPortICTagRamIO(p))

  if (p.useMacro) {
    val m = Module(new ICacheTagRamMacro(p))
    m.io <> io
  } else {
    val m = Module(new ICacheTagRamDev(p))
    m.io <> io
  }
}
