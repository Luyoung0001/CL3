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
    val portNum = 2
    val memBanks = Seq.fill(p.banks) { 
      SyncReadMem(1 << (p.tagRamIdxBits - 2), UInt(tagRamDataBits.W)) 
    }
    
    val port = Wire(Vec(portNum, new ICacheTagRamIO(p)))
    port(0) <> io.p0
    port(1) <> io.p1
    for (i <- 0 until portNum) {
        val bank = bankSel(port(i).addr)
        val bank_reg = RegNext(bank)

        val row = rowAddr(port(i).addr)
        val wen = port(i).wr
        val wdata = port(i).din
        val rdata = WireDefault(0.U(tagRamDataBits.W))
        for (j <- 0 until p.banks) {
            when(j.U === bank){
                when (wen) {
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
    // val mem   = SyncReadMem(1 << p.tagRamIdxBits, UInt(p.tagRamDataBits.W))
    // val rdata = mem.read(io.addr, true.B)
    // val r_q   = rdata

    // when (io.wr) {
    //     mem.write(io.addr, io.din)
    // }
    // io.dout := r_q
}

class ICacheTagRam(p: ICacheParams)
    extends Module {
  val io = IO(new DualPortICTagRamIO(p))

  val m = Module(new ICacheTagRamDev(p))
  m.io <> io
}

// class ICacheTagRam(p: ICacheParams)
//     extends Module {
//   val io = IO(new ICacheTagRamIO(p))

//   if (p.useMacro) {
//     val m = Module(new ICacheTagRamMacro(p))
//     m.io <> io
//   } else {
//     val m = Module(new ICacheTagRamDev(p))
//     m.io <> io
//   }
// }