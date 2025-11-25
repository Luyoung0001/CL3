package cl3

import chisel3._
import chisel3.util._
import cl3.SramAddr._

//-----------------------------------------------------------------
// Single Port RAM 8KB
// Mode: Read First
//-----------------------------------------------------------------
class ICacheDataRamDev(p: ICacheParams) extends Module {
    val io = IO(new DualPortICDataRamIO(p))
    
    val dataBits     = p.dataRamDataBits
    // val depthPerBank = 1 << (p.dataRamIdxBits - p.bankSelBits)
    // Synchronous write
    val memBanks = Seq.fill(p.banks) { 
        SyncReadMem(1 << p.dataRamIdxBits, UInt(dataBits.W)) 
    }
    val portNum = 2
    // val port = VecInit(Seq(io.p0, io.p1))
    val port = Wire(Vec(portNum, new ICacheDataRamIO(p)))
    port(0) <> io.p0
    port(1) <> io.p1
    for (i <- 0 until portNum) {
        val bank = bankSel(port(i).addr)
        val bank_reg = RegNext(bank)

        val row = rowAddr(port(i).addr)
        val wen = port(i).wr
        val wdata = port(i).din
        val rdata = WireDefault(0.U(dataBits.W))
        for (j <- 0 until p.banks) {
            when(j.U === bank){
                when (wen) {
                    memBanks(j).write(row, wdata)
                }
            }
        }
        val doutVec = Wire(Vec(memBanks.length, UInt(dataBits.W)))
        for (j <- 0 until memBanks.length) {
          doutVec(j) := memBanks(j).read(row, true.B)
        }
        rdata := doutVec(bank_reg)
        port(i).dout := rdata
    }
    // val r_q   = rdata
    // io.dout := memBanks(bank).read(row, true.B)
}

// class ICacheDataRam(p: ICacheParams)
//     extends Module {
//   val io = IO(new DualPortICDataRamIO(p))


//   val m = Module(new ICacheDataRamDev(p))
//   m.io <> io
// }

class ICacheDataRam(p: ICacheParams)
    extends Module {
  val io = IO(new DualPortICDataRamIO(p))

  if (p.useMacro) {
    val m = Module(new ICacheDataRamMacro(p))
    m.io <> io
  } else {
    val m = Module(new ICacheDataRamDev(p))
    m.io <> io
  }
}
