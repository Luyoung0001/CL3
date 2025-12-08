package cl3

import chisel3._
import chisel3.util._
import chisel3.util.random.GaloisLFSR

class BTBEntry extends Bundle {
  val target = UInt(32.W)
  val isCall = Bool()
  val isRet  = Bool()
  val isJmp  = Bool()
}

class BTBRdPort extends Bundle {
  val pc    = Input(UInt(30.W)) // PC[31:2]
  val hit   = Output(Bool())
  val rdata = Output(new BTBEntry)
}

class BTBWrPort extends Bundle {
  val pc    = Input(UInt(30.W)) // PC[31:2]
  val wdata = Input(new BTBEntry)
  val wen   = Input(Bool())
}

/* Fully Associative BTB */
class CL3BTB(num: Int) extends Module {
  val io = IO(new Bundle {
    val rd = Vec(2, new BTBRdPort)
    val wr = new BTBWrPort
  })

  class BTBPhysicalEntry extends Bundle {
    val valid  = Bool()
    val tag    = UInt(29.W)
    val offset = Bool()
    val data   = new BTBEntry
  }

  val entries = RegInit(VecInit(Seq.fill(num)(0.U.asTypeOf(new BTBPhysicalEntry))))

  val lfsr       = GaloisLFSR.maxPeriod(log2Ceil(num))
  val replaceIdx = lfsr

  for (i <- 0 until 2) {
    val reqTag = io.rd(i).pc(29, 1)
    val reqOff = io.rd(i).pc(0)
    val hits   = VecInit(entries.map { e =>
      e.valid && (e.tag === reqTag) && (e.offset === reqOff)
    })
    io.rd(i).hit   := hits.asUInt.orR
    io.rd(i).rdata := Mux1H(hits, entries.map(_.data))
  }

  val wrTag = io.wr.pc(29, 1)
  val wrOff = io.wr.pc(0)

  val writeHits = VecInit(entries.map { e =>
    e.valid && (e.tag === wrTag) && (e.offset === wrOff)
  })
  val exists    = writeHits.asUInt.orR
  val hitIdx    = OHToUInt(writeHits)

  when(io.wr.wen && exists) {
    entries(hitIdx).data := io.wr.wdata
  }.elsewhen(io.wr.wen && !exists) {
    entries(replaceIdx).valid  := true.B
    entries(replaceIdx).tag    := wrTag
    entries(replaceIdx).offset := wrOff
    entries(replaceIdx).data   := io.wr.wdata
  }

}
