package cl3

import chisel3._
import chisel3.util._
import chisel3.util.random.GaloisLFSR

class BTBEntry extends Bundle {
  val target = UInt(30.W)
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
    val data   = new BTBEntry
  }

  val btb_bank  = Seq.fill(2)(RegInit(VecInit(Seq.fill(num / 2)(0.U.asTypeOf(new BTBPhysicalEntry)))))

  val rd_mux_bit   = io.rd(0).pc(0)

  val rd_tag  = Seq.fill(2)(Wire(UInt(29.W)))
  val rd_hit  = Seq.fill(2)(Wire(Bool()))
  val rdata   = Seq.fill(2)(Wire(new BTBEntry))
  rd_tag(0) := Mux(rd_mux_bit, io.rd(1).pc(29, 1), io.rd(0).pc(29, 1))
  rd_tag(1) := Mux(rd_mux_bit, io.rd(0).pc(29, 1), io.rd(1).pc(29, 1))

  // val entries = RegInit(VecInit(Seq.fill(num)(0.U.asTypeOf(new BTBPhysicalEntry))))

  val lfsr       = GaloisLFSR.maxPeriod(log2Ceil(num) - 1)
  val replaceIdx = lfsr

  for(i <- 0 until 2) {
    val hits = VecInit(btb_bank(i).map { e => e.valid && e.tag === rd_tag(i)})
    rd_hit(i) := hits.asUInt.orR
    rdata(i)  := Mux1H(hits, btb_bank(i).map(_.data))
  }

  io.rd(0).hit := Mux(rd_mux_bit, rd_hit(1), rd_hit(0))
  io.rd(1).hit := Mux(rd_mux_bit, rd_hit(0), rd_hit(1))

  io.rd(0).rdata := Mux(rd_mux_bit, rdata(1), rdata(0)) 
  io.rd(1).rdata := Mux(rd_mux_bit, rdata(0), rdata(1))

  // for (i <- 0 until 2) {
  //   val reqTag = io.rd(i).pc
  //   val hits   = VecInit(entries.map { e =>
  //     e.valid && e.tag === reqTag
  //   })
  //   io.rd(i).hit   := hits.asUInt.orR
  //   io.rd(i).rdata := Mux1H(hits, entries.map(_.data))
  // }

  // val wrTag = io.wr.pc

  val wen = Seq.fill(2)(Wire(Bool()))
  wen(0) := io.wr.wen && !io.wr.pc(0)
  wen(1) := io.wr.wen &&  io.wr.pc(0)

  for( i <- 0 until 2) {
    val hits = VecInit(btb_bank(i).map { e => e.valid && e.tag === io.wr.pc(29, 1)})

    val hitIdx = OHToUInt(hits)

    when(wen(i) && hits.asUInt.orR) {
      btb_bank(i)(hitIdx).data := io.wr.wdata
    }.elsewhen(wen(i) && !hits.asUInt.orR) {
      btb_bank(i)(replaceIdx).valid := true.B
      btb_bank(i)(replaceIdx).tag   := io.wr.pc(29, 1)
      btb_bank(i)(replaceIdx).data  := io.wr.wdata
    }
  }


  // val writeHits = VecInit(entries.map { e =>
  //   e.valid && e.tag === wrTag
  // })
  // val exists    = writeHits.asUInt.orR
  // val hitIdx    = OHToUInt(writeHits)

  // when(io.wr.wen && exists) {
  //   entries(hitIdx).data := io.wr.wdata
  // }.elsewhen(io.wr.wen && !exists) {
  //   entries(replaceIdx).valid  := true.B
  //   entries(replaceIdx).tag    := wrTag
  //   entries(replaceIdx).data   := io.wr.wdata
  // }

}
