package cl3

import chisel3._
import chisel3.util._


class BTBEntry extends Bundle {
  val target = UInt(32.W)
  val isCall = Bool()
  val isRet  = Bool()
  val isJmp  = Bool()
}

class CL3BTB(width: Int) extends Module {
    val io = IO(new Bundle {

        val rdTag0 = Input(UInt((30 - width).W))
        val rdIdx0 = Input(UInt(width.W))
        val rdata0 = Output(new BTBEntry)
        val hit0   = Output(Bool())

        val rdTag1 = Input(UInt((30 - width).W))
        val rdIdx1 = Input(UInt(width.W))
        val rdata1 = Output(new BTBEntry)
        val hit1   = Output(Bool())

        val waddr  = Input(UInt(32.W))
        val wdata  = Input(new BTBEntry)
        val wen    = Input(Bool())

    })

    dontTouch(io)

    val tagTable  = RegInit(VecInit(Seq.fill(1 << width)(0.U((30 - width).W))))
    val dataTable = RegInit(VecInit(Seq.fill(1 << width)(0.U.asTypeOf(new BTBEntry))))

    val rdata0 = dataTable(io.rdIdx0)
    val rdata1 = dataTable(io.rdIdx1)

    val tag0 = tagTable(io.rdIdx0)
    val tag1 = tagTable(io.rdIdx1)

    io.hit0 := tag0 === io.rdTag0
    io.hit1 := tag1 === io.rdTag1

    io.rdata0 := rdata0
    io.rdata1 := rdata1

    val wTag = io.waddr(31, width + 2)
    val wIdx = io.waddr(width + 1, 2)

    when(io.wen) {
        dataTable(wIdx) := io.wdata
        tagTable(wIdx)  := wTag
    }
}


