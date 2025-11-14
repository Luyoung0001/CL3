package cl3

import chisel3._
import chisel3.util._
import chisel3.experimental._
import cl3.SramAddr._

// SRAM
class SramTagRAM_Generic(val addrBits: Int, val dataBits: Int)
    extends ExtModule with HasExtModuleResource {

    val CLK = IO(Input(Clock()))
    val CEN = IO(Input(Bool()))
    val WEN = IO(Input(Bool()))
    val A   = IO(Input(UInt(addrBits.W)))
    val D   = IO(Input(UInt(dataBits.W)))
    val Q   = IO(Output(UInt(dataBits.W)))

    addResource("/vsrc/S55NLLG1PH_X128Y2D20.v")
}

class SramDataRam_Generic(val addrBits: Int, val dataBits: Int)
    extends ExtModule with HasExtModuleResource {

    val CLK = IO(Input(Clock()))
    val CEN = IO(Input(Bool()))
    val WEN = IO(Input(Bool()))
    val A   = IO(Input(UInt(addrBits.W)))
    val D   = IO(Input(UInt(dataBits.W)))
    val Q   = IO(Output(UInt(dataBits.W)))

    addResource("/vsrc/S55NLLG1PH_X128Y2D64.v")
}

class ICacheTagRamMacro(p: ICacheParams) extends Module {
    val io = IO(new ICacheDataRamIO(p))
    val sram = Module(new SramTagRAM_Generic(p.tagRamIdxBits, p.tagRamDataBits))
    sram.CLK := clock
    sram.CEN := false.B
    sram.WEN := !io.wr
    sram.A   := io.addr
    sram.D   := io.din

    io.dout  := RegNext(sram.Q)
}

class ICacheDataRamMacro(p: ICacheParams) extends Module {
    val io = IO(new ICacheTagRamIO(p))
    val sramBanks = Seq.fill(p.banks) (
        Module(new SramDataRam_Generic(p.dataRamIdxBits, p.dataRamDataBits))
    )

    val bank = bankSel(io.addr)
    val row  = rowAddr(io.addr)
    val wen  = io.wr
    val wdata= io.din
    val rdata = RegInit(0.U(p.tagRamDataBits.W))
    for(i <- 0 until p.banks){
        sramBanks(i).CLK := clock
        sramBanks(i).CEN := false.B
        sramBanks(i).WEN := true.B
        sramBanks(i).A   := row
        sramBanks(i).D   := wdata
        when(i.U === bank){
            sramBanks(i).WEN := !wen
            rdata  := sramBanks(i).Q
        }
    }
    io.dout := rdata

}