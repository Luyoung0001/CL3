package cl3

import chisel3._
import chisel3.util._
import chisel3.experimental._
import cl3.SramAddr._

// SRAM
class S55NLLG1PH_X64Y1D20(val addrBits: Int, val dataBits: Int)
    extends ExtModule with HasExtModuleResource {

    val CLK = IO(Input(Clock()))
    val CEN = IO(Input(Bool()))
    val WEN = IO(Input(Bool()))
    val A   = IO(Input(UInt(addrBits.W)))
    val D   = IO(Input(UInt(dataBits.W)))
    val Q   = IO(Output(UInt(dataBits.W)))

    addResource("/vsrc/S55NLLG1PH_X64Y1D20.v")
}

class S55NLLG1PH_X128Y2D64(val addrBits: Int, val dataBits: Int)
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
    val io = IO(new DualPortICTagRamIO(p))

    val tagRamDataBits = p.tagRamDataBits
    val portNum = 2
    val banks = p.banks
    val memBanks = Seq.fill(banks)(Module(new S55NLLG1PH_X64Y1D20(
        addrBits  = p.tagRamIdxBits - 2,
        dataBits  = p.tagRamDataBits
    )))
    val port = Wire(Vec(portNum, new ICacheTagRamIO(p)))
    port(0) <> io.p0

    port(1) <> io.p1

    val bank_p0 = bankSel(port(0).addr)
    val bank_p1 = bankSel(port(1).addr)

    val bankConflict = bank_p0 === bank_p1
    for (b <- 0 until banks) {
        memBanks(b).CLK := clock
        memBanks(b).CEN := false.B
        memBanks(b).WEN := true.B
        memBanks(b).A   := 0.U
        memBanks(b).D   := 0.U
    }
    when(bankConflict){
        for (i <- 0 until portNum) {
            val bank = bankSel(port(i).addr)
            val row = rowAddr(port(i).addr)
            val wen = port(i).wr
            val wdata = port(i).din
            for (j <- 0 until p.banks) {
                when(j.U === bank){
                    when(wen) {
                        memBanks(j).A   := row
                        memBanks(j).WEN := false.B
                        memBanks(j).D   := wdata
                    }
                }
            }
        }
        when ( !(port(0).wr || port(1).wr) ){
            val bank = bankSel(port(0).addr)
            val row = rowAddr(port(0).addr)
            for (j <- 0 until p.banks) {
                when(j.U === bank){
                    memBanks(j).A   := row
                }
            }
        }
    } .otherwise{
        for (i <- 0 until portNum) {
            val bank = bankSel(port(i).addr)
            val bank_reg = RegNext(port(i).addr)

            val row = rowAddr(port(i).addr)
            val wen = port(i).wr
            val wdata = port(i).din
            for (j <- 0 until p.banks) {
                when(j.U === bank){
                    memBanks(j).A   := row
                    memBanks(j).WEN := !wen
                    memBanks(j).D   := wdata
                }
            }
        }
    }
    val bank_reg = RegNext(bank_p0)
    val bank_reg1 = RegNext(bank_p1)
    val doutVec = Wire(Vec(memBanks.length, UInt(tagRamDataBits.W)))

    val row_p0 = rowAddr(port(0).addr)
    val row_p1 = rowAddr(port(1).addr)
    val sameLine = bankConflict && (row_p0 =/= row_p1)
    for (j <- 0 until memBanks.length) {
        doutVec(j) := memBanks(j).Q
    }
    val conflictLast  = RegNext(sameLine, init = false.B)
    val conflictData = RegEnable(doutVec(bank_reg1), 0.U.asTypeOf(doutVec(0)), sameLine)
    port(0).dout := doutVec(bank_reg)
    port(1).dout := Mux(conflictLast, conflictData, doutVec(bank_reg1))



    // val sram = Module(new SramTagRAM_Generic(p.tagRamIdxBits, p.tagRamDataBits))
    // sram.CLK := clock
    // sram.CEN := false.B
    // sram.WEN := !io.wr
    // sram.A   := io.addr
    // sram.D   := io.din

    // io.dout  := RegNext(sram.Q)
}

class ICacheDataRamMacro(p: ICacheParams) extends Module {
    val io = IO(new DualPortICDataRamIO(p))
    val banks = p.banks
    val portNum = 2
    val memBanks = Seq.fill(banks) (
        Module(new S55NLLG1PH_X128Y2D64(p.dataRamIdxBits, p.dataRamDataBits))
    )

    val port = Wire(Vec(portNum, new ICacheDataRamIO(p)))
    port(0) <> io.p0
    port(1) <> io.p1

    val bank_p0 = bankSel(port(0).addr)
    val bank_p1 = bankSel(port(1).addr)
    val bankConflict = bank_p0 === bank_p1

    for (b <- 0 until banks) {
        memBanks(b).CLK := clock
        memBanks(b).CEN := false.B
        memBanks(b).WEN := true.B
        memBanks(b).A   := 0.U
        memBanks(b).D   := 0.U
    }
    when(bankConflict){
        for (i <- 0 until portNum) {
            val bank = bankSel(port(i).addr)
            val row = rowAddr(port(i).addr)
            val wen = port(i).wr
            val wdata = port(i).din
            for (j <- 0 until p.banks) {
                when(j.U === bank){
                    when(wen) {
                        memBanks(j).A   := row
                        memBanks(j).WEN := false.B
                        memBanks(j).D   := wdata
                    }
                }
            }
        }
        when ( !(port(0).wr || port(1).wr) ){
            val bank = bankSel(port(0).addr)
            val row = rowAddr(port(0).addr)
            for (j <- 0 until p.banks) {
                when(j.U === bank){
                    memBanks(j).A   := row
                }
            }
        }
    } .otherwise{
        for (i <- 0 until portNum) {
            val bank = bankSel(port(i).addr)
            val bank_reg = RegNext(port(i).addr)

            val row = rowAddr(port(i).addr)
            val wen = port(i).wr
            val wdata = port(i).din
            for (j <- 0 until p.banks) {
                when(j.U === bank){
                    memBanks(j).A   := row
                    memBanks(j).WEN := !wen
                    memBanks(j).D   := wdata
                }
            }
        }
    }

    val bank_reg = RegNext(bank_p0)
    val bank_reg1 = RegNext(bank_p1)
    val doutVec = Wire(Vec(memBanks.length, UInt(p.dataRamDataBits.W)))
    for (j <- 0 until memBanks.length) {
        doutVec(j) := memBanks(j).Q
    }
    
    val row_p0 = rowAddr(port(0).addr)
    val row_p1 = rowAddr(port(1).addr)
    val sameLine = bankConflict && (row_p0 =/= row_p1)

    val conflictLast  = RegNext(sameLine, init = false.B)
    val conflictData = RegEnable(doutVec(bank_reg1), 0.U.asTypeOf(doutVec(0)), sameLine)
    port(0).dout := doutVec(bank_reg)
    port(1).dout := Mux(conflictLast, conflictData, doutVec(bank_reg1))    


}