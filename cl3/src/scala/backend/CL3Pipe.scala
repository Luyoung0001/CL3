package cl3

import chisel3._
import chisel3.util._

class PipeInput() extends Bundle {

  val issue = new PipeISInput
  val lsu   = new PipeLSUInput
  val exu   = Vec(2, new PipeEXUInput)
  val div   = new PipeDIVInput
  val mul   = new PipeMULInput
  val csr   = new PipeCSRInput

  val pipe = Flipped(new PipeOutput)

  val irq     = Input(Bool())
  val flush   = Input(Bool())
  val flushWB = Input(Bool())

}

class PipeOutput extends Bundle {
  val e1    = Output(new PipeInfo)
  val e2    = Output(new PipeInfo)
  val wb    = Output(new PipeInfo)
}

class PipeIO extends Bundle {
  val in  = new PipeInput
  val out = new PipeOutput
}

class CL3Pipe(pipeID: Int) extends Module {

  val io = IO(new PipeIO)

  //--------------------------------  E1 stage -----------------------------------//

  val e1_q = RegInit(0.U.asTypeOf(new PipeInfo))

  val e2_stall = Wire(Bool())

  val e1_flush = io.in.flush
  val e1_stall = io.in.pipe.e1.stall || io.out.e1.stall

  val branch_misaligned_w = io.in.exu(0).br.valid && (io.in.exu(0).br.pc(1, 0) =/= 0.U)

  e1_q.valid := Mux1H(Seq(
    (!e1_flush && !e1_stall) -> io.in.issue.fire,
    (!e1_flush &&  e1_stall) -> e1_q.valid,
    ( e1_flush && !e1_stall) -> false.B,
    ( e1_flush &&  e1_stall) -> false.B
  ))

  when(io.in.issue.fire && !e1_flush && !e1_stall) {
    e1_q.info      := io.in.issue.info
    e1_q.rs1       := io.in.issue.rs1
    e1_q.rs2       := io.in.issue.rs2
    e1_q.rs1_id    := io.in.issue.rs1_id
    e1_q.rs2_id    := io.in.issue.rs2_id
    e1_q.rdy_stage := io.in.issue.rdy_stage

    e1_q.except := MuxCase(ExceptionInfo.none, Seq(
      io.in.irq -> ExceptionInfo.apply("h20".U(6.W), io.in.issue.info.pc, 0.U), //TODO: timer or ext ?
      io.in.issue.except.valid -> io.in.issue.except,
      branch_misaligned_w -> ExceptionInfo.apply("h10".U(6.W), io.in.issue.info.pc, 0.U),
    ))

    e1_q.npc := MuxCase(io.in.issue.info.pc + 4.U, Seq(
      io.in.exu(0).br.valid -> io.in.exu(0).br.pc
    ))

  }
  
  io.out.e1        := e1_q
  io.out.e1.result := io.in.exu(0).result
  io.out.e1.stall  := e2_stall ||
    e1_q.valid && e1_q.info.isDIV && !io.in.div.valid ||
    e1_q.valid && e1_q.info.isLSU && e1_q.rdy_stage(0) && io.in.lsu.stall
    

  io.out.e1.commit := e1_q.valid && !io.out.e1.stall

  def getBypassResult(id: UInt, default: UInt): UInt = {
    if (pipeID == 0) {
      MuxLookup(id, default)(
        Seq(
          2.U -> io.out.e2.result,
          3.U -> io.in.pipe.e2.result,
          4.U -> io.out.wb.result,
          5.U -> io.in.pipe.wb.result
        )
      )
    } else {
      MuxLookup(id, default)(
        Seq(
          1.U -> io.in.pipe.e1.result,
          2.U -> io.in.pipe.e2.result,
          3.U -> io.out.e2.result,
          4.U -> io.in.pipe.wb.result,
          5.U -> io.out.wb.result
        )
      )
    }
  }

  io.out.e1.rs1 := getBypassResult(e1_q.rs1_id, e1_q.rs1)
  io.out.e1.rs2 := getBypassResult(e1_q.rs2_id, e1_q.rs2)

  //--------------------------------  E2 stage -----------------------------------//

  val e2_q = RegInit(0.U.asTypeOf(new PipeInfo))

  val wb_stall = Wire(Bool())
  e2_stall := io.in.pipe.e2.stall || io.out.e2.stall
  val e2_flush = e1_stall && !e2_stall || e1_flush && !e2_stall

  e2_q.valid := Mux1H(Seq(
    (!e2_flush && !e2_stall) -> e1_q.valid,
    (!e2_flush &&  e2_stall) -> e2_q.valid,
    ( e2_flush && !e2_stall) -> false.B,
    ( e2_flush &&  e2_stall) -> false.B
  ))

  val e2_is_waiting_lsu = e2_q.valid && e2_q.info.isLSU && e2_q.rdy_stage(0) && !e2_q.except.valid
  val e2_is_sending_lsu = e2_q.valid && e2_q.info.isLSU && e2_q.rdy_stage(1) && !e2_q.except.valid

  when(!e2_flush && !e2_stall) {
    e2_q       := e1_q
    e2_q.info  := e1_q.info
    e2_q.result := MuxCase( io.in.exu(0).result, Seq(e1_q.info.isDIV -> io.in.div.result, e1_q.info.isCSR -> io.in.csr.rdata))
    e2_q.csr.wen   := io.in.csr.wen && e1_q.info.isCSR
    e2_q.csr.wdata := io.in.csr.wdata

    e2_q.except := Mux(e2_is_waiting_lsu, io.in.lsu.except, e1_q.except)

  }

  io.out.e2          := e2_q
  io.out.e2.info.wen := !(io.in.pipe.e2.stall || io.out.e2.stall ) && e2_q.info.wen // TODO:
  io.out.e2.result   := MuxCase(e2_q.result, Seq( io.out.e2.isLd -> io.in.lsu.rdata, io.out.e2.isMul -> io.in.mul.result, e2_q.rdy_stage(0) -> io.in.exu(1).result))

  io.out.e2.stall    := wb_stall ||
    e2_is_waiting_lsu && !io.in.lsu.valid ||
    e2_is_sending_lsu &&  io.in.lsu.stall

  io.out.e2.commit   := io.out.e2.info.wen


  //--------------------------------  WB stage -----------------------------------//

  val wb_q = RegInit(0.U.asTypeOf(new PipeInfo))

  wb_stall := io.in.pipe.wb.stall || io.out.wb.stall
  val wb_flush = !wb_stall && e2_stall || io.in.flushWB

  wb_q.valid := Mux1H(Seq(
    (!wb_flush && !wb_stall) -> e2_q.valid,
    (!wb_flush &&  wb_stall) -> wb_q.valid,
    ( wb_flush && !wb_stall) -> false.B,
    ( wb_flush &&  wb_stall) -> false.B
  ))

  when(!wb_flush && !wb_stall) {
    wb_q               := e2_q
    wb_q.except        := e2_q.except
    wb_q.result        := io.out.e2.result
    wb_q.mem.cacheable := Mux(e2_q.isMem, io.in.lsu.cacheable, true.B)

    wb_q.csr.waddr := Mux(e2_q.csr.wen, e2_q.info.inst(31, 20), 0.U)
    wb_q.csr.wdata := e2_q.csr.wdata
    wb_q.csr.wen   := e2_q.csr.wen

  }

  val wb_is_waiting_lsu = wb_q.valid && wb_q.info.isLSU && wb_q.rdy_stage(1) && !wb_q.except.valid

  val wb_result_buf_q = RegInit(0.U(32.W))
  val wb_result_buf_valid_q = RegInit(false.B)

  when(wb_flush && io.out.wb.commit && (io.out.wb.isLd || io.out.wb.isMul)) {
    wb_result_buf_q := io.out.wb.result
    wb_result_buf_valid_q := true.B
  }.elsewhen(!wb_flush && !wb_stall) {
    wb_result_buf_valid_q := false.B
  }

  io.out.wb               := wb_q
  io.out.wb.result        := MuxCase(wb_q.result, Seq(
    (io.out.wb.isLd  && wb_q.rdy_stage(1))  -> io.in.lsu.rdata,
    (io.out.wb.isMul && wb_q.rdy_stage(1)) -> io.in.mul.result,
    wb_result_buf_valid_q -> wb_result_buf_q
  ))

  io.out.wb.mem.cacheable := Mux(wb_q.rdy_stage(1), io.in.lsu.cacheable, wb_q.mem.cacheable)
  io.out.wb.commit := wb_q.valid && !wb_stall
  io.out.wb.stall  := wb_is_waiting_lsu && !io.in.lsu.valid
  io.out.wb.except := Mux(wb_is_waiting_lsu, io.in.lsu.except, wb_q.except)
  io.out.wb.npc    := Mux(io.out.wb.except.valid, io.in.csr.tvec, wb_q.npc)
}
