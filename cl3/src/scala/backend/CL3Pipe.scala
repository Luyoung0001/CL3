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
  val flush = Output(Bool())
}

class PipeIO extends Bundle {
  val in  = new PipeInput
  val out = new PipeOutput
}

class CL3Pipe(pipeID: Int) extends Module {

  val io = IO(new PipeIO)

  val e1_q = RegInit(0.U.asTypeOf(new PipeInfo))


  val e2_stall = Wire(Bool())

  val e1_flush = io.in.flush
  val e1_stall = io.in.pipe.e1.stall || io.out.e1.stall

  // when(e1_flush && !e1_stall) {
  //   e1_q.valid := false.B
  // }.elsewhen(!e1_stall) {
  //   e1_q.valid := io.in.issue.fire
  // }

  e1_q.valid := Mux1H(Seq(
    (!e1_flush && !e1_stall) -> io.in.issue.fire,
    (!e1_flush &&  e1_stall) -> e1_q.valid,
    ( e1_flush && !e1_stall) -> false.B,
    ( e1_flush &&  e1_stall) -> false.B

  ))

  when(io.in.issue.fire && !e1_flush && !e1_stall) {
    e1_q.info      := io.in.issue.info
    // TODO: support RVC
    e1_q.npc       := Mux(io.in.exu(0).br.valid, io.in.exu(0).br.pc, io.in.issue.info.pc + 4.U)
    e1_q.rs1       := io.in.issue.rs1
    e1_q.rs2       := io.in.issue.rs2
    e1_q.rs1_id    := io.in.issue.rs1_id
    e1_q.rs2_id    := io.in.issue.rs2_id
    e1_q.rdy_stage := io.in.issue.rdy_stage
    e1_q.except    := io.in.issue.except
  }

  io.out.e1        := e1_q
  io.out.e1.valid  := e1_q.valid
  io.out.e1.result := io.in.exu(0).result

  // io.out.e1.stall  := e1_q.valid && e1_q.info.isDIV && !io.in.div.valid || e2_stall
  io.out.e1.stall  := e1_q.valid && e1_q.info.isDIV && !io.in.div.valid || e2_stall ||
    e1_q.valid && e1_q.info.isLSU && e1_q.rdy_stage(0) && io.in.lsu.stall

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

  val e2_q = RegInit(0.U.asTypeOf(new PipeInfo))

  // val e2_flush = io.in.flush || io.out.flush // TODO:
  // val e2_stall = io.in.stall                 // TODO:
  
  val wb_stall = Wire(Bool())
  e2_stall := io.in.pipe.e2.stall || io.out.e2.stall
  val e2_flush = e1_stall && !e2_stall || e1_flush && !e2_stall

  // when(e2_flush && !io.in.stall) {
  //   e2_q.valid := false.B
  // }.elsewhen(!io.in.stall) {
  //   e2_q        := e1_q
  //   e2_q.result := MuxCase( io.in.exu(0).result, Seq(e1_q.info.isDIV -> io.in.div.result, e1_q.info.isCSR -> io.in.csr.rdata))
  // }

  e2_q.valid := Mux1H(Seq(
    (!e2_flush && !e2_stall) -> e1_q.valid,
    (!e2_flush &&  e2_stall) -> e2_q.valid,
    ( e2_flush && !e2_stall) -> false.B,
    ( e2_flush &&  e2_stall) -> false.B
  ))

  when(!e2_flush && !e2_stall) {
    e2_q := e1_q
    e2_q.result := MuxCase( io.in.exu(0).result, Seq(e1_q.info.isDIV -> io.in.div.result, e1_q.info.isCSR -> io.in.csr.rdata))
  }

  io.out.e2          := e2_q
  io.out.e2.info.wen := !(io.in.pipe.e2.stall || io.out.e2.stall ) && e2_q.info.wen // TODO:
  io.out.e2.result   := MuxCase(e2_q.result, Seq( io.out.e2.isLd -> io.in.lsu.rdata, io.out.e2.isMul -> io.in.mul.result, e2_q.rdy_stage(0) -> io.in.exu(1).result))

  // io.out.e2.stall    := e2_q.valid && e2_q.info.isLSU && e2_q.rdy_stage(0) && !io.in.lsu.valid || wb_stall
  io.out.e2.stall    := e2_q.valid && e2_q.info.isLSU && e2_q.rdy_stage(0) && !io.in.lsu.valid || wb_stall ||
    e2_q.valid && e2_q.info.isLSU && e2_q.rdy_stage(1) && io.in.lsu.stall

  val wb_q = RegInit(0.U.asTypeOf(new PipeInfo))

  wb_stall := io.in.pipe.wb.stall || io.out.wb.stall
  val wb_flush = !wb_stall && e2_stall

  wb_q.valid := Mux1H(Seq(
    (!wb_flush && !wb_stall) -> e2_q.valid,
    (!wb_flush &&  wb_stall) -> wb_q.valid,
    ( wb_flush && !wb_stall) -> false.B,
    ( wb_flush &&  wb_stall) -> false.B
  ))

  when(!wb_flush && !wb_stall) {
    wb_q               := e2_q
    wb_q.result        := io.out.e2.result
    wb_q.mem.cacheable := Mux(e2_q.isMem, io.in.lsu.cacheable, true.B)

  }

  val wb_result_buf_q = RegInit(0.U(32.W))
  val wb_result_buf_valid_q = RegInit(false.B)

  when(wb_flush && io.out.wb.commit) {
    wb_result_buf_q := io.out.wb.result
    wb_result_buf_valid_q := true.B
  }.elsewhen(!wb_flush && !wb_stall) {
    wb_result_buf_valid_q := false.B
  }
  // when(!io.in.stall) {
  //   wb_q               := e2_q
  //   wb_q.result        := io.out.e2.result
  //   wb_q.mem.cacheable := Mux(e2_q.isMem, io.in.lsu.cacheable, true.B)
  // }

  // wb_q.valid := e2_q.valid && !io.in.stall && !io.in.flushWB

  io.out.wb               := wb_q
  io.out.wb.result        := MuxCase(
    wb_q.result,
    Seq(
      (io.out.wb.isLd && wb_q.rdy_stage(1))  -> io.in.lsu.rdata,
      (io.out.wb.isMul && wb_q.rdy_stage(1)) -> io.in.mul.result,
      wb_result_buf_valid_q -> wb_result_buf_q
    )
  )

  

  io.out.wb.mem.cacheable := Mux(wb_q.rdy_stage(1), io.in.lsu.cacheable, wb_q.mem.cacheable)
  io.out.wb.valid := wb_q.valid && !wb_stall

  io.out.wb.stall := wb_q.valid && wb_q.info.isLSU && wb_q.rdy_stage(1) && !io.in.lsu.valid
  io.out.flush := false.B // TODO: add exception
}
