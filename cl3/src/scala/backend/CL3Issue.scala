package cl3

import chisel3._
import chisel3.util._
import javax.swing.plaf.synth.Region

class CL3IssueIO extends Bundle {
  val in = new Bundle {
    val fetch = Vec(2, Flipped(Decoupled(Input(new DEInfo))))
    val csr   = new ISCSRInput
    val exec  = Vec(4, new ISEXUInput)
    val mul   = new PipeMULInput // TODO: maybe we can use BoringUtil here
    val div   = new PipeDIVInput
    val lsu   = new PipeLSUInput
    val irq   = Input(Bool())
  }

  val out = new Bundle {
    val br   = Output(new BrInfo)
    val bp   = Output(new BpInfo)
    val op   = Vec(8, Output(new OpInfo))
    val csr  = new ISCSROutput
    val hold = Output(Bool())
    val flush = Output(Bool())

    val debug = new Bundle {
      val fetch0_ok          = Output(Bool())
      val fetch1_ok          = Output(Bool())
      val slot0_data_check   = Output(Bool())
      val slot1_data_check   = Output(Bool())
      val slot0_struct_check = Output(Bool())
      val slot1_struct_check = Output(Bool())
      val slot0_order_check  = Output(Bool())
      val slot1_order_check  = Output(Bool())
      val slot1_type_check   = Output(Bool())
      val slot1_data_check_internal = Output(Bool())
      val delay0             = Output(UInt(2.W))
      val id0                = Output(UInt(3.W))
      val inst_delay0        = Output(UInt(2.W))
      val delay1             = Output(UInt(2.W))
      val id1                = Output(UInt(3.W))
      val inst_delay1        = Output(UInt(2.W))

      val slot0_rdIdx        = Output(UInt(5.W))
      val slot1_rs1Idx       = Output(UInt(5.W))
      val slot1_rs2Idx       = Output(UInt(5.W))

      val mismatch0          = Output(Bool())
      val mismatch1          = Output(Bool())
      val dual_issue         = Output(Bool())
      val single_issue       = Output(Bool())

      val pipe1mem           = Output(Bool())
      val rdystage           = Output(Bool())

      val mispred = Output(UInt(32.W))
      val brj_num = Output(UInt(32.W))
    }
  }
}

class CL3Issue extends Module with CL3Config {
  val io = IO(new CL3IssueIO)

  val fetch0 = io.in.fetch(0)
  val fetch1 = io.in.fetch(1)

  val pc_q   = RegInit(0.U(32.W))
  val priv_q = RegEnable("b11".U(2.W), io.in.csr.br.valid)

  val single_issue = Wire(Bool())
  val dual_issue   = Wire(Bool())
  val flush        = Wire(Bool())
  val stall        = Wire(Bool())


  dontTouch(io)

  // val fetch0_pc_match = fetch0.bits.pc(31, 1) === (pc_q(31, 1))
  // val fetch1_pc_match = fetch1.bits.pc(31, 1) === (pc_q(31, 1))

  val fetch0_ok = fetch0.valid && !(flush || io.out.br.valid)
  // val fetch0_ok = fetch0.valid
  val fetch1_ok = fetch1.valid && fetch0_ok

  // val mispred = (fetch0.valid || fetch1.valid) && !(fetch0_ok || fetch1_ok)

  val slot = Wire(Vec(2, Valid(new DEInfo)))

  // slot(0).bits  := Mux(fetch0_ok, fetch0.bits, fetch1.bits)
  // slot(0).valid := Mux(fetch0_ok, true.B, Mux(fetch1_ok, true.B, false.B))

  // slot(1).bits  := fetch1.bits
  // slot(1).valid := Mux(fetch0_ok, fetch1_ok, false.B)
  slot(0).bits  := fetch0.bits
  slot(0).valid := fetch0_ok
  slot(1).bits  := fetch1.bits
  slot(1).valid := fetch1_ok

  val pipes  = Seq(Module(new CL3Pipe(0)), Module(new CL3Pipe(1)))
  val rf     = Module(new CL3RF())
  val bypass = Module(new BypassNetwork)

  stall       := pipes.map(_.io.out.e1.stall).reduce(_ || _)
  flush       := pipes.map(_.io.out.flush).reduce(_ || _)
  io.out.hold := stall

  val slot_op = Wire(Vec(2, new OpInfo))

  for (i <- 0 until 2) {
    // pipes(i).io.in.stall := stall
    pipes(i).io.in.irq   := io.in.irq
    pipes(i).io.in.csr   := io.in.csr
    pipes(i).io.in.lsu   := io.in.lsu
    pipes(i).io.in.mul   := io.in.mul
    pipes(i).io.in.div   := io.in.div

    for (j <- 0 until 2) {
      pipes(i).io.in.exu(j).br     := io.in.exec(j * 2 + i).br
      pipes(i).io.in.exu(j).result := io.in.exec(j * 2 + i).result
    }
    pipes(i).io.in.issue.info := slot(i).bits
    pipes(i).io.in.issue.except := 0.U // TODO:

    val rfRdA = i * 2
    val rfRdB = i * 2 + 1

    bypass.io.in.issue(i).rs1Idx := slot(i).bits.rs1Idx
    bypass.io.in.issue(i).rs1    := rf.io.rd(rfRdA).rdata
    bypass.io.in.issue(i).rs2Idx := slot(i).bits.rs2Idx
    bypass.io.in.issue(i).rs2    := rf.io.rd(rfRdB).rdata

    bypass.io.in.pipe(i) := pipes(i).io.out

    rf.io.rd(rfRdA).raddr := slot(i).bits.rs1Idx
    rf.io.rd(rfRdB).raddr := slot(i).bits.rs2Idx

    slot_op(i)       := OpInfo.fromDE(slot(i).bits)
    slot_op(i).valid := slot(i).valid
    slot_op(i).rs1   := bypass.io.out.issue(i).rs1
    slot_op(i).rs2   := bypass.io.out.issue(i).rs2

    pipes(i).io.in.issue.rs1 := slot_op(i).rs1
    pipes(i).io.in.issue.rs2 := slot_op(i).rs2
  }

  val slot0_mismatch = Wire(Bool())

  pipes(0).io.in.flush := false.B // TODO: check this
  pipes(1).io.in.flush := slot0_mismatch // TODO: check this

  pipes(0).io.in.flushWB := false.B
  pipes(1).io.in.flushWB := pipes(0).io.out.flush

  pipes(0).io.in.pipe := pipes(1).io.out
  pipes(1).io.in.pipe := pipes(0).io.out

  val byp_res = bypass.io.out.issue
  pipes(0).io.in.issue.rs1_id := byp_res(0).rs1_id
  pipes(0).io.in.issue.rs2_id := byp_res(0).rs2_id

  val rs1_dep_internal  = (slot_op(0).rdIdx === slot_op(1).rs1Idx) && slot_op(0).wen && slot_op(0).rdIdx.orR //TODO:
  val rs2_dep_internal  = (slot_op(0).rdIdx === slot_op(1).rs2Idx) && slot_op(0).wen && slot_op(0).rdIdx.orR //TODO:
  val data_dep_internal = rs1_dep_internal || rs2_dep_internal

  io.out.debug.slot0_rdIdx := slot_op(0).rdIdx
  io.out.debug.slot1_rs1Idx := slot_op(1).rs1Idx
  io.out.debug.slot1_rs2Idx := slot_op(1).rs2Idx

  pipes(1).io.in.issue.rs1_id := Mux(rs1_dep_internal, 1.U, byp_res(1).rs1_id)
  pipes(1).io.in.issue.rs2_id := Mux(rs2_dep_internal, 1.U, byp_res(1).rs2_id)

  val pipe_e1 = Wire(Vec(2, new PipeInfo))
  pipe_e1(0) := pipes(0).io.out.e1
  pipe_e1(1) := pipes(1).io.out.e1

  val lsu_delay = pipe_e1(0).isMem && pipe_e1(0).rdy_stage(1) ||
    pipe_e1(1).isMem && pipe_e1(1).rdy_stage(1)

  val mul_delay = pipe_e1(0).isMul && pipe_e1(0).rdy_stage(1) ||
    pipe_e1(1).isMul && pipe_e1(1).rdy_stage(1)

  val div_pending = Wire(Bool())

  val slot0_data_check = Mux1H(
    Seq(
      slot(0).bits.isBr  -> byp_res(0).delay.orR,
      slot(0).bits.isEXU -> byp_res(0).delay(1),
      slot(0).bits.isCSR -> byp_res(0).delay.orR,
      slot(0).bits.isDIV -> byp_res(0).delay.orR,
      slot(0).bits.isMUL -> byp_res(0).delay(1),
      slot(0).bits.isLSU -> byp_res(0).delay(1)
    )
  )

  val slot0_struct_check = io.in.lsu.stall || div_pending || stall

  val slot0_order_check = (pipe_e1(0).isMem || pipe_e1(1).isMem) &&
    (slot(0).bits.isDIV || slot(0).bits.isMUL || slot(0).bits.isCSR)

  val slot0_fire = !(slot0_data_check || slot0_struct_check || slot0_order_check || io.in.irq) && slot_op(0).valid

  val slot1_data_check = Mux1H(
    Seq(
      slot(1).bits.isBr  -> byp_res(1).delay.orR,
      slot(1).bits.isEXU -> byp_res(1).delay(1),
      slot(1).bits.isCSR -> byp_res(1).delay.orR, // TODO:
      slot(1).bits.isDIV -> byp_res(1).delay.orR, // TODO:
      slot(1).bits.isMUL -> byp_res(1).delay(1),
      slot(1).bits.isLSU -> byp_res(1).delay(1)
    )
  )

  val slot1_data_check_internal = data_dep_internal &&
    (pipes(0).io.in.issue.rdy_stage =/= 0.U || slot(1).bits.isBr)

  val slot1_struct_check = io.in.lsu.stall || div_pending || stall || !slot0_fire

  val slot1_order_check = (pipe_e1(0).isMem || pipe_e1(1).isMem) &&
    (slot(1).bits.isDIV || slot(1).bits.isMUL || slot(1).bits.isCSR) &&
    slot(1).valid

  // val slot1_type_check = !(slot(1).bits.isEXU && (slot(0).bits.isEXU || slot(0).bits.isLSU  || slot(0).bits.isMUL) ||
  val slot1_type_check = !(slot(1).bits.isEXU && (slot(0).bits.isEXU || slot(0).bits.isLSU || slot(0).bits.isMUL || slot(0).bits.isBr) ||
    slot(1).bits.isBr  && (slot(0).bits.isEXU ||  slot(0).bits.isLSU || slot(0).bits.isMUL) ||
    slot(1).bits.isLSU && (slot(0).bits.isEXU ||  slot(0).bits.isMUL || slot(0).bits.isBr)  ||
    slot(1).bits.isMUL && (slot(0).bits.isEXU ||  slot(0).bits.isLSU || slot(0).bits.isBr)  || !slot(0).valid) && slot(1).valid

  val slot1_fire = slot_op(1).valid &&
    !(slot1_data_check_internal || slot1_data_check || slot1_struct_check || slot1_type_check || slot1_order_check || io.in.irq)

  // TODO: timing check
  pipes(0).io.in.issue.rdy_stage := MuxCase(
    byp_res(0).delay + slot(0).bits.inst_delay,
    Seq(
      (lsu_delay && slot(0).bits.isLSU) -> 2.U(2.W),
      (mul_delay && slot(0).bits.isMUL) -> 2.U(2.W)
    )
  )
  pipes(0).io.in.issue.fire      := slot0_fire
  pipes(1).io.in.issue.fire      := slot1_fire
  pipes(1).io.in.issue.rdy_stage := MuxCase(
    byp_res(1).delay + slot(1).bits.inst_delay,
    Seq(
      data_dep_internal                 -> (1.U(2.W) + slot(1).bits.inst_delay), // TODO:
      (lsu_delay && slot(1).bits.isLSU) -> 2.U(2.W),
      (mul_delay && slot(1).bits.isMUL) -> 2.U(2.W)
    )
  )

// TODO: CSR
  rf.io.wr(0).wen   := pipes(0).io.out.wb.info.wen && pipes(0).io.out.wb.commit
  rf.io.wr(0).waddr := pipes(0).io.out.wb.rdIdx
  rf.io.wr(0).wdata := pipes(0).io.out.wb.result

  rf.io.wr(1).wen   := pipes(1).io.out.wb.info.wen && pipes(1).io.out.wb.commit
  rf.io.wr(1).waddr := pipes(1).io.out.wb.rdIdx
  rf.io.wr(1).wdata := pipes(1).io.out.wb.result

  // EXU 0
  io.out.op(0)       := slot_op(0)
  io.out.op(0).valid := pipes(0).io.in.issue.fire

  // EXU 1
  io.out.op(1)       := slot_op(1)
  io.out.op(1).valid := pipes(1).io.in.issue.fire

  // LSU
  // io.out.op(2)       := lsu_op
  // io.out.op(2).valid := lsu_op.valid && ~io.in.irq && (slot0_issue_lsu || slot1_issue_lsu)

  val issue_slot0_lsu = slot(0).bits.isLSU && slot0_fire && pipes(0).io.in.issue.rdy_stage(0)
  val issue_slot1_lsu = slot(1).bits.isLSU && slot1_fire && pipes(1).io.in.issue.rdy_stage(0)
  val issue_lsu_op    = Wire(new OpInfo)
  issue_lsu_op       := Mux(issue_slot0_lsu, slot_op(0), slot_op(1))
  issue_lsu_op.valid := (issue_slot0_lsu || issue_slot1_lsu) && ~io.in.irq


  val mispred = Wire(Bool())
  val e1_slot0_lsu = pipe_e1(0).isMem && pipe_e1(0).rdy_stage(1) && pipe_e1(0).commit
  val e1_slot1_lsu = pipe_e1(1).isMem && pipe_e1(1).rdy_stage(1) && pipe_e1(1).commit && !mispred
  val e1_lsu_op    = Wire(new OpInfo)
  e1_lsu_op       := Mux(e1_slot1_lsu, OpInfo.fromPipe(pipe_e1(1)), OpInfo.fromPipe(pipe_e1(0)))
  e1_lsu_op.valid := e1_slot0_lsu || e1_slot1_lsu

  io.out.op(2) := Mux(e1_lsu_op.valid, e1_lsu_op, issue_lsu_op)

  // MUL
  val issue_slot0_mul = slot(0).bits.isMUL && slot0_fire && pipes(0).io.in.issue.rdy_stage(0)
  val issue_slot1_mul = slot(1).bits.isMUL && slot1_fire && pipes(1).io.in.issue.rdy_stage(0)
  // io.out.op(3)       := mul_op
  // io.out.op(3).valid := mul_op.valid && ~io.in.irq && (slot0_issue_mul || slot1_issue_mul)
  val issue_mul_op    = Wire(new OpInfo)
  issue_mul_op       := Mux(issue_slot0_mul, slot_op(0), slot_op(1))
  issue_mul_op.valid := (issue_slot0_mul || issue_slot1_mul) && ~io.in.irq

  val e1_slot0_mul = pipe_e1(0).isMul && pipe_e1(0).rdy_stage(1) && pipe_e1(0).commit
  val e1_slot1_mul = pipe_e1(1).isMul && pipe_e1(1).rdy_stage(1) && pipe_e1(1).commit

  val e1_mul_op = Wire(new OpInfo)
  e1_mul_op       := Mux(e1_slot1_mul, OpInfo.fromPipe(pipe_e1(1)), OpInfo.fromPipe(pipe_e1(0)))
  e1_mul_op.valid := e1_slot0_mul || e1_slot1_mul

  io.out.op(3) := Mux(e1_mul_op.valid, e1_mul_op, issue_mul_op)

  // DIV
  io.out.op(4)       := slot_op(0)
  io.out.op(4).valid := pipes(0).io.in.issue.fire && slot(0).bits.isDIV

  val div_pending_q = RegInit(false.B)
  when(pipes(0).io.in.flush || pipes(1).io.in.flush) {
    div_pending_q := false.B
  }.elsewhen(io.out.op(4).valid) {
    div_pending_q := true.B
  }.elsewhen(io.in.div.valid) {
    div_pending_q := false.B
  }
  div_pending := div_pending_q

  // CSR
  io.out.op(5)       := slot_op(0)
  io.out.op(5).valid := slot_op(0).valid && ~io.in.irq && slot(0).bits.isCSR

  dual_issue   := pipes(1).io.in.issue.fire && !io.in.irq
  single_issue := pipes(0).io.in.issue.fire && !dual_issue && !io.in.irq

  // EXEC2
  io.out.op(6)       := OpInfo.fromPipe(pipe_e1(0))
  io.out.op(6).valid := pipe_e1(0).isALU && pipe_e1(0).rdy_stage(0)

  // EXEC3
  io.out.op(7)       := OpInfo.fromPipe(pipe_e1(1))
  io.out.op(7).valid := pipe_e1(1).isALU && pipe_e1(1).rdy_stage(1)

  // Branch


  val slot_pred_q    = RegInit(VecInit(Seq.fill(2)(false.B)))
  when(slot0_fire) {
    slot_pred_q(0) := slot(0).bits.pred
  }.otherwise{
    slot_pred_q(0) := false.B
  }
  when(slot1_fire) {
    slot_pred_q(1) := slot(1).bits.pred
  }.otherwise{
    slot_pred_q(1) := false.B
  }

  val slot_br_q     = RegInit(VecInit(Seq.fill(2)(false.B)))
  when(io.in.exec(0).br.valid) {
    slot_br_q(0)  := true.B
  }.otherwise{
    slot_br_q(0) := false.B
  }

  when(io.in.exec(1).br.valid) {
    slot_br_q(1)  := true.B
  }.otherwise{
    slot_br_q(1) := false.B
  }
  
  val flag = RegInit(0.U(2.W))

  when(single_issue) {
    flag := 1.U
  }.elsewhen(dual_issue) {
    flag := 2.U
  }.otherwise {
    flag := 0.U
  }

  pc_q := MuxCase(
    pc_q,
    Seq(
      io.in.csr.br.valid     -> io.in.csr.br.pc,
      io.out.br.valid -> io.out.br.pc,
      (io.in.exec(0).br.valid && dual_issue) -> (fetch1.bits.pc + 4.U),
      (io.in.exec(0).br.valid && single_issue) -> io.in.exec(0).br.pc,
      io.in.exec(1).br.valid -> io.in.exec(1).br.pc,
      dual_issue          -> (pc_q + 8.U),
      single_issue        -> (pc_q + 4.U),
    )
  )

  val second_pc = RegInit(0.U(32.W))
  when(dual_issue) {
    second_pc := fetch1.bits.pc
  }.otherwise{
    second_pc := 0.U
  }


  val pc_mismatch = flag(1) && !slot_br_q(0) && second_pc =/= io.in.exec(0).bp.source + 4.U ||
    slot_br_q(0) && second_pc =/= io.in.exec(0).bp.target

  val another_mismatch =
    flag(1) && slot_br_q(1) && slot_pred_q(1) && fetch0.bits.pc =/= pc_q 

  slot0_mismatch := (slot_br_q(0) ^ slot_pred_q(0)) || pc_mismatch
  val slot1_mismatch = (slot_br_q(1) ^ slot_pred_q(1)) || another_mismatch

  val next_pc = MuxCase(
    pc_q,
    Seq(
      (flag(1) && !slot_pred_q(0) && slot_br_q(0)) -> io.in.exec(0).bp.target,
      (flag(1) && slot_pred_q(0) && !slot_br_q(0)) -> (io.in.exec(0).bp.source + 4.U),
      (flag(1) && pc_mismatch) -> io.in.exec(0).bp.target,
    )
  )

  mispred := slot0_mismatch || slot1_mismatch || pc_mismatch

  io.out.flush := slot0_mismatch && pipe_e1(1).isMem && pipe_e1(1).rdy_stage(0)

  io.out.br.valid := mispred || io.in.csr.br.valid
  io.out.br.pc    := Mux(io.in.csr.br.valid, io.in.csr.br.pc, next_pc)
  io.out.br.priv  := Mux(io.in.csr.br.valid, io.in.csr.br.priv, 3.U) //TODO:

  io.out.bp := Mux(pipe_e1(1).isBr || pipe_e1(1).isJmp, io.in.exec(1).bp, io.in.exec(0).bp)

  io.out.csr.waddr  := pipes(0).io.out.wb.csr.waddr
  io.out.csr.wdata  := pipes(0).io.out.wb.csr.wdata
  io.out.csr.wen    := pipes(0).io.out.wb.csr.wen
  io.out.csr.except := 0.U // TODO:

  // handshake

  io.in.fetch(0).ready := (fetch0_ok && pipes(0).io.in.issue.fire) && !io.in.irq
  // io.in.fetch(1).ready := (fetch1_ok && !fetch0_ok && pipes(0).io.in.issue.fire || pipes(
  //   1
  // ).io.in.issue.fire) && !io.in.irq
  io.in.fetch(1).ready := (fetch1_ok && pipes(1).io.in.issue.fire) && !io.in.irq

  if (EnableDiff) {
    val difftest = Module(new Difftest)
    difftest.io.reset := reset
    difftest.io.clock := clock

    // TODO: use a more elegant way to do signal connection
    difftest.io.diff_info(0).commit := pipes(0).io.out.wb.commit
    difftest.io.diff_info(0).pc     := pipes(0).io.out.wb.info.pc
    difftest.io.diff_info(0).inst   := pipes(0).io.out.wb.info.inst
    difftest.io.diff_info(0).skip   := !pipes(0).io.out.wb.mem.cacheable
    difftest.io.diff_info(0).npc    := pipes(0).io.out.wb.npc
    difftest.io.diff_info(0).rdIdx  := pipes(0).io.out.wb.rdIdx
    difftest.io.diff_info(0).wen    := pipes(0).io.out.wb.info.wen
    difftest.io.diff_info(0).wdata  := pipes(0).io.out.wb.result

    difftest.io.diff_info(1).commit := pipes(1).io.out.wb.commit
    difftest.io.diff_info(1).pc     := pipes(1).io.out.wb.info.pc
    difftest.io.diff_info(1).inst   := pipes(1).io.out.wb.info.inst
    difftest.io.diff_info(1).skip   := !pipes(1).io.out.wb.mem.cacheable
    difftest.io.diff_info(1).npc    := pipes(1).io.out.wb.npc
    difftest.io.diff_info(1).rdIdx  := pipes(1).io.out.wb.rdIdx
    difftest.io.diff_info(1).wen    := pipes(1).io.out.wb.info.wen
    difftest.io.diff_info(1).wdata  := pipes(1).io.out.wb.result
  }

  io.out.debug.fetch0_ok          := fetch0_ok
  io.out.debug.fetch1_ok          := fetch1_ok
  io.out.debug.slot0_data_check   := slot0_data_check
  io.out.debug.slot0_struct_check := slot0_struct_check
  io.out.debug.slot0_order_check  := slot0_order_check
  io.out.debug.slot1_data_check   := slot1_data_check
  io.out.debug.slot1_order_check  := slot1_order_check
  io.out.debug.slot1_struct_check := slot1_struct_check
  io.out.debug.slot1_type_check   := slot1_type_check
  io.out.debug.slot1_data_check_internal := slot1_data_check_internal

  io.out.debug.delay0      := byp_res(0).delay
  io.out.debug.id0         := byp_res(0).id
  io.out.debug.inst_delay0 := slot(0).bits.inst_delay
  io.out.debug.delay1      := byp_res(1).delay
  io.out.debug.id1         := byp_res(1).id
  io.out.debug.inst_delay1 := slot(1).bits.inst_delay
  io.out.debug.mismatch0   := slot0_mismatch
  io.out.debug.mismatch1   := slot1_mismatch
  io.out.debug.dual_issue  := dual_issue
  io.out.debug.single_issue := single_issue
  io.out.debug.rdystage    := pipe_e1(1).rdy_stage(1)
  io.out.debug.pipe1mem    := pipe_e1(1).isMem

  val mispred_q = RegInit(0.U(32.W))
  when(mispred) {
     mispred_q := mispred_q + 1.U
  }
  val brj_num_q = RegInit(0.U(32.W))
  when(io.out.bp.valid) {
    brj_num_q := brj_num_q + 1.U
  }

  io.out.debug.mispred := mispred_q
  io.out.debug.brj_num := brj_num_q

  dontTouch(io.out.debug)

}
