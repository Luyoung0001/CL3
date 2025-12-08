package cl3

import chisel3._
import chisel3.util._

class BypassResult extends Bundle {
  val data  = UInt(32.W)
  val id    = UInt(3.W)
  val delay = UInt(2.W)
}

class BypassResultPair extends Bundle {
  val a = Output(new BypassResult)
  val b = Output(new BypassResult)

  def rs1:    UInt = a.data
  def rs2:    UInt = b.data
  def delay:  UInt = Mux(a.delay > b.delay, a.delay, b.delay)
  def id:     UInt = Mux(a.delay > b.delay, a.id, b.id)
  def rs1_id: UInt = Mux(a.delay.orR, a.delay ## 0.U(1.W) + a.id, 0.U)
  def rs2_id: UInt = Mux(b.delay.orR, b.delay ## 0.U(1.W) + b.id, 0.U)
}

class BypassOutput extends Bundle {
  val issue = Output(Vec(2, new BypassResultPair))
}

class BypassInput extends Bundle {
  val issue = Input(Vec(2, new BypassISInfo))
  val pipe  = Input(Vec(2, new PipeOutput))
}

class BypassIO extends Bundle {
  val in  = new BypassInput
  val out = new BypassOutput
}

class BypassNetwork extends Module with PipeConstant {

  val io = IO(new BypassIO)

  val pipes = Seq(io.in.pipe(0), io.in.pipe(1))
  val issue = Seq(io.in.issue(0), io.in.issue(1))

  type BypassCond  = PipeInfo => Bool
  type StageAccess = PipeOutput => PipeInfo

  val e1_cond: BypassCond = (p: PipeInfo) => p.valid && p.info.wen
  val e2_cond: BypassCond = (p: PipeInfo) => p.valid && p.info.wen
  val wb_cond: BypassCond = (p: PipeInfo) => p.valid && p.info.wen

  val stageInfo     = Seq(
    ((p: PipeOutput) => p.e1, e1_cond, P0_E1, P1_E1),
    ((p: PipeOutput) => p.e2, e2_cond, P0_E2, P1_E2),
    ((p: PipeOutput) => p.wb, wb_cond, P0_WB, P1_WB)
  )
  val bypassSources = stageInfo.flatMap {
    case (getStage, condition, p0_id, p1_id) => {

      val info_p0 = getStage(pipes(0))
      val info_p1 = getStage(pipes(1))

      Seq(
        (condition(info_p1), info_p1.rdIdx, info_p1.result, p1_id, info_p1.rdy_stage),
        (condition(info_p0), info_p0.rdIdx, info_p0.result, p0_id, info_p0.rdy_stage)
      )
    }
  }

  def getDefaultResult(defaultData: UInt): BypassResult = {
    val res = Wire(new BypassResult)
    res.data  := defaultData
    res.id    := 0.U
    res.delay := 0.U
    res
  }
  // val bypassSources = Seq(
  //   (pipe1.e1.isALU || pipe1.e1.isJmp, pipe1.e1.rdIdx, pipe1.e1.result),
  //   (pipe0.e1.isALU || pipe0.e1.isJmp, pipe0.e1.rdIdx, pipe0.e1.result),
  //   (pipe1.e2.valid && pipe1.e2.info.wen, pipe1.e2.rdIdx, pipe1.e2.result),
  //   (pipe0.e2.valid && pipe0.e2.info.wen, pipe0.e2.rdIdx, pipe0.e2.result),
  //   (pipe1.wb.valid && pipe1.wb.info.wen, pipe1.wb.rdIdx, pipe1.wb.result),
  //   (pipe0.wb.valid && pipe0.wb.info.wen, pipe0.wb.rdIdx, pipe0.wb.result)
  // )

  // io.out.ra0 := BypassNetwork(issue(0).raIdx, issue(0).ra, bypassSources)
  // io.out.rb0 := BypassNetwork(issue(0).rbIdx, issue(0).rb, bypassSources)
  // io.out.ra1 := BypassNetwork(issue(1).raIdx, issue(1).ra, bypassSources)
  // io.out.rb1 := BypassNetwork(issue(1).rbIdx, issue(1).rb, bypassSources)

  for (i <- 0 until 2) {
    io.out.issue(i).a := BypassNetwork(issue(i).rs1Idx, getDefaultResult(issue(i).rs1), bypassSources)
    io.out.issue(i).b := BypassNetwork(issue(i).rs2Idx, getDefaultResult(issue(i).rs2), bypassSources)
  }

}

// object BypassNetwork {

//   type BypassSource = (Bool, UInt, UInt, UInt, UInt)

//   def apply(rsIdx: UInt, defaultValue: BypassResult, sources: Seq[BypassSource]): BypassResult = {
//     val cases = sources.map { case (condition, rdIdx, data, srcid, rdyIdx) =>
//       val bypass_hit = condition && (rdIdx =/= 0.U) && (rdIdx === rsIdx)

//       val result = Wire(new BypassResult)
//       result.data := data
//       result.id   := srcid

//       val currentStageIdx = srcid(2, 1)
//       result.delay := Mux(rdyIdx >= currentStageIdx, rdyIdx - currentStageIdx, 0.U) // TODO:

//       (bypass_hit, result)
//     }

//     val defaultCase = (true.B, defaultValue)
//     PriorityMux(cases :+ defaultCase)
//   }
// }
object BypassNetwork {

  type BypassSource = (Bool, UInt, UInt, UInt, UInt)

  def apply(rsIdx: UInt, defaultValue: BypassResult, sources: Seq[BypassSource]): BypassResult = {

    val hits = sources.map { case (condition, rdIdx, _, _, _) =>
      condition && (rdIdx =/= 0.U) && (rdIdx === rsIdx)
    }

    val dataCandidates = sources.map { case (_, _, data, srcid, rdyIdx) =>
      val res = Wire(new BypassResult)
      res.data := data
      res.id   := srcid

      val currentStageIdx = srcid(2, 1)
      res.delay := Mux(rdyIdx >= currentStageIdx, rdyIdx - currentStageIdx, 0.U)
      res
    }

    val selOH = PriorityEncoderOH(hits)

    val anyHit = hits.reduce(_ || _)

    val bypassData = Mux1H(selOH, dataCandidates)

    Mux(anyHit, bypassData, defaultValue)
  }
}