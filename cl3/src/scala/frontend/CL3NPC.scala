package cl3

import chisel3._
import chisel3.util._

trait NPCConfig {
  val numBTBEntries:       Int = 32
  val numGlobalPHTEntries: Int = 256
  val numRasEntries:       Int = 6
  val numGlobalPHTWidth:   Int = log2Ceil(numGlobalPHTEntries)
  val numRasEntriesWidth:  Int = log2Ceil(numRasEntries)
} //TODO: add require clause

class CL3NPC() extends Module with NPCConfig with CL3Config {
  val io = IO(new NPCFullIO())

  val pc0 = io.info.pc
  val pc1 = pc0 + 4.U // TODO:

  val pc_plus_8 = io.info.pc + 8.U

  if (EnableBP) {

    val btb_entry = Wire(Vec(2, new BTBEntry))
    val btb       = Module(new CL3BTB(numBTBEntries))

    btb.io.rd(0).pc := pc0(31, 2)
    btb.io.rd(1).pc := pc1(31, 2)

    btb.io.wr.wen := io.bp.valid && io.mispred && io.bp.isTaken

    btb.io.wr.pc           := io.bp.source(31, 2)
    btb.io.wr.wdata.target := io.bp.target
    btb.io.wr.wdata.isCall := io.bp.isCall
    btb.io.wr.wdata.isJmp  := io.bp.isJmp
    btb.io.wr.wdata.isRet  := io.bp.isRet

    btb_entry(0) := btb.io.rd(0).rdata
    btb_entry(1) := btb.io.rd(1).rdata

    // --- RAS (Return Address Stack) ---

    val ras_q = RegInit(VecInit(Seq.fill(numRasEntries)(1.U(32.W))))

    // RAS Index (speculative)
    val ras_idx_spe_q = RegInit(0.U(numRasEntriesWidth.W))

    // RAS Index (actually)
    val ras_idx_real_q = RegInit(0.U(numRasEntriesWidth.W))
    when(io.bp.valid && io.bp.isCall) {
      ras_idx_real_q := ras_idx_real_q + 1.U
    }.elsewhen(io.bp.valid && io.bp.isRet) {
      ras_idx_real_q := ras_idx_real_q - 1.U
    }

    val ras_npc = ras_q(ras_idx_spe_q)

    val pc0_is_call = btb.io.rd(0).hit && btb_entry(0).isCall && !ras_npc(0)
    val pc1_is_call = btb.io.rd(1).hit && btb_entry(1).isCall && !ras_npc(0)

    val pc0_is_ret = btb.io.rd(0).hit && btb_entry(0).isRet && !ras_npc(0)
    val pc1_is_ret = btb.io.rd(1).hit && btb_entry(1).isRet && !ras_npc(0)

    val ras_idx = MuxCase(
      ras_idx_spe_q,
      Seq(
        (io.bp.valid && io.bp.isCall)                    -> (ras_idx_real_q + 1.U),
        (io.bp.valid && io.bp.isRet)                     -> (ras_idx_real_q - 1.U),
        ((pc0_is_call || pc1_is_call) && io.info.accept) -> (ras_idx_spe_q + 1.U),
        ((pc0_is_call || pc1_is_call) && io.info.accept) -> (ras_idx_spe_q - 1.U)
      )
    )

    when(io.bp.valid && io.bp.isCall) {
      ras_q(ras_idx) := io.bp.source + 4.U
      ras_idx_spe_q  := ras_idx
    }.elsewhen((pc0_is_call || pc1_is_call) && io.info.accept) {
      ras_q(ras_idx) := Mux(btb.io.rd(1).hit, pc1, pc0) + 4.U
      ras_idx_spe_q  := ras_idx
    }.elsewhen((pc0_is_ret || pc1_is_ret) && io.info.accept || io.bp.valid && io.bp.isRet) {
      ras_idx_spe_q := ras_idx
    }

    // Global history Register (actually), update when branch is resolved
    val ghr_real_q = RegInit(0.U(numGlobalPHTWidth.W))
    when(io.bp.valid && !io.bp.isJmp) {
      ghr_real_q := ghr_real_q(numGlobalPHTWidth - 2, 0) ## io.bp.isTaken
    }

    // Global history Register (speculative)
    val ghr_q      = RegInit(0.U(numGlobalPHTWidth.W))
    val pred_taken = Wire(Bool())

    when(io.mispred && !io.bp.isJmp && io.bp.valid) {
      ghr_q := ghr_real_q(numGlobalPHTWidth - 2, 0) ## io.bp.isTaken
    }.elsewhen((btb.io.rd(0).hit && !btb_entry(0).isJmp) && io.info.accept) {
      ghr_q := ghr_q(numGlobalPHTWidth - 2, 0) ## pred_taken
    }.elsewhen((btb.io.rd(1).hit && !btb_entry(1).isJmp) && io.info.accept) {
      ghr_q := ghr_q(numGlobalPHTWidth - 2, 0) ## pred_taken
    }

    val pht_rd_idx = Wire(Vec(2, UInt(numGlobalPHTWidth.W)))

    pht_rd_idx(0) := pc0(1 + numGlobalPHTWidth, 2)
    pht_rd_idx(1) := pc1(1 + numGlobalPHTWidth, 2)

    val pht_wr_idx = io.bp.source(1 + numGlobalPHTWidth, 2)

    val pht_q = RegInit(VecInit(Seq.fill(numGlobalPHTEntries)(2.U(2.W))))

    val pht_rd_data = Wire(Vec(2, UInt(2.W)))

    pht_rd_data(0) := pht_q(pht_rd_idx(0))
    pht_rd_data(1) := pht_q(pht_rd_idx(1))

    val pht_data = pht_q(pht_wr_idx)

    val pht_is_taken = Wire(Vec(2, Bool()))
    pht_is_taken(0) := pht_rd_data(0) >= 2.U
    pht_is_taken(1) := pht_rd_data(1) >= 2.U

    when(io.bp.valid && io.bp.isTaken && pht_data =/= 3.U) {
      pht_q(pht_wr_idx) := pht_data + 1.U
    }.elsewhen(io.bp.valid && io.bp.isNotTaken && pht_data =/= 0.U) {
      pht_q(pht_wr_idx) := pht_data - 1.U
    }

    val bp_trigger0 = btb.io.rd(0).hit && (pc0_is_ret || pht_is_taken(0) || btb_entry(0).isJmp)
    val bp_trigger1 = btb.io.rd(1).hit && (pc1_is_ret || pht_is_taken(1) || btb_entry(1).isJmp)

    val npc0 = Mux(
      pc0_is_ret,
      ras_npc,
      Mux(btb.io.rd(0).hit && (pht_is_taken(0) || btb_entry(0).isJmp), btb_entry(0).target, pc_plus_8)
    )
    val npc1 = Mux(
      pc1_is_ret,
      ras_npc,
      Mux(btb.io.rd(1).hit && (pht_is_taken(1) || btb_entry(1).isJmp), btb_entry(1).target, pc_plus_8)
    )

    io.info.npc := Mux(bp_trigger0, npc0, Mux(bp_trigger1, npc1, pc_plus_8))

    io.info.taken := Mux(bp_trigger0, 1.U(2.W), Mux(bp_trigger1, 2.U(2.W), 0.U(2.W)))
    pred_taken    := (bp_trigger0 || bp_trigger1) && io.info.accept

  } else {
    io.info.npc   := pc_plus_8
    io.info.taken := 0.U
  }
}
