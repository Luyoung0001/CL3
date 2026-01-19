package cl3
import chisel3._
import chisel3.util._

case class ReplParams(
  nSets: Int,
  nWays: Int,
  lfsrBits: Int = 16
)

object ReplParams {
  def fromICache(p: ICacheParams, lfsrBits: Int = 16): ReplParams =
    ReplParams(nSets = p.numLines, nWays = p.numWays, lfsrBits = lfsrBits)

  def fromDCache(p: DCacheParams, lfsrBits: Int = 16): ReplParams =
    ReplParams(nSets = p.sets, nWays = p.ways, lfsrBits = lfsrBits)
}

sealed trait ReplKind
case object ReplRandom extends ReplKind
case object ReplRR     extends ReplKind
case object ReplPLRU   extends ReplKind
case object ReplLRU    extends ReplKind


object ReplUtil {
  def chooseInvalidFirst(invalidWayOH: UInt, policyVictimOH: UInt): UInt =
    Mux(invalidWayOH.orR, PriorityEncoderOH(invalidWayOH), policyVictimOH)
}


class ReplPortIO(p: ReplParams) extends Bundle {
  val setIdx       = Input(UInt(log2Ceil(p.nSets).W))

  // OH - one hot
  val invalidWayOH = Input(UInt(p.nWays.W))

  val hitValid     = Input(Bool())
  val hitWayOH     = Input(UInt(p.nWays.W))

  val refillValid  = Input(Bool())
  val refillWayOH  = Input(UInt(p.nWays.W))

  val victimWayOH  = Output(UInt(p.nWays.W))
}

class ReplIO(p: ReplParams, nPorts: Int) extends Bundle {
  val ports = Vec(nPorts, new ReplPortIO(p))
}


class Replacement(p: ReplParams, kind: ReplKind, nPorts: Int) extends Module {
  val io = IO(new ReplIO(p, nPorts))

  val policyIO: ReplIO = kind match {
    case ReplRandom => Module(new ReplRandomPolicy(p, nPorts)).io
    case ReplRR     => Module(new ReplRRPolicy(p, nPorts)).io
    case ReplPLRU   => Module(new ReplPLRUPolicy(p, nPorts)).io
    case ReplLRU    => Module(new ReplLRUPolicy(p, nPorts)).io 
  }
  io <> policyIO
}

// Random Replacement Policy
class ReplRandomPolicy(p: ReplParams, nPorts: Int) extends Module {
  val io = IO(new ReplIO(p, nPorts))

  val lfsr    = chisel3.util.random.LFSR(p.lfsrBits)
  val idxBits = log2Ceil(p.nWays)
  val idx     = lfsr(idxBits-1, 0)

  val policyVictimOH = UIntToOH(idx, p.nWays)

  for (i <- 0 until nPorts) {
    io.ports(i).victimWayOH := ReplUtil.chooseInvalidFirst(io.ports(i).invalidWayOH, policyVictimOH)
  }
}

// Round-Robin Replacement Policy
class ReplRRPolicy(p: ReplParams, nPorts: Int) extends Module {
  val io = IO(new ReplIO(p, nPorts))

  val ptrBits = log2Ceil(p.nWays)
  val ptr = RegInit(VecInit(Seq.fill(p.nSets)(0.U(ptrBits.W))))
  val ptr_next = Wire(Vec(p.nSets, UInt(ptrBits.W)))
  ptr_next := ptr
  for (i <- 0 until nPorts) {
    val curPtr = ptr(io.ports(i).setIdx)
    val policyVictimOH = UIntToOH(curPtr, p.nWays)
    io.ports(i).victimWayOH := ReplUtil.chooseInvalidFirst(io.ports(i).invalidWayOH, policyVictimOH)

    when (io.ports(i).refillValid) {
      ptr_next(io.ports(i).setIdx) := curPtr + 1.U
    }
  }

  when (io.ports.map(p => p.refillValid).reduce(_||_)) {
    ptr := ptr_next
  }
}

// Pseudo-LRU Replacement Policy
class ReplPLRUPolicy(p: ReplParams, nPorts: Int) extends Module {
  val io = IO(new ReplIO(p, nPorts))
  require(isPow2(p.nWays), s"PLRU needs pow2 ways, got ${p.nWays}")

  val levels = log2Ceil(p.nWays)
  val metaBits = p.nWays - 1
  val meta = RegInit(VecInit(Seq.fill(p.nSets)(0.U(metaBits.W))))

  def victimIdxFromMeta(m: UInt): UInt = {
    val node = Wire(Vec(levels + 1, UInt(log2Ceil(p.nWays).W)))
    node(0) := 0.U
    val idx  = Wire(Vec(levels + 1, UInt(levels.W)))
    idx(0) := 0.U

    for (level <- 0 until levels) {
      val dir = m(node(level)) // 0=left, 1=right (victim direction)
      val shifted = (idx(level) << 1) | dir.asUInt
      idx(level + 1) := shifted(levels - 1, 0)

      if(level < levels - 1){
        node(level + 1) := (node(level) << 1) + 1.U + dir
      } else {
        node(level + 1) := 0.U // not used
      }
      
    }
    idx(levels)
  }

  def updateMeta(m: UInt, wayIdx: UInt): UInt = {
    val nodeVec = Wire(Vec(levels, UInt(log2Ceil(p.nWays).W)))
    val metaVec = Wire(Vec(levels + 1, UInt(metaBits.W)))

    nodeVec(0) := 0.U
    metaVec(0) := m

    for (level <- 0 until levels) {
      val dir = wayIdx(levels - 1 - level)
      val mask = (1.U(metaBits.W) << nodeVec(level))
      val cleared = metaVec(level) & ~mask
      val insert  = Mux((~dir).asBool, mask, 0.U(metaBits.W))
      metaVec(level + 1) := cleared | insert

      if (level < levels - 1) {
        nodeVec(level + 1) := (nodeVec(level) << 1) + 1.U + dir.asUInt
      }
    }
    metaVec(levels)
  }
  
  val metaNext = Wire(Vec(p.nSets, UInt(metaBits.W)))
  metaNext := meta

  for (i <- 0 until nPorts) {
    val curMeta = meta(io.ports(i).setIdx)
    val victimIdx = victimIdxFromMeta(curMeta)
    val policyVictimOH = UIntToOH(victimIdx, p.nWays)
    io.ports(i).victimWayOH := ReplUtil.chooseInvalidFirst(io.ports(i).invalidWayOH, policyVictimOH)

    val doUpdate = io.ports(i).hitValid || io.ports(i).refillValid
    val updWayOH = Mux(io.ports(i).refillValid, io.ports(i).refillWayOH, io.ports(i).hitWayOH)
    val updWayIdx = OHToUInt(updWayOH)

    when (doUpdate) {
      metaNext(io.ports(i).setIdx) := updateMeta(curMeta, updWayIdx)
    }
  }
  when (io.ports.map(p => p.hitValid || p.refillValid).reduce(_||_)) {
    meta := metaNext
  }
}

// LRU Replacement Policy
class ReplLRUPolicy(p: ReplParams, nPorts: Int) extends Module {
  val io = IO(new ReplIO(p, nPorts))

  val ageW = log2Ceil(p.nWays)
  val ages = RegInit(VecInit(Seq.fill(p.nSets)(VecInit(Seq.fill(p.nWays)(0.U(ageW.W)))))) // sets * ways * way bits

  def updatedAges(cur: Vec[UInt], updWayIdx: UInt): Vec[UInt] = {
    val next = Wire(Vec(p.nWays, UInt(ageW.W)))
    val updAge = cur(updWayIdx)

    for (w <- 0 until p.nWays) {
      when (w.U === updWayIdx) {
        next(w) := (p.nWays - 1).U  // MRU
      }.elsewhen (cur(w) > updAge) {
        next(w) := cur(w) - 1.U
      }.otherwise {
        next(w) := cur(w)
      }
    }
    next
  }

  def victimIdxFromAges(age: Vec[UInt]): UInt = {
    val minAge = age.reduce((a,b) => Mux(a < b, a, b))
    PriorityEncoder(VecInit(age.map(_ === minAge)).asUInt)
  }

  val agesNext = Wire(Vec(p.nSets, Vec(p.nWays, UInt(ageW.W))))
  agesNext := ages
  
  for (i <- 0 until nPorts) {
    val s         = io.ports(i).setIdx
    val doUpdate  = io.ports(i).hitValid || io.ports(i).refillValid
    val updWayOH  = Mux(io.ports(i).refillValid, io.ports(i).refillWayOH, io.ports(i).hitWayOH)
    val victimIdx = victimIdxFromAges(ages(s))
    val policyVictimOH = UIntToOH(victimIdx, p.nWays)
    io.ports(i).victimWayOH := ReplUtil.chooseInvalidFirst(io.ports(i).invalidWayOH, policyVictimOH)
    when (doUpdate) {
      agesNext(s) := updatedAges(ages(s), OHToUInt(updWayOH))
    }
  }
  when (io.ports.map(p => p.hitValid || p.refillValid).reduce(_||_)) {
    ages := agesNext
  }
}