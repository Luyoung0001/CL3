package cl3

import chisel3._
import chisel3.util._

// CPU <-> DCache
class MemReq(p: DCacheParams) extends Bundle {
  val addr       = UInt(p.addrW.W)
  val dataWr     = UInt(p.dataW.W)
  val rd         = Bool()
  val wr         = UInt(p.wstrbW.W)
  val cacheable  = Bool()
  val reqTag     = UInt(p.reqTagW.W)
  val invalidate = Bool()
  val writeback  = Bool()
  val flush      = Bool()
}

class MemResp(p: DCacheParams) extends Bundle {
  val dataRd  = UInt(p.dataW.W)
  val ack     = Bool()
  val error   = Bool()
  val respTag = UInt(p.reqTagW.W)
}

class CpuMemPort(p: DCacheParams) extends Bundle {
  val req    = Input(new MemReq(p))
  val resp   = Output(new MemResp(p))
  val accept = Output(Bool())
}

// DCache <-> PMEM
class PMemReq(p: DCacheParams) extends Bundle {
  val wr    = Bool()
  val valid = Bool()
  val len   = UInt(p.axi.axiLenBits.W)
  val addr  = UInt(p.addrW.W)
  val id    = UInt(p.axi.axiIdBits.W)
  val burst = UInt(p.axi.axiBurstBits.W)

}

class PMemReqAxi(p: DCacheParams) extends Bundle {
  val wr        = UInt(p.wstrbW.W)
  val rd        = Bool()
  val len       = UInt(p.axi.axiLenBits.W)
  val addr      = UInt(p.addrW.W)
  val writeData = UInt(p.dataW.W)

}

class PMemResp(p: DCacheParams) extends Bundle {
  val accept   = Bool()
  val ack      = Bool()
  val error    = Bool()
  val readData = UInt(p.dataW.W)
}

class PMemPortAxi(p: DCacheParams) extends Bundle {
  val req  = Input(new PMemReqAxi(p))
  val resp = Output(new PMemResp(p))
}

class PMemPortAxi2(p: DCacheParams) extends Bundle {
  val req  = Input(new PMemReq(p))
  val resp = Output(new PMemResp(p))
}

// DCache_Mux, Cached/UnCached
class TwoMemBacks(p: DCacheParams) extends Bundle {
  val cached   = new Bundle {
    val req    = Input(new MemReq(p))
    val resp   = Output(new MemResp(p))
    val accept = Output(Bool())
  }
  val uncached = new Bundle {
    val req    = Input(new MemReq(p))
    val resp   = Output(new MemResp(p))
    val accept = Output(Bool())
  }
}

class DCacheMuxIO(p: DCacheParams) extends Bundle {
  val cpu         = new CpuMemPort(p)
  val backs       = Flipped(new TwoMemBacks(p))
  val cacheActive = Output(Bool())
}

class DCachePmemMuxIO(p: DCacheParams) extends Bundle {
  val outport  = Flipped(new PMemPortAxi(p))
  val in0port  = new PMemPortAxi(p)
  val in1port  = new PMemPortAxi(p)
  val select_i = Input(Bool())
}

// DCache Core
class DCacheCoreIO(p: DCacheParams) extends Bundle {
  val cpu    = new CpuMemPort(p)
  val pmem   = Flipped(new PMemPortAxi(p))
  val atomic = Input(new AtomicInfo())
}

// DCache_if_pmem
class DCacheIfPMemIO(p: DCacheParams) extends Bundle {
  val cpu  = new CpuMemPort(p)
  val pmem = Flipped(new PMemPortAxi(p))
}

// Tag RAM
class TagRamPort0(p: DCacheParams)    extends Bundle {
  val addr  = Input(UInt(p.tagIdxW.W))
  val rdata = Output(UInt((p.tagEntryW - 1).W))
}
class TagRamPort1(p: DCacheParams)    extends Bundle {
  val addr  = Input(UInt(p.tagIdxW.W))
  val wdata = Input(UInt((p.tagEntryW - 1).W))
  val wen   = Input(Bool())
}
class DCacheTagRamIO(p: DCacheParams) extends Bundle {
  val p0 = new TagRamPort0(p)
  val p1 = new TagRamPort1(p)
}

// Dirty RAM (valid/dirty bits split version)
class DirtyRamPort0(p: DCacheParams) extends Bundle {
  val addr  = Input(UInt(p.tagIdxW.W))
  val rdata = Output(Bool())
}
class DirtyRamPort1(p: DCacheParams) extends Bundle {
  val addr  = Input(UInt(p.tagIdxW.W))
  val wdata = Input(Bool())
  val wen   = Input(Bool())
}
class DCacheDirtyRamIO(p: DCacheParams) extends Bundle {
  val p0 = new DirtyRamPort0(p)
  val p1 = new DirtyRamPort1(p)
}

// Data RAM
class DataRamRWPort(p: DCacheParams)   extends Bundle {
  val addr  = Input(UInt(p.dataIdxW.W))
  val wdata = Input(UInt(p.dataW.W))
  val wstrb = Input(UInt(p.wstrbW.W))
  val rdata = Output(UInt(p.dataW.W))
}
class DCacheDataRamIO(p: DCacheParams) extends Bundle {
  val p0 = new DataRamRWPort(p)
  val p1 = new DataRamRWPort(p)
}

class DCacheAxiIO(p: DCacheParams) extends Bundle {
  val pmem = new PMemPortAxi(p)
  val axi  = new Axi4MasterIO(p.axi)
}

class DCacheAxitoAXiIO(p: DCacheParams) extends Bundle {
  val pmem    = new PMemPortAxi2(p)
  val out_axi = new Axi4MasterIO(p.axi)
  val in_axi  = new Axi4SlaveIO(p.axi)
}

class DCacheTopIO(p: DCacheParams) extends Bundle {
  val cpu    = new CpuMemPort(p)
  val axi    = new Axi4MasterIO(p.axi)
  val atomic = Input(new AtomicInfo())
}

class DCacheAxiFifoIO(WIDTH: Int, DEPTH: Int, ADDR_W: Int) extends Bundle {
  val in_data = Input(UInt(WIDTH.W))
  val push_i  = Input(Bool())
  val pop_i   = Input(Bool())

  val out_data = Output(UInt(WIDTH.W))
  val accept_o = Output(Bool())
  val valid_o  = Output(Bool())
}

object amoOp extends ChiselEnum {
  val AMONONE = Value("b0000".U)
  val AMOSWAP = Value("b0010".U)
  val AMOADD  = Value("b0011".U)
  val AMOXOR  = Value("b0100".U)
  val AMOAND  = Value("b0101".U)
  val AMOOR   = Value("b0110".U)
  val AMOMIN  = Value("b0111".U)
  val AMOMAX  = Value("b1000".U)
  val AMOMINU = Value("b1001".U)
  val AMOMAXU = Value("b1010".U)
}

class AtomicInfo extends Bundle {
  val isAmo     = Bool()
  val isLR      = Bool()
  val isSC      = Bool()
  val amoCode   = amoOp()
  // val aq    = Input(Bool()) // ooo
  // val rl    = Input(Bool())
}

case class WB(p: DCacheParams) {
  val valid = RegInit(false.B)
  val row   = Reg(UInt((p.dataIdxW - p.bankSelBits).W))
  val data  = Reg(UInt(p.dataW.W))
  val mask  = Reg(UInt(p.dataW.W))
}
