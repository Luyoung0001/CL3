package cl3

import chisel3._
import chisel3.util._

class dcache_core(p: DCacheParams) extends Module with CL3Config {
  val io = IO(new DCacheCoreIO(p))

  val (cpureq, cpuresp, pmemreq, pmemresp) = (io.cpu.req, io.cpu.resp, io.pmem.req, io.pmem.resp)
  val atomic = io.atomic

  // val states = Enum(12)
  import DCacheState._
  val state_q = RegInit(STATE_RESET)
  val next_state_r = WireDefault(state_q)
//-----------------------------------------------------------------
// Request buffer
//-----------------------------------------------------------------
  val memReq_m_q  = RegInit(0.U.asTypeOf(new MemReq(p)))
  val atomic_m_q  = RegInit(0.U.asTypeOf(new AtomicInfo))
  
  when(io.cpu.accept && cpureq.rd) {
    memReq_m_q := cpureq
  } .elsewhen(cpuresp.ack) {
    memReq_m_q := 0.U.asTypeOf(new MemReq(p))
  }

  val mem_accept_r    = WireDefault(false.B)
  val tag_hit_any_m_w = Wire(Bool())
  when(state_q === STATE_LOOKUP){
    // Previous access missed - do not accept new requests
    when ((memReq_m_q.rd || atomic_m_q.isAmo || (memReq_m_q.wr =/= 0.U)) && !tag_hit_any_m_w){
      mem_accept_r := false.B
    // Write followed by read - detect writes to the same bank, or addresses which alias in tag lookups
    } .elsewhen((memReq_m_q.wr.orR) && cpureq.rd && (cpureq.addr(3, 2) === memReq_m_q.addr(3, 2)) && memReq_m_q.addr(31, 28) =/= 0.U){
      mem_accept_r := false.B
    } .otherwise{
      mem_accept_r := true.B
    }
  }

  io.cpu.accept := mem_accept_r

// Tag comparison address
  val req_addr_tag_cmp_m_w = memReq_m_q.addr(p.tagCmpAddrH, p.tagCmpAddrL)

  cpuresp.respTag := memReq_m_q.reqTag

//-----------------------------------------------------------------
// Registers / Wires
//-----------------------------------------------------------------
  val replace_way_q = RegInit(0.U(p.wayBits.W))

  val pmem_last_w       = Wire(Bool()) 
  val pmem_req_w  = WireDefault(0.U.asTypeOf(new PMemReqAxi(p)))
  val pmem_resp_w = WireDefault(0.U.asTypeOf(new PMemResp(p)))

  val evict_way_w           = Wire(Bool())
  val flushing_q = RegInit(false.B)

  val flush_addr_q = RegInit(0.U(p.tagReqLineW.W))

  val tag_addr_x_r = Wire(UInt(p.tagReqLineW.W))
  val tag_addr_m_r = Wire(UInt(p.tagReqLineW.W))

  val data_r  = WireDefault(0.U(32.W))
  val scSuccess = WireDefault(false.B)
  val atoData   = RegInit(0.U(p.dataW.W)) // amo
  val EVICT_ADDR_W = 32 - p.offBits
  val evict_addr_r = Wire(UInt(EVICT_ADDR_W.W))
  val evict_request_w = Wire(Bool())

  val atoOn: Bool = if (EnableAtomic) true.B else false.B

  if(EnableAtomic){
    when(io.cpu.accept && cpureq.rd){
      atomic_m_q := atomic
    } .elsewhen(cpuresp.ack){
      atomic_m_q := 0.U.asTypeOf(new AtomicInfo)
    }

  //-----------------------------------------------------------------
  // Atomic calculate
  //-----------------------------------------------------------------
  // memReq_m_q.dataWr : rs2
    atoData := MuxLookup(atomic_m_q.amoCode, 0.U(p.dataW.W))(Seq(
      amoOp.AMOADD   -> (memReq_m_q.dataWr + data_r),
      amoOp.AMOAND   -> (memReq_m_q.dataWr & data_r),
      amoOp.AMOMAX   -> Mux(memReq_m_q.dataWr.asSInt > data_r.asSInt, memReq_m_q.dataWr, data_r),
      amoOp.AMOMIN   -> Mux(memReq_m_q.dataWr.asSInt < data_r.asSInt, memReq_m_q.dataWr, data_r),
      amoOp.AMOMAXU  -> Mux(memReq_m_q.dataWr > data_r, memReq_m_q.dataWr, data_r),
      amoOp.AMOMINU  -> Mux(memReq_m_q.dataWr < data_r, memReq_m_q.dataWr, data_r),
      amoOp.AMOOR    -> (memReq_m_q.dataWr | data_r),
      amoOp.AMOXOR   -> (memReq_m_q.dataWr ^ data_r),
      amoOp.AMOSWAP  -> memReq_m_q.dataWr
    ))
  //-----------------------------------------------------------------
  // Reservation RAM
  //-----------------------------------------------------------------
    val reservedValid = RegInit(false.B)
    val reservedAddr  = RegInit(0.U((p.dataW - 2).W))
    reservedValid := Mux(atomic_m_q.isLR, true.B, 
                        Mux(atomic_m_q.isSC, false.B, reservedValid))
    when((reservedAddr(29, 3) === evict_addr_r) && evict_request_w){
      reservedValid := false.B
    }
    reservedAddr  := Mux(atomic_m_q.isLR, cpureq.addr(p.dataW - 1, 2), reservedAddr)

  //-----------------------------------------------------------------
  // Store Condition (SC.W) 
  //-----------------------------------------------------------------
    scSuccess := atomic_m_q.isSC && reservedValid && reservedAddr === cpureq.addr(p.dataW - 1, 2)
  }

  // Read Port
  tag_addr_x_r := cpureq.addr(p.tagReqLineH, p.tagReqLineL)

  // Lookup
  when (state_q === STATE_LOOKUP && (next_state_r === STATE_LOOKUP || next_state_r === STATE_WRITEBACK || ((next_state_r === STATE_AMOCOMPUTE) && atoOn))){
    tag_addr_x_r := cpureq.addr(p.tagReqLineH, p.tagReqLineL)
  // Cache flush
  } .elsewhen (flushing_q){
    tag_addr_x_r := flush_addr_q
  } .otherwise {
    tag_addr_x_r := memReq_m_q.addr(p.tagReqLineH, p.tagReqLineL)
  }

  // Write Port
  tag_addr_m_r := flush_addr_q

  // Cache flush
  when(flushing_q || state_q === STATE_RESET){
    tag_addr_m_r := flush_addr_q
  } .otherwise{
    tag_addr_m_r := memReq_m_q.addr(p.tagReqLineH, p.tagReqLineL)
  }

  // Tag RAM write data
  val tag_data_in_m_r = WireDefault(0.U(p.tagEntryW.W))

  // Cache flush
  when(state_q === STATE_FLUSH || state_q === STATE_RESET || flushing_q) {
    tag_data_in_m_r := 0.U
  // Line refill
  } .elsewhen(state_q === STATE_REFILL){
    tag_data_in_m_r := Cat(0.U(1.W), 1.U(1.W), memReq_m_q.addr(p.tagCmpAddrH, p.tagCmpAddrL))
  // Invalidate - mark entry (if matching line) not valid (even if dirty...)
  } .elsewhen(state_q === STATE_INVALIDATE){
    tag_data_in_m_r := Cat(0.U(1.W), 0.U(1.W), memReq_m_q.addr(p.tagCmpAddrH, p.tagCmpAddrL))
  // Evict completion
  } .elsewhen(state_q === STATE_EVICT_WAIT){
    tag_data_in_m_r := Cat(0.U(1.W), 1.U(1.W), memReq_m_q.addr(p.tagCmpAddrH, p.tagCmpAddrL))
  // Write - mark entry as dirty
  } .elsewhen(state_q === STATE_WRITE || (state_q === STATE_LOOKUP && ((memReq_m_q.wr.orR) || (((atomic_m_q.isAmo || (atomic_m_q.isSC && scSuccess))) && atoOn) ))){
    tag_data_in_m_r := Cat(1.U(1.W), 1.U(1.W), memReq_m_q.addr(p.tagCmpAddrH, p.tagCmpAddrL))
  }
  // Tag RAM write enable

  val tag_data_out_w = Wire(Vec(p.ways, UInt(p.tagEntryW.W)))
  dontTouch(tag_data_out_w)
  class TagMeta(p: DCacheParams) extends Bundle {
    val valid   = Bool()
    val dirty   = Bool()
    val tagBits = UInt(p.tagBits.W)
  }
  val nWays = p.ways

  val tagMeta_m_w = VecInit((0 until nWays).map { w =>
    val m = Wire(new TagMeta(p))
    m.valid   := tag_data_out_w(w)(p.tagValidBit)
    m.dirty   := tag_data_out_w(w)(p.tagDirtyBit)
    m.tagBits := tag_data_out_w(w)(p.tagBits - 1, 0)
    m
  })

  val tag_valid_m_w     = VecInit(tagMeta_m_w.map(_.valid))
  val tag_dirty_m_w     = VecInit(tagMeta_m_w.map(_.dirty))
  val tag_addr_bits_m_w = VecInit(tagMeta_m_w.map(_.tagBits))

  val tag_hit_m_w: Vec[Bool] = VecInit((0 until nWays).map { w =>
    tag_valid_m_w(w) && (tag_addr_bits_m_w(w) === req_addr_tag_cmp_m_w)
  })

  tag_hit_any_m_w := tag_hit_m_w.reduce(_||_)

  val tag_dirty_any_m_w = (0 until nWays).map { w =>
    tag_valid_m_w(w) && tag_dirty_m_w(w)
  }.reduce(_||_)

  val tag_hit_and_dirty_m_w = (0 until nWays).map { w =>
    tag_hit_m_w(w) && tag_dirty_m_w(w)
  }.reduce(_||_)
  
  val u_tag   = Seq.fill(nWays)(Module(new dcache_core_tag_ram(p)))
  val u_dirty = Seq.fill(nWays)(Module(new dcache_core_dirty_ramDev(p)))
  
  for (i <- 0 until nWays) {
    val tag_write_m_r   = WireDefault(false.B)
    val dirty_write_m_r = Wire(Bool())

    // Cache flush (reset)
    when (state_q === STATE_RESET) {
      tag_write_m_r := true.B
    }
    // Cache flush
    .elsewhen (state_q === STATE_FLUSH) {
      tag_write_m_r := !tag_dirty_any_m_w
    }
    // Write - hit, mark as dirty
    // .elsewhen (state_q === STATE_LOOKUP && memReq_m_q.wr.orR) {
    //     tag_write_m_r := tag_hit_m_w(i)
    // }
    // Write - write after refill
    .elsewhen (state_q === STATE_WRITE) {
      tag_write_m_r := (replace_way_q === i.U)
    }
    // Write - mark entry as dirty
    .elsewhen (state_q === STATE_EVICT_WAIT && pmem_resp_w.ack) {
      tag_write_m_r := (replace_way_q === i.U)
    }
    // Line refill
    .elsewhen (state_q === STATE_REFILL) {
      tag_write_m_r := (pmem_resp_w.ack && pmem_last_w && (replace_way_q === i.U))
    }
    // Invalidate - line matches address - invalidate
    .elsewhen (state_q === STATE_INVALIDATE) {
      tag_write_m_r := tag_hit_m_w(i)
    }

    // Dirty bit write can be broader: any tag write, plus hit-writes during lookup/AMO.
    dirty_write_m_r := tag_write_m_r
    when (state_q === STATE_LOOKUP && ((memReq_m_q.wr.orR) || (((atomic_m_q.isAmo || (atomic_m_q.isSC && scSuccess)) && atoOn)))) {
      dirty_write_m_r := tag_write_m_r || tag_hit_m_w(i)
    }
    u_tag(i).io.p0.addr := tag_addr_x_r
    tag_data_out_w(i)   := Cat(u_dirty(i).io.p0.rdata, u_tag(i).io.p0.rdata)
    u_tag(i).io.p1.addr := tag_addr_m_r
    u_tag(i).io.p1.wdata := tag_data_in_m_r(p.tagDirtyBit - 1, 0)
    u_tag(i).io.p1.wen := tag_write_m_r

    u_dirty(i).io.p1.addr := tag_addr_m_r
    u_dirty(i).io.p1.wdata := tag_data_in_m_r(p.tagDirtyBit)
    u_dirty(i).io.p1.wen := dirty_write_m_r
    u_dirty(i).io.p0.addr := tag_addr_x_r
  }

  val data_out_m_w = Wire(Vec(nWays, UInt(32.W)))

  val selWay = replace_way_q

  val evict_way_r  = tag_valid_m_w(selWay) && tag_dirty_m_w(selWay)
  val evict_data_r = data_out_m_w(selWay)
  evict_addr_r := Mux(
      flushing_q,
      Cat(tag_valid_m_w(selWay), flush_addr_q),
      Cat(tag_addr_bits_m_w(selWay), memReq_m_q.addr(p.tagReqLineH, p.tagReqLineL))
  )

  evict_way_w  := (flushing_q || !tag_hit_any_m_w) && evict_way_r

//-----------------------------------------------------------------
// DATA RAMS
//-----------------------------------------------------------------
// Data addressing   

  val CACHE_DATA_ADDR_W = p.offBits + p.idxBits - 2
  
  val data_addr_x_r    = Wire(UInt(CACHE_DATA_ADDR_W.W))
  val data_addr_m_r    = Wire(UInt(CACHE_DATA_ADDR_W.W))

  val data_write_addr_q = RegInit(0.U(CACHE_DATA_ADDR_W.W))
  // Data RAM refill write address
  when (state_q =/= STATE_REFILL && next_state_r === STATE_REFILL) {
      data_write_addr_q := pmem_req_w.addr(CACHE_DATA_ADDR_W + 1, 2)
  }.elsewhen (state_q =/= STATE_EVICT && next_state_r === STATE_EVICT) { // todo : for 64 bits data
      data_write_addr_q := data_addr_m_r + 1.U
  }.elsewhen (state_q === STATE_REFILL && pmem_resp_w.ack) { // todo : for 64 bits data
      data_write_addr_q := data_write_addr_q + 1.U
  }.elsewhen (state_q === STATE_EVICT && pmem_resp_w.accept) { // todo : for 64 bits data
      data_write_addr_q := data_write_addr_q + 1.U
  }

// -------------------------------
// // Data RAM address
  data_addr_x_r := cpureq.addr(CACHE_DATA_ADDR_W + 1, 2)
  data_addr_m_r := memReq_m_q.addr(CACHE_DATA_ADDR_W + 1, 2)

  // Line refill / evict
  when (state_q === STATE_REFILL || state_q === STATE_EVICT) {
    data_addr_x_r := data_write_addr_q
    data_addr_m_r := data_addr_x_r
  }.elsewhen (state_q === STATE_FLUSH || state_q === STATE_RESET) {
    data_addr_x_r := Cat(flush_addr_q, 0.U((p.offBits - 2).W))
    data_addr_m_r := data_addr_x_r
  }.elsewhen (state_q =/= STATE_EVICT && next_state_r === STATE_EVICT) {
    data_addr_x_r := Cat(memReq_m_q.addr(p.tagReqLineH, p.tagReqLineL), 0.U((p.offBits - 2).W))
    data_addr_m_r := data_addr_x_r
  }
  // Lookup post refill
  .elsewhen (state_q === STATE_READ || ((state_q === STATE_AMOCOMPUTE) && atoOn)) {
    data_addr_x_r := memReq_m_q.addr(CACHE_DATA_ADDR_W + 1, 2)
  }
  // Possible line update on write
  .otherwise {
    data_addr_m_r := memReq_m_q.addr(CACHE_DATA_ADDR_W + 1, 2)
  }
  // -------------------------------
// Data RAM write enable
  val u_data = Seq.fill(nWays)(Module(new dcache_core_data_ram(p)))
  for (i <- 0 until nWays){
    val data_write_m_r = WireDefault(0.U(4.W))
    when (state_q === STATE_REFILL) {
      data_write_m_r := Mux(pmem_resp_w.ack && (replace_way_q === i.U), "b1111".U(4.W), "b0000".U(4.W)) // todo : 64 bits data
    } .elsewhen (state_q === STATE_WRITE || state_q === STATE_LOOKUP) {
      data_write_m_r := Mux((atomic_m_q.isAmo || (atomic_m_q.isSC && scSuccess)) && atoOn, "b1111".U(4.W), memReq_m_q.wr) & Fill(4, tag_hit_m_w(i))
    }

    val data_in_m_w =  Mux(state_q === STATE_REFILL, pmem_resp_w.readData, 
                        Mux(atomic_m_q.isAmo && atoOn, atoData, memReq_m_q.dataWr))
    u_data(i).io.p0.addr  := data_addr_x_r
    u_data(i).io.p0.wdata := 0.U(32.W)
    u_data(i).io.p0.wstrb := 0.U(4.W)
    u_data(i).io.p1.addr  := data_addr_m_r
    u_data(i).io.p1.wdata := data_in_m_w
    u_data(i).io.p1.wstrb := data_write_m_r
    data_out_m_w(i) := u_data(i).io.p0.rdata

  }

//-----------------------------------------------------------------
// Flush counter
//-----------------------------------------------------------------
  when((state_q === STATE_RESET) || (state_q === STATE_FLUSH && next_state_r === STATE_FLUSH_ADDR)) {
    flush_addr_q := flush_addr_q + 1.U
  }.elsewhen(state_q === STATE_LOOKUP) {
    flush_addr_q := 0.U
  }

  when(state_q === STATE_LOOKUP && next_state_r === STATE_FLUSH_ADDR) {
    flushing_q := true.B
  }.elsewhen(state_q === STATE_FLUSH && next_state_r === STATE_LOOKUP) {
    flushing_q := false.B
  }

  val flush_last_q = RegInit(false.B)

  when(state_q === STATE_LOOKUP) {
    flush_last_q := false.B
  }.elsewhen(flush_addr_q === Fill(p.idxBits, 1.U(1.W))) {
    flush_last_q := true.B
  }

//-----------------------------------------------------------------
// Replacement Policy
//----------------------------------------------------------------- 
// Using random replacement policy - this way we cycle through the ways
// when needing to replace a line.
  val replP = ReplParams.fromDCache(p)
  val repl  = Module(new Replacement(replP, ReplPLRU, 1))
  val refillValidReg   =  RegNext(state_q === STATE_WRITE || state_q === STATE_READ, false.B)
  repl.io.ports(0).setIdx       := memReq_m_q.addr(p.tagReqLineH, p.tagReqLineL)
  repl.io.ports(0).invalidWayOH := ~(tag_valid_m_w.asUInt)
  repl.io.ports(0).hitValid     := tag_hit_any_m_w && cpuresp.ack && !refillValidReg
  repl.io.ports(0).hitWayOH     := tag_hit_m_w.asUInt
  repl.io.ports(0).refillValid  := state_q === STATE_WRITE || state_q === STATE_READ 
  repl.io.ports(0).refillWayOH  := repl.io.ports(0).victimWayOH

  when(state_q === STATE_WRITE || state_q === STATE_READ) {
    replace_way_q := OHToUInt(repl.io.ports(0).victimWayOH)
    // replace_way_q := replace_way_q + 1.U

  }.elsewhen(flushing_q && tag_dirty_any_m_w && !evict_way_w && state_q =/= STATE_FLUSH_ADDR) {
    replace_way_q := replace_way_q + 1.U
  }.elsewhen(state_q === STATE_EVICT_WAIT && next_state_r === STATE_FLUSH_ADDR) {
    replace_way_q := 0.U
  }.elsewhen(state_q === STATE_FLUSH && next_state_r === STATE_LOOKUP) {
    replace_way_q := 0.U
  }.elsewhen(state_q === STATE_LOOKUP && next_state_r === STATE_FLUSH_ADDR) {
    replace_way_q := 0.U
  }.elsewhen(state_q === STATE_WRITEBACK) {
    when (tag_hit_any_m_w) {
      replace_way_q := PriorityEncoder(tag_hit_m_w)
    }
  }

//-----------------------------------------------------------------
// Output Result
//-----------------------------------------------------------------
// Data output mux

  when(tag_hit_any_m_w) {
    data_r := Mux1H(tag_hit_m_w, data_out_m_w)
  }

  cpuresp.dataRd := Mux(atomic_m_q.isSC && atoOn, !scSuccess, data_r)

//-----------------------------------------------------------------
// Next State Logic
//-----------------------------------------------------------------
  switch(state_q) {
//-----------------------------------------
// STATE_RESET
//-----------------------------------------
    is(STATE_RESET) {
      // Final line checked
      when(flush_last_q) {
        next_state_r := STATE_LOOKUP
      }
    }
//-----------------------------------------
// STATE_FLUSH_ADDR
//-----------------------------------------
    is(STATE_FLUSH_ADDR) {
      next_state_r := STATE_FLUSH
    }
//-----------------------------------------
// STATE_FLUSH
//-----------------------------------------
    is(STATE_FLUSH) {
        // Dirty line detected - evict unless initial cache reset cycle
      when(tag_dirty_any_m_w) {
        // Evict dirty line - else wait for dirty way to be selected
        when(evict_way_w) {
          next_state_r := STATE_EVICT
        }
      // Final line checked, nothing dirty
      }.elsewhen(flush_last_q) {
        next_state_r := STATE_LOOKUP
      }.otherwise {
        next_state_r := STATE_FLUSH_ADDR
      }
    }
//-----------------------------------------
// STATE_LOOKUP
//-----------------------------------------
    is(STATE_LOOKUP) {
      // Previous access missed in the cache
      when((memReq_m_q.rd || (memReq_m_q.wr =/= 0.U)) && !tag_hit_any_m_w && !(atomic_m_q.isSC && atoOn)) {
        // Evict dirty line first
        when(evict_way_w) {
          next_state_r := STATE_EVICT
        // Allocate line and fill
        }.otherwise {
          next_state_r := STATE_REFILL
        }
      // Writeback a single line
      }.elsewhen(cpureq.writeback && io.cpu.accept) {
        next_state_r := STATE_WRITEBACK
      // Flush whole cache
      }.elsewhen(cpureq.flush && io.cpu.accept) {
        next_state_r := STATE_FLUSH_ADDR
      // Invalidate line (even if dirty)
      }.elsewhen(cpureq.invalidate && io.cpu.accept) {
        next_state_r := STATE_INVALIDATE
      // Atomic operation
      }.elsewhen(atomic.isAmo && io.cpu.accept && atoOn) {
        next_state_r := STATE_AMOCOMPUTE
      }
    }
//-----------------------------------------
// STATE_REFILL
//-----------------------------------------
    is(STATE_REFILL) {
      // End of refill
      when(pmem_resp_w.ack && pmem_last_w) {
        // Refill reason was write
        when(memReq_m_q.wr =/= 0.U) {
          next_state_r := STATE_WRITE
        // Refill reason was read
        }.otherwise {
          next_state_r := STATE_READ
        }
      }
    }
//-----------------------------------------
// STATE_WRITE/READ/AMO
//-----------------------------------------
    is(STATE_WRITE, STATE_AMOCOMPUTE) {
      next_state_r := STATE_LOOKUP
    }
    is(STATE_READ) {
      next_state_r := Mux(atomic_m_q.isAmo && atoOn, STATE_AMOCOMPUTE, STATE_LOOKUP)
    }
//-----------------------------------------
// STATE_EVICT
//-----------------------------------------
    is(STATE_EVICT) {
      // End of evict, wait for write completion
      when(pmem_resp_w.accept && pmem_last_w) {
        next_state_r := STATE_EVICT_WAIT
      }
    }
//-----------------------------------------
// STATE_EVICT_WAIT
//-----------------------------------------
    is(STATE_EVICT_WAIT) {
      // Single line writeback
      when(pmem_resp_w.ack && memReq_m_q.writeback) {
        next_state_r := STATE_LOOKUP
      // Evict due to flush
      }.elsewhen(pmem_resp_w.ack && flushing_q) {
        next_state_r := STATE_FLUSH_ADDR
      // Write ack, start re-fill now
      }.elsewhen(pmem_resp_w.ack) {
        next_state_r := STATE_REFILL
      }
    }
// -----------------------------------------
// STATE_WRITEBACK: Writeback a cache line
// -----------------------------------------
    is(STATE_WRITEBACK) {
      // Line is dirty - write back to memory
      when(tag_hit_and_dirty_m_w) {
        next_state_r := STATE_EVICT
      // Line not dirty, carry on
      }.otherwise {
        next_state_r := STATE_LOOKUP
      }
    }
// -----------------------------------------
// STATE_INVALIDATE: Invalidate a cache line
// -----------------------------------------
    is(STATE_INVALIDATE) {
      next_state_r := STATE_LOOKUP
    }
  }

// Update state
  state_q := next_state_r

  val mem_ack_r = WireDefault(false.B)

  when(state_q === STATE_LOOKUP) {
    // Normal hit - read or write
    when((memReq_m_q.rd || (memReq_m_q.wr =/= 0.U)) && tag_hit_any_m_w) {
      mem_ack_r := true.B
    }
    // Flush, invalidate, writeback, atomic
    .elsewhen(memReq_m_q.flush || memReq_m_q.invalidate || memReq_m_q.writeback || (atomic_m_q.isSC && atoOn)) {
      mem_ack_r := true.B
    }
  }

  cpuresp.ack := mem_ack_r

//-----------------------------------------------------------------
// AXI Request
//-----------------------------------------------------------------

  val pmem_rd_q  = RegInit(false.B)
  val pmem_wr0_q = RegInit(false.B)
  val pmem_len_q = RegInit(0.U(p.axi.axiLenBits.W))
  val pmem_addr_q = RegInit(0.U(32.W))

  pmem_rd_q := Mux(pmem_req_w.rd, !pmem_resp_w.accept, pmem_rd_q)

  when(state_q =/= STATE_EVICT && next_state_r === STATE_EVICT) {
    pmem_wr0_q := true.B
  }.elsewhen(pmem_resp_w.accept) {
    pmem_wr0_q := false.B
  }

  
  when(state_q =/= STATE_EVICT && next_state_r === STATE_EVICT) {
    pmem_len_q := 7.U
  }.elsewhen(pmem_req_w.rd && pmem_resp_w.accept) {
    pmem_len_q := pmem_req_w.len
  }.elsewhen(state_q === STATE_REFILL && pmem_resp_w.ack) {
    pmem_len_q := pmem_len_q - 1.U
  }.elsewhen(state_q === STATE_EVICT && pmem_resp_w.accept) {
    pmem_len_q := pmem_len_q - 1.U
  }

  pmem_last_w := (pmem_len_q === 0.U)

  when((pmem_req_w.len.orR) && pmem_resp_w.accept) {
    pmem_addr_q := pmem_req_w.addr + 4.U
  }.elsewhen(pmem_resp_w.accept) {
    pmem_addr_q := pmem_addr_q + 4.U
  }

//-----------------------------------------------------------------
// Skid buffer for write data
//-----------------------------------------------------------------
  val pmem_wr_q = RegInit(0.U(4.W))
  val pmem_write_data_q = RegInit(0.U(32.W))

  when(pmem_req_w.wr.orR && !pmem_resp_w.accept) {
    pmem_wr_q := pmem_req_w.wr
  }.elsewhen(pmem_resp_w.accept) {
    pmem_wr_q := 0.U
  }

  when(!pmem_resp_w.accept) {
    pmem_write_data_q := pmem_req_w.writeData
  }

// ------------------------------------------------------
// AXI Error Handling
// ------------------------------------------------------
  val error_q = RegInit(false.B)
  
  when(pmem_resp_w.ack && pmem_resp_w.error) {
    error_q := true.B
  }.elsewhen(cpuresp.ack) {
    error_q := false.B
  }

  cpuresp.error := error_q    

//-----------------------------------------------------------------
// Outport
//-----------------------------------------------------------------
  val refill_request_w = (state_q =/= STATE_REFILL && next_state_r === STATE_REFILL)
  evict_request_w      := (state_q === STATE_EVICT) && (evict_way_w || memReq_m_q.writeback)

  // AXI Read/Write channel
  pmem_req_w.rd        := (refill_request_w || pmem_rd_q)
  pmem_req_w.wr        := Mux(evict_request_w || pmem_wr_q.orR, "hF".U(4.W), 0.U(4.W))
  pmem_req_w.addr      := Mux(pmem_req_w.len.orR,
                          Mux(pmem_req_w.rd,
                            Cat(memReq_m_q.addr(31, p.tagReqLineL), 0.U(p.offBits.W)),
                            Cat(evict_addr_r, 0.U(p.offBits.W))),
                            pmem_addr_q
                          )
  pmem_req_w.len       := Mux(refill_request_w || pmem_rd_q || (state_q === STATE_EVICT && pmem_wr0_q), 7.U, 0.U)
  pmem_req_w.writeData := Mux(pmem_wr_q.orR, pmem_write_data_q, evict_data_r)

  // Outport signals
  pmemreq     := pmem_req_w
  pmem_resp_w := pmemresp

  if(CL3Config.EnablePerf){
    val perf = Module(new DCachePerf(p))
    val req_fire = cpureq.rd && io.cpu.accept
    perf.io.ev_req_fire          := req_fire
    perf.io.ev_read_req          := req_fire && !(cpureq.dataWr.orR)
    perf.io.ev_write_req         := req_fire && (cpureq.dataWr.orR)
    perf.io.ev_miss              := (state_q === STATE_READ || state_q === STATE_WRITE)
    perf.io.ev_refill_beat_fire  := pmem_resp_w.ack
    perf.io.ev_refill_line_last  := pmem_resp_w.ack && pmem_last_w
    perf.io.ev_miss_penalty_cyc  := (state_q === STATE_REFILL) || (state_q === STATE_READ) || (state_q === STATE_WRITE) || (state_q === STATE_EVICT_WAIT) || (state_q === STATE_EVICT)
    perf.io.ev_writeback         := state_q === STATE_EVICT_WAIT
    perf.io.ev_amo               := req_fire && (io.atomic.isAmo || io.atomic.isLR || io.atomic.isSC)
  }
  // val dbg_state = Wire(UInt(80.W))
  // dbg_state := "RESET".U

  // switch(state_q) {
  //     is(STATE_RESET)       { dbg_state := "RESET" }
  //     is(STATE_FLUSH_ADDR)  { dbg_state := "FLUSH_ADDR" }
  //     is(STATE_FLUSH)       { dbg_state := "FLUSH" }
  //     is(STATE_LOOKUP)      { dbg_state := "LOOKUP" }
  //     is(STATE_READ)        { dbg_state := "READ" }
  //     is(STATE_WRITE)       { dbg_state := "WRITE" }
  //     is(STATE_REFILL)      { dbg_state := "REFILL" }
  //     is(STATE_EVICT)       { dbg_state := "EVICT" }
  //     is(STATE_EVICT_WAIT)  { dbg_state := "EVICT_WAIT" }
  //     is(STATE_INVALIDATE)  { dbg_state := "INVAL" }
  //     is(STATE_WRITEBACK)   { dbg_state := "WRITEBACK" }
  // }
}
