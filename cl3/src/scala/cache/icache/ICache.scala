package cl3

import chisel3._
import chisel3.util._

class ICache(p: ICacheParams) extends Module with CL3Config {
  val io = IO(new ICacheIO(p))

  val (cpu, aw, w, b, ar, r) = (io.cpu, io.axi.aw, io.axi.w, io.axi.b, io.axi.ar, io.axi.r)

  cpu.resp_accept := false.B
  cpu.resp_valid  := false.B
  cpu.resp_error  := false.B
  cpu.resp_inst   := 0.U

  aw.valid := false.B
  aw.bits  := 0.U.asTypeOf(io.axi.aw.bits)
  w.valid  := false.B
  w.bits   := 0.U.asTypeOf(io.axi.w.bits)
  ar.valid := false.B
  ar.bits  := 0.U.asTypeOf(io.axi.ar.bits)
  b.ready  := true.B
  r.ready  := true.B

  import ICacheStateP0._
  import ICacheStateP1._
  
  val state_q        = RegInit(STATE_FLUSH)
  val next_state_r   = WireDefault(state_q)

  val state_p1_q        = RegInit(STATE_LOOKUP_P1)
  val next_state_p1_r   = WireDefault(state_p1_q)

  val invalidate_q  = RegInit(false.B)
  val replace_way_q = RegInit(0.U(p.wayBits.W))

  val req_line_addr_w   = cpu.req_pc(p.tagReqLineH, p.tagReqLineL)
  val req_data_addr_w   = cpu.req_pc(p.dataRamAddrBits + 2, 3)
  val flush_addr_q      = RegInit(0.U(p.tagReqLineW.W));

  val unaligned_en = cpu.req_pc(2)
  val crossLineEn  = cpu.req_pc(p.lineSizeBits - 1, 2).andR


//-----------------------------------------------------------------
// Lookup validation
//-----------------------------------------------------------------
  val lookup_valid_q = RegInit(false.B)
  when(cpu.req_rd && cpu.resp_accept) {
    lookup_valid_q := true.B
  }.elsewhen(cpu.resp_valid) {
    lookup_valid_q := false.B
  }

//-----------------------------------------------------------------
// Lookup address
//-----------------------------------------------------------------
  val lookup_addr_q = RegInit(0.U(p.pcBits.W))
  when(cpu.req_rd && cpu.resp_accept) {
    when(unaligned_en) {
      lookup_addr_q := Cat(cpu.req_pc(p.pcBits - 1, 3), 0.U(3.W))
    }.otherwise {
      lookup_addr_q := cpu.req_pc
    }
  }
  // lookup_addr_q := Mux(cpu.req_rd && cpu.resp_accept, cpu.req_pc, lookup_addr_q)

  val req_pc_tag_cmp_w = lookup_addr_q(p.tagCmpAddrH, p.tagCmpAddrL)

//-----------------------------------------------------------------
// TAG RAMS
//-----------------------------------------------------------------
  val tag_addr_r = Wire(UInt(p.tagReqLineW.W))
  tag_addr_r := flush_addr_q

// Tag RAM address

  // Cache flush
  when(state_q === STATE_FLUSH) {
    tag_addr_r := flush_addr_q
    // Line refill
  }.elsewhen(
    state_q === STATE_REFILL || state_q === STATE_RELOOKUP || state_p1_q === STATE_REFILL_P1 || next_state_p1_r === STATE_REFILL_P1
  ) {
    tag_addr_r := lookup_addr_q(p.tagReqLineH, p.tagReqLineL)
    // Lookup
  }.otherwise {
    tag_addr_r := req_line_addr_w
  }

// Tag RAM write data
  val tag_data_in_r = WireDefault(0.U(p.tagRamDataBits.W))

  // Cache flush
  when(state_q === STATE_FLUSH) {
    tag_data_in_r := 0.U
    // Line refill
  }.elsewhen(state_q === STATE_REFILL) {
    tag_data_in_r := Cat(1.U(1.W), lookup_addr_q(p.tagCmpAddrH, p.tagCmpAddrL)) // valid = 1, tag = addr
  }

// Tag RAM write enable
  val nWays            = p.numWays
  val tag_data_out_w   = Wire(Vec(p.numWays, UInt(p.tagRamDataBits.W)))
  class TagMeta(p: ICacheParams) extends Bundle {
    val valid   = Bool()
    val tagBits = UInt((p.cacheTagAddrBits + 1).W)
  }

  val tagMeta_m_w = VecInit((0 until nWays).map { w =>
    val m = Wire(new TagMeta(p))
    m.valid   := tag_data_out_w(w)(p.cacheTagValidBit)
    m.tagBits := tag_data_out_w(w)(p.cacheTagAddrBits, 0)
    m
  })

  val tag_valid_w          = VecInit(tagMeta_m_w.map(_.valid))
  val tag_addr_bits_w      = VecInit(tagMeta_m_w.map(_.tagBits))
  val tag_hit_w: Vec[Bool] = VecInit((0 until nWays).map { w =>
    tag_valid_w(w) && (tag_addr_bits_w(w) === req_pc_tag_cmp_w)
  })
  val tag_hit_any_w        = tag_hit_w.reduce(_||_)

  val u_tag                = Seq.fill(p.numWays)(Module(new ICacheTagRam(p)))
  val u_valid              = Seq.fill(p.numWays)(Module(new ICacheValidRam(p)))

  for (i <- 0 until p.numWays) {
    val tag_write_r = WireDefault(false.B)
    // Cache flush
    when(state_q === STATE_FLUSH) {
      tag_write_r := true.B
      // Line refill
    }.elsewhen(state_q === STATE_REFILL) {
      tag_write_r := r.valid && r.bits.last && (replace_way_q === i.U)
    }
    val tagRam      = u_tag(i).io.p0
    tagRam.addr       := tag_addr_r
    tagRam.din        := tag_data_in_r
    tagRam.wr         := tag_write_r
    
    // tag_data_out_w(i) := tagRam.dout
    val validRam    = u_valid(i).io.p0
    tag_data_out_w(i) := Cat(validRam.dout, tagRam.dout(p.cacheTagAddrBits, 0))

    validRam.addr := tag_addr_r
    validRam.wr   := tag_write_r
    validRam.din  := tag_data_in_r(p.cacheTagValidBit)
    u_valid(i).io.flush := (state_q === STATE_FLUSH)
  }

//-----------------------------------------------------------------
// DATA RAMS
//-----------------------------------------------------------------
  val data_addr_r       = Wire(UInt(p.dataRamAddrBits.W))
  val data_write_addr_q = RegInit(0.U(p.dataRamAddrBits.W))
  val refill_word_idx_q = RegInit(0.U(3.W))
  val refill_lower_q    = RegInit(0.U(p.axi.axiDataBits.W))

  when(r.valid && r.bits.last) {
    refill_word_idx_q := 0.U
  }.elsewhen(r.valid) {
    refill_word_idx_q := refill_word_idx_q + 1.U
  }

  refill_lower_q := Mux(r.valid, r.bits.data, refill_lower_q)

// Data RAM refill write address
  when(state_q === STATE_LOOKUP && next_state_r === STATE_REFILL) {
    data_write_addr_q := ar.bits.addr(p.dataRamAddrBits + 2, 3)
  }.elsewhen(state_q === STATE_REFILL && r.valid && refill_word_idx_q(0)) { // todo : for 64 bits data
    data_write_addr_q := data_write_addr_q + 1.U
  }

// Data RAM address
  data_addr_r := req_data_addr_w
  when(state_q === STATE_REFILL) {
    data_addr_r := data_write_addr_q
  }.elsewhen(state_q === STATE_RELOOKUP || state_p1_q === STATE_RELOOKUP_P1) {
    data_addr_r := lookup_addr_q(p.dataRamAddrBits + 2, 3)
  }.otherwise {
    data_addr_r := req_data_addr_w
  }

// Data RAM write enable
  // val data0_out = Wire(UInt(p.dataRamDataBits.W))
  // val data1_out = Wire(UInt(p.dataRamDataBits.W))
  val data_out = Wire(Vec(p.numWays, UInt(p.dataRamDataBits.W)))
  val u_data   = Seq.fill(p.numWays)(Module(new ICacheDataRam(p)))
  for (i <- 0 until p.numWays) {
    val data_write_r = r.valid && (replace_way_q === i.U) && (state_q === STATE_REFILL)

    val dataRam = u_data(i).io.p0
    dataRam.addr := data_addr_r
    dataRam.din  := Cat(r.bits.data, refill_lower_q)
    dataRam.wr   := data_write_r
    data_out(i)  := dataRam.dout
  }

//-----------------------------------------------------------------
// Flush counter
//-----------------------------------------------------------------
  when(state_q === STATE_FLUSH) {
    flush_addr_q := flush_addr_q + 1.U
  }.elsewhen(cpu.req_invalidate && cpu.resp_accept) {
    flush_addr_q := req_line_addr_w
  }.otherwise {
    flush_addr_q := 0.U
  }

//-----------------------------------------------------------------
// Replacement Policy
//-----------------------------------------------------------------
// Using random replacement policy - this way we cycle through the ways
// when needing to replace a line.
  val replP = ReplParams.fromICache(p)
  val repl  = Module(new Replacement(replP, ReplRandom, 2))

  val refillValid_p0_r = RegNext(r.valid && r.bits.last && (state_q === STATE_REFILL))
  repl.io.ports(0).setIdx       := lookup_addr_q(p.tagReqLineH, p.tagReqLineL)
  repl.io.ports(0).invalidWayOH := ~(tag_valid_w.asUInt)
  repl.io.ports(0).hitValid     := tag_hit_any_w && cpu.resp_valid && !refillValid_p0_r
  repl.io.ports(0).hitWayOH     := tag_hit_w.asUInt
  repl.io.ports(0).refillValid  := r.valid && r.bits.last && (state_q === STATE_REFILL)
  repl.io.ports(0).refillWayOH  := repl.io.ports(0).victimWayOH

  replace_way_q := Mux(r.valid && r.bits.last && (state_q === STATE_REFILL), OHToUInt(repl.io.ports(0).victimWayOH), replace_way_q)
  val p1_relookup    = RegInit(false.B)
  val p0_tag_hit     = RegInit(VecInit(Seq.fill(nWays)(false.B)))
  val p0_tag_hit_any = p0_tag_hit.asUInt.orR
// State Transition
  switch(state_q) {
    is(STATE_FLUSH) {
      when(invalidate_q) {
        next_state_r := STATE_LOOKUP
      }.elsewhen(flush_addr_q === Fill(p.lineAddrBits, 1.U(1.W))) {
        next_state_r := STATE_LOOKUP
      }
    }
    is(STATE_LOOKUP) {
      val tag_hit_any = Mux(p1_relookup, p0_tag_hit_any, tag_hit_any_w)
      when(lookup_valid_q && !tag_hit_any) {
        next_state_r := STATE_REFILL
      }.elsewhen(cpu.req_invalidate || cpu.req_flush) {
        next_state_r := STATE_FLUSH
      }
    }
    is(STATE_REFILL) {
      when(r.valid && r.bits.last) {
        next_state_r := STATE_RELOOKUP
      }
    }
    is(STATE_RELOOKUP) {
      // next_state_r := STATE_DONE
      next_state_r := STATE_LOOKUP

    }
  }

// Second port for unaligned access

  state_p1_q := next_state_p1_r

  val unaligned_Reg = RegInit(false.B)
  val crossLineReg  = RegInit(false.B)
  unaligned_Reg := Mux(cpu.req_rd && cpu.resp_accept, unaligned_en, unaligned_Reg)
  crossLineReg  := Mux(cpu.req_rd && cpu.resp_accept, crossLineEn, crossLineReg)

  val req_line_addr_p1_w   =
    Mux(unaligned_en, (cpu.req_pc + 4.U)(p.tagReqLineH, p.tagReqLineL), cpu.req_pc(p.tagReqLineH, p.tagReqLineL))
  val req_data_addr_p1_w   =
    Mux(unaligned_en, cpu.req_pc(p.dataRamAddrBits + 2, 3) + 1.U, cpu.req_pc(p.dataRamAddrBits + 2, 3))

  // -----------------------------------------------------------------
  // Lookup validation
  // -----------------------------------------------------------------
  val lookup_valid_p1_q = RegInit(false.B)
  when(cpu.req_rd && cpu.resp_accept && unaligned_en) {
    lookup_valid_p1_q := true.B
  }.elsewhen(cpu.resp_valid) {
    lookup_valid_p1_q := false.B
  }

  // -----------------------------------------------------------------
  // Lookup address
  // -----------------------------------------------------------------
  val lookup_addr_p1_q = RegInit(0.U(p.pcBits.W))
  when(cpu.req_rd && cpu.resp_accept) {
    when(unaligned_en) {
      lookup_addr_p1_q := cpu.req_pc + 4.U
    }.otherwise {
      lookup_addr_p1_q := cpu.req_pc
    }
  }
  // lookup_addr_p1_q := Mux(cpu.req_rd && cpu.resp_accept, cpu.req_pc, lookup_addr_p1_q)

  val req_pc_tag_cmp_p1_w = lookup_addr_p1_q(p.tagCmpAddrH, p.tagCmpAddrL)

//-----------------------------------------------------------------
// TAG RAMS
//-----------------------------------------------------------------
  // val req_line_addr_p1_w = cpu.req_pc(p.tagReqLineH, p.tagReqLineL)
  val tag_addr_p1_r = Wire(UInt(p.tagReqLineW.W))

// Tag RAM address

  when(state_p1_q === STATE_REFILL_P1 || state_p1_q === STATE_RELOOKUP_P1 || state_q === STATE_RELOOKUP) {
    tag_addr_p1_r := lookup_addr_p1_q(p.tagReqLineH, p.tagReqLineL)
  } 
  // Lookup
  .otherwise {
    tag_addr_p1_r := req_line_addr_p1_w
  }

// Tag RAM write data
  val tag_data_in_p1_r = WireDefault(0.U(p.tagRamDataBits.W))

  when(state_p1_q === STATE_REFILL_P1) {
    tag_data_in_p1_r := Cat(1.U(1.W), lookup_addr_p1_q(p.tagCmpAddrH, p.tagCmpAddrL)) // valid = 1, tag = addr
  }
// Tag RAM write enable
  val tag_data_out_p1_w   = Wire(Vec(p.numWays, UInt(p.tagRamDataBits.W)))

  val tagMeta_m_p1_w = VecInit((0 until nWays).map { w =>
    val m = Wire(new TagMeta(p))
    m.valid   := tag_data_out_p1_w(w)(p.cacheTagValidBit)
    m.tagBits := tag_data_out_p1_w(w)(p.cacheTagAddrBits, 0)
    m
  })

  val tag_valid_p1_w          = VecInit(tagMeta_m_p1_w.map(_.valid))
  val tag_addr_bits_p1_w      = VecInit(tagMeta_m_p1_w.map(_.tagBits))
  // Tag hit
  val tag_hit_p1_w: Vec[Bool] = VecInit((0 until nWays).map { w =>
    tag_valid_p1_w(w) && (tag_addr_bits_p1_w(w) === req_pc_tag_cmp_p1_w)
  })
  val tag_hit_any_p1_w        = tag_hit_p1_w.reduce(_||_)
  
  val refillValid_p1_r = RegNext(next_state_p1_r === STATE_RELOOKUP_P1)
  repl.io.ports(1).setIdx       := lookup_addr_p1_q(p.tagReqLineH, p.tagReqLineL)
  repl.io.ports(1).invalidWayOH := ~(tag_valid_p1_w.asUInt)
  repl.io.ports(1).hitValid     := tag_hit_any_p1_w && cpu.resp_valid && !refillValid_p1_r && crossLineReg
  repl.io.ports(1).hitWayOH     := tag_hit_p1_w.asUInt
  repl.io.ports(1).refillValid  := (next_state_p1_r === STATE_RELOOKUP_P1) && crossLineReg
  repl.io.ports(1).refillWayOH  := repl.io.ports(1).victimWayOH

  val replace_way_p1_q = RegInit(0.U(p.wayBits.W))
  replace_way_p1_q     := Mux(r.valid && r.bits.last , OHToUInt(repl.io.ports(1).victimWayOH), replace_way_p1_q)

  for (i <- 0 until p.numWays) {
    val tag_write_r = WireDefault(false.B)
    when(state_p1_q === STATE_REFILL_P1) {
      tag_write_r := r.valid && r.bits.last && (replace_way_p1_q === i.U)
    }
    val tagRam      = u_tag(i).io.p1

    tagRam.addr          := tag_addr_p1_r
    tagRam.din           := tag_data_in_p1_r
    tagRam.wr            := tag_write_r
    // tag_data_out_p1_w(i) := tagRam.dout

    val validRam    = u_valid(i).io.p1
    tag_data_out_p1_w(i) := Cat(validRam.dout, tagRam.dout(p.cacheTagAddrBits, 0))
    validRam.addr := tag_addr_p1_r
    validRam.wr   := tag_write_r
    validRam.din  := tag_data_in_p1_r(p.cacheTagValidBit)
  }

//-----------------------------------------------------------------
// DATA RAMS
//-----------------------------------------------------------------
  val data_addr_p1_r       = Wire(UInt(p.dataRamAddrBits.W))
  val data_write_addr_p1_q = RegInit(0.U(p.dataRamAddrBits.W))
  val refill_word_idx_p1_q = RegInit(0.U(3.W))
  val refill_lower_p1_q    = RegInit(0.U(p.axi.axiDataBits.W))

  when(r.valid && r.bits.last) {
    refill_word_idx_p1_q := 0.U
  }.elsewhen(r.valid) {
    refill_word_idx_p1_q := refill_word_idx_p1_q + 1.U
  }

  refill_lower_p1_q := Mux(r.valid, r.bits.data, refill_lower_p1_q)

// Data RAM refill write address
  when(state_p1_q === STATE_LOOKUP_P1 && next_state_p1_r === STATE_REFILL_P1) {
    data_write_addr_p1_q := ar.bits.addr(p.dataRamAddrBits + 2, 3)
  }.elsewhen(state_q === STATE_RELOOKUP && next_state_p1_r === STATE_REFILL_P1) {
    data_write_addr_p1_q := ar.bits.addr(p.dataRamAddrBits + 2, 3)
  }.elsewhen(state_p1_q === STATE_REFILL_P1 && r.valid && refill_word_idx_p1_q(0)) {
    data_write_addr_p1_q := data_write_addr_p1_q + 1.U
  }

// Data RAM address
  data_addr_p1_r := req_data_addr_p1_w
  when(state_p1_q === STATE_REFILL_P1) {
    data_addr_p1_r := data_write_addr_p1_q
  }.elsewhen(state_p1_q === STATE_RELOOKUP_P1 || state_q === STATE_RELOOKUP) {
    data_addr_p1_r := lookup_addr_p1_q(p.dataRamAddrBits + 2, 3)
  }.otherwise {
    data_addr_p1_r := req_data_addr_p1_w
  }

// Data RAM write enable
  val data_out_p1 = Wire(Vec(p.numWays, UInt(p.dataRamDataBits.W)))

  for (i <- 0 until p.numWays) {
    val data_write_r = r.valid && (replace_way_p1_q === i.U) && (state_p1_q === STATE_REFILL_P1)
    
    val dataRam = u_data(i).io.p1
    dataRam.addr   := data_addr_p1_r
    dataRam.din    := Cat(r.bits.data, refill_lower_p1_q)
    dataRam.wr     := data_write_r
    data_out_p1(i) := dataRam.dout
  }

  switch(state_p1_q) {
    is(STATE_LOOKUP_P1) {
      p1_relookup := false.B
      when(lookup_valid_p1_q && !tag_hit_any_p1_w) {
        next_state_p1_r := Mux(next_state_r === STATE_REFILL, STATE_WAIT_P1, STATE_REFILL_P1)
      }
    }
    is(STATE_WAIT_P1) {
      when(r.valid && r.bits.last) {
        next_state_p1_r := Mux(crossLineReg, STATE_REFILL_P1, STATE_RELOOKUP_P1)
      }
    }
    is(STATE_REFILL_P1) {
      when(next_state_r === STATE_REFILL) {
        next_state_p1_r := STATE_WAIT_P1
      }.elsewhen(r.valid && r.bits.last) {
        next_state_p1_r := STATE_RELOOKUP_P1
      }
    }
    is(STATE_RELOOKUP_P1) {
      // next_state_r := STATE_DONE
      p0_tag_hit      := tag_hit_w
      p1_relookup     := true.B
      next_state_p1_r := STATE_LOOKUP_P1

    }
  }
//-----------------------------------------------------------------
// Instruction Output
//-----------------------------------------------------------------
  // cpu.resp_valid := lookup_valid_q && (state_q === STATE_DONE) && tag_hit_any_w
  val p0_resp_valid = lookup_valid_q && (state_q === STATE_LOOKUP) && (Mux(p1_relookup, p0_tag_hit_any, tag_hit_any_w))
  val p1_resp_valid = (lookup_valid_p1_q && (state_p1_q === STATE_LOOKUP_P1) && tag_hit_any_p1_w) || !unaligned_Reg

  cpu.resp_valid := p0_resp_valid && p1_resp_valid

  val inst_r = WireDefault(0.U(p.instBits.W))

  val hitVec = Mux(p1_relookup, p0_tag_hit, tag_hit_w)
  when (hitVec.asUInt.orR) {
    inst_r := Mux1H(hitVec, data_out)
  }

  val inst_p1_r = WireDefault(0.U(p.instBits.W))

  when(tag_hit_any_p1_w){
    inst_p1_r := Mux1H(tag_hit_p1_w, data_out_p1)
  }
  
  cpu.resp_inst := Mux(unaligned_Reg, Cat(inst_p1_r(31, 0), inst_r(63, 32)), inst_r)

// Update state
  state_q         := next_state_r
  cpu.resp_accept := (state_q === STATE_LOOKUP && next_state_r =/= STATE_REFILL) && (state_p1_q === STATE_LOOKUP_P1 && (next_state_p1_r =/= STATE_REFILL_P1))

//-----------------------------------------------------------------
// Invalidate
//-----------------------------------------------------------------
  invalidate_q := Mux(cpu.req_invalidate && cpu.resp_accept, true.B, false.B)

//-----------------------------------------------------------------
// AXI Request Hold
//-----------------------------------------------------------------
  val arvalid_q = RegInit(false.B)
  arvalid_q := Mux(ar.valid && !ar.ready, true.B, false.B)

//-----------------------------------------------------------------
// AXI Error Handling
//-----------------------------------------------------------------
  val axi_state   = RegInit(true.B)
  when(r.valid && r.ready && r.bits.last) {
    axi_state := true.B
  }.elsewhen(ar.valid && ar.ready) {
    axi_state := false.B
  }
  val axi_error_q = RegInit(false.B)
  when(r.valid && r.ready && r.bits.resp =/= 0.U) {
    axi_error_q := true.B
  }.elsewhen(cpu.resp_valid) {
    axi_error_q := false.B
  }
  cpu.resp_error := axi_error_q

  ar.valid := ((state_q === STATE_REFILL && next_state_r === STATE_REFILL) || (state_p1_q === STATE_REFILL_P1 && next_state_p1_r === STATE_REFILL_P1)) && axi_state

  val araddr_p0_w = Cat(lookup_addr_q(p.axi.axiAddrBits - 1, p.lineSizeBits), 0.U(p.lineSizeBits.W))
  val araddr_p1_w = Cat(lookup_addr_p1_q(p.axi.axiAddrBits - 1, p.lineSizeBits), 0.U(p.lineSizeBits.W))

  val p0_refill_sel = (next_state_r === STATE_REFILL) || (state_q === STATE_REFILL)
  val p1_refill_sel = (next_state_p1_r === STATE_REFILL_P1) || (state_p1_q === STATE_REFILL_P1)

  val araddr_sel_w = Mux(p0_refill_sel, araddr_p0_w, Mux(p1_refill_sel, araddr_p1_w, 0.U))

  ar.bits.addr  := araddr_sel_w
  ar.bits.burst := 1.U            // INCR
  ar.bits.id    := p.axiIdDefault.U
  ar.bits.len   := p.axi.axiLenBits.U - 1.U
  ar.bits.cache := "b0110".U(4.W) // 0010 Cacheable，Nonbufferable
  ar.bits.lock  := 0.U
  ar.bits.size  := (log2Ceil(p.axi.axiDataBits / 8)).U
  // ar.bits.port  := "b101".U(3.W) // default: instruction access, secure, privileged

  // ar.bits.qos   := 0.U
  // ar.bits.user  := 0.U
  // ar.bits.region:= 0.U
  // when(state_q === STATE_LOOKUP && next_state_r =/= STATE_REFILL && cpu.req_rd) {
  //         state_q := STATE_DONE
  //     }

//-----------------------------------------------------------------
// Performance monitor hookup
//-----------------------------------------------------------------
  if (CL3Config.EnablePerf) {
    val perf = Module(new ICachePerf(p))
    perf.io.ev_req_fire          := cpu.req_rd && cpu.resp_accept
    perf.io.ev_miss              := lookup_valid_q && !tag_hit_any_w && (state_q === STATE_LOOKUP)
    perf.io.ev_refill_burst_fire := ar.valid && ar.ready
    perf.io.ev_refill_beat_fire  := r.valid
    perf.io.ev_refill_line_last  := r.valid && r.bits.last
    perf.io.ev_stall_cycle       := cpu.req_rd && !cpu.resp_accept
    perf.io.ev_miss_penalty_cyc  := (state_q === STATE_REFILL) || (state_q === STATE_RELOOKUP)
    perf.io.ev_flush             := cpu.req_flush && cpu.resp_accept
    perf.io.ev_invalidate        := cpu.req_invalidate && cpu.resp_accept
    perf.io.ev_axi_err           := r.valid && r.bits.resp =/= 0.U
  }

}
