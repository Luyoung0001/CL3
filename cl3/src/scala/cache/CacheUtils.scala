package cl3

import chisel3._
import chisel3.util._

object SramAddr {
  @inline def bankSel(addr: UInt, bankSelBits: Int = 2): UInt =
    addr(bankSelBits - 1, 0)

  @inline def rowAddr(addr: UInt, bankSelBits: Int = 2): UInt =
    addr >> bankSelBits

  @inline def byteMaskToBitMask(byteMask: UInt, dataBits: Int = 32): UInt = {
    val bytes = dataBits / 8
    // Cat(Seq.tabulate(bytes) { i =>
    //     Fill(8, byteMask(i))
    // }.reverse)
    Cat((0 until bytes).reverse.map(i => Fill(8, !byteMask(i))))
  }
}

object DCacheState extends ChiselEnum {
  val STATE_RESET, STATE_FLUSH_ADDR, STATE_FLUSH, // 0000 0, 0001 1, 0010 2
      STATE_LOOKUP, STATE_READ, STATE_WRITE, STATE_REFILL, // 0011 3, 0100 4, 0101 5, 0110 6
      STATE_EVICT, STATE_EVICT_WAIT, STATE_INVALIDATE,  // 0111 7, 1000 8, 1001 9
      STATE_WRITEBACK, STATE_AMOCOMPUTE = Value // 1010 A, 1011 B
}

object ICacheStateP0 extends ChiselEnum {
  val STATE_FLUSH, STATE_LOOKUP, STATE_REFILL, STATE_RELOOKUP = Value
}

object ICacheStateP1 extends ChiselEnum {
  val STATE_LOOKUP_P1, STATE_REFILL_P1, STATE_RELOOKUP_P1, STATE_WAIT_P1 = Value
}
