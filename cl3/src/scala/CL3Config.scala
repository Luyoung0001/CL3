package cl3

import chisel3._
import chisel3.util._

trait CL3Config {
  val ADDR_WIDTH    = 32
  val DATA_WIDTH    = 64
  val EnableMMU     = false
  val EnableBP      = true
  val EnableDiff    = true
  val EnablePerf    = false
  val EnableExtIntr = false
  val EnableAtomic  = true
  // val SimMemOption = "DPI-C"
  val SimMemOption  = "SoC"
  val BOOT_ADDR     = (if (SimMemOption == "SoC") "h20000000" else "h80000000").U
}

object CL3Config extends CL3Config {}
