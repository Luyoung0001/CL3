package cl3

import chisel3._
import chisel3.util._

trait CL3Config {
  val AddrWidth    = 32
  val DataWidth    = 64
  val EnableMMU    = false
  val EnableBP     = true
  val EnableDiff   = true
  val EnablePerf   = true
  // val SimMemOption = "DPI-C"
  val SimMemOption = "SoC"
  val BOOTADDR     = "h80000000".U
}

object CL3Config extends CL3Config {}
