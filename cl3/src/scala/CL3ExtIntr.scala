package cl3

import chisel3._
import chisel3.util._

class RandomExtIrq(
  val pulseLen:     Int   = 20,
  val lfsrWidth:    Int   = 16,
  val minGapCycles: Int   = 2048,
  // Trigger mask: a new pulse is triggered when (lfsr & triggerMask) === 0.U
  val triggerMask:  BigInt = 0x00FF
) extends Module {

  val io = IO(new Bundle {
    val enable = Input(Bool())   // 1: enable random interrupt generation
    val extIrq = Output(Bool())  // external interrupt output to the CPU
  })

  // --------------------------------------------------------------------------
  // Pseudo-random LFSR
  // --------------------------------------------------------------------------

  // LFSR register with a non-zero seed
  val lfsr = RegInit("hACE1".U(lfsrWidth.W))
  val feedback = lfsr(15) ^ lfsr(13) ^ lfsr(12) ^ lfsr(10)

  // Shift right by one, insert feedback at MSB
  val lfsrNext = Cat(lfsr(lfsrWidth - 2, 0), feedback)

  // LFSR is updated every cycle (even if enable is low)
  lfsr := lfsrNext

  // --------------------------------------------------------------------------
  // Pulse state and counter
  // --------------------------------------------------------------------------

  // 1 when we are currently outputting an interrupt pulse
  val irqActive = RegInit(false.B)

  // Remaining cycles in the current pulse
  private val pulseCntWidth = log2Ceil(pulseLen + 1)
  val pulseCnt = RegInit(0.U(pulseCntWidth.W))

  // --------------------------------------------------------------------------
  // Minimum gap counter ("cool-down" period)
  // --------------------------------------------------------------------------

  // Counts down between pulses:
  //   after a pulse finishes, we load minGapCycles and count down to 0.
  private val gapCntWidth = log2Ceil(minGapCycles + 1)
  val gapCnt = RegInit(0.U(gapCntWidth.W))

  // Output register for the interrupt line
  val extIrqReg = RegInit(false.B)
  io.extIrq := extIrqReg

  // Pre-build mask as a UInt with the correct width
  private val triggerMaskU = (triggerMask & ((BigInt(1) << lfsrWidth) - 1)).U(lfsrWidth.W)
  // --------------------------------------------------------------------------
  // Main control logic
  // --------------------------------------------------------------------------

  when (!io.enable) {
    // When disabled: clear all state and force output low
    irqActive := false.B
    pulseCnt  := 0.U
    gapCnt    := 0.U
    extIrqReg := false.B

  } .otherwise {
    when (irqActive) {
      // We are currently generating an interrupt pulse
      extIrqReg := true.B

      when (pulseCnt === 0.U) {
        // Pulse has completed:
        //   - De-assert IRQ
        //   - Enter cool-down period
        irqActive := false.B
        extIrqReg := false.B
        gapCnt    := minGapCycles.U(gapCntWidth.W)
      } .otherwise {
        // Continue the current pulse
        pulseCnt := pulseCnt - 1.U
      }

    } .otherwise {
      // Not currently in a pulse, default output low
      extIrqReg := false.B

      when (gapCnt =/= 0.U) {
        // Still in cool-down period: just decrement the gap counter
        gapCnt := gapCnt - 1.U

      } .otherwise {
        // Cool-down finished → allowed to attempt a new trigger
        // Trigger condition: (lfsr & triggerMask) == 0
        when ((lfsr & triggerMaskU) === 0.U) {
          // Start a new pulse
          irqActive := true.B
          pulseCnt  := (pulseLen - 1).U(pulseCntWidth.W)
          extIrqReg := true.B
          // gapCnt will be reloaded when the pulse ends.
        }
      }
    }
  }
}