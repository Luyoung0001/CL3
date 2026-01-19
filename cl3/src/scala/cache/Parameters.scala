package cl3
import chisel3._
import chisel3.util._

case class AXI4Params(
  axiIdBits:    Int = 4,
  axiAddrBits:  Int = 32, // [31:0]
  axiDataBits:  Int = 32, // W/R data [31:0]
  axiLenBits:   Int = 8,  // [7:0]
  axiBurstBits: Int = 2,  // [1:0]
  axiRespBits:  Int = 2   // [1:0]
) {
  val axiStrbBits: Int = axiDataBits / 8
}

case class ICacheParams(
  axiIdDefault: Int = 0,

  // I-Cache request
  pcBits:   Int = 32,
  instBits: Int = 64, // req_inst_o is 64bit

  // Data RAM: addr[9:0], data[63:0]
  dataRamDataBits: Int = 64,

  // Cache parameters
  numWays:       Int = 4,
  numLines:      Int = 128,
  lineSizeBytes: Int = 32,

  useMacro: Boolean = false,
  axi:      AXI4Params = AXI4Params()
  // macroVsrc      : String  = "/vsrc"
) {
  val wayBits     : Int = log2Ceil(numWays)
  val lineSizeBits: Int = log2Ceil(lineSizeBytes)
  val lineAddrBits: Int = log2Ceil(numLines)

  val lineWords: Int = lineSizeBytes / (axi.axiDataBits / 8)

  val banks:       Int = lineSizeBytes / 8
  val bankSelBits: Int = log2Ceil(banks)

  val dataRamAddrBits:  Int = lineAddrBits + lineSizeBits - 3

  val tagCmpAddrBits:   Int = pcBits - (lineAddrBits + lineSizeBits)
  val tagRamIdxBits:    Int = lineAddrBits                           // depth = 256
  val cacheTagValidBit: Int = tagCmpAddrBits
  val cacheTagAddrBits: Int = tagCmpAddrBits - 1
  val tagRamDataBits:   Int = tagCmpAddrBits + 1

  val dataRamIdxBits: Int = log2Ceil(numLines)
  // Tag req bits
  val tagReqLineL:    Int = lineSizeBits
  val tagReqLineH:    Int = lineSizeBits + lineAddrBits - 1
  val tagReqLineW:    Int = lineAddrBits


  // Tag compare bits
  val tagCmpAddrL: Int = lineSizeBits + lineAddrBits
  val tagCmpAddrH: Int = pcBits - 1
}

case class DCacheParams(
  addrW:       Int = 32,
  dataW:       Int = 32,
  reqTagW:     Int = 11,  // mem_req_tag
  axi_id:      Int = 0,
  ways:        Int = 4,
  sets:        Int = 128,
  lineBytes:   Int = 32,  // 32B
  tagMetaBits: Int = 2,   // {dirty, valid}

  dataRamDataBits: Int = 32,
  banks:           Int = 4,

  // sram
  useMacro: Boolean = false,
  axi:      AXI4Params = AXI4Params()
  // macroVsrc      : String  = "/vsrc"
) {

  val wayBits: Int = log2Ceil(ways)

  val offBits = log2Ceil(lineBytes) // = 5
  val idxBits = log2Ceil(sets)      // = 8

  val beatBytes = dataW / 8 // = 4
  val wstrbW    = beatBytes // = 4

  val lineWords = lineBytes / beatBytes // = 8

  val bankSelBits: Int = log2Ceil(banks)

  // Tag bits: 32 - 5 - 8 = 19
  val tagBits   = addrW - offBits - idxBits // = 19
  // tagBits + {dirty,valid} = 19 + 2 = 21
  val tagEntryW = tagBits + tagMetaBits     // = 21

  val tagReqLineL = offBits               // = 5
  val tagReqLineH = offBits + idxBits - 1 // = 12
  val tagReqLineW = idxBits               // = 8

  val tagCmpAddrL = tagReqLineH + 1               // = 13
  val tagCmpAddrH = addrW - 1                     // = 31
  val tagCmpAddrW = tagCmpAddrH - tagCmpAddrL + 1 // = 19

  val tagValidBit = tagBits + 0 // = 19
  val tagDirtyBit = tagBits + 1 // = 20

  val dataIdxW = idxBits + (offBits - log2Ceil(beatBytes)) // = 11

  val tagIdxW = idxBits // = 8
}
