#ifndef _DIFFTEST_H_
#define _DIFFTEST_H_
#include <stdint.h>

#define RESET_VECTOR 0x80000000U
#define TRAP_VECTOR  0x20000000U
#define UART_ADDR    0x10000000U
#define STOP_ADDR    0x1000000CU
#define PMEM_SIZE 0x8000000U
#define GPR_NUM 32
#define CSR_NUM 16 //Don't modify this
#define __EXPORT __atrribute__((visibility("default")))
#define DIFF_MEMCPY (void (*)(unsigned int, void *, size_t, bool))
#define DIFF_REGCPY (void (*)(void *, bool))
#define DIFF_EXEC (void (*)(uint64_t))
#define DIFF_INIT (void (*)(int))

#define COLOR_RED "\033[1;31m"
#define COLOR_END "\033[0m"


#define MEPC    0x341
#define MTVEC   0x305
#define MCAUSE  0x342
#define MSTATUS 0x300

enum { DIFFTEST_TO_DUT, DIFFTEST_TO_REF };


typedef struct context {
  uint32_t gpr[GPR_NUM];
  uint32_t csr[CSR_NUM];
  uint32_t pc;
} core_context_t;

// Note: The layout of this struct must be kept consistent with the SystemVerilog definition.
// SystemVerilog packed struct places the first-declared field at MSB. In memory (little-endian),
// the lowest address corresponds to the last-declared field. Therefore, we declare the C struct
// in reverse order and force 1-byte packing to match the SV packed layout exactly.
#pragma pack(push, 1)
typedef struct _diff_info_t {
  // 为匹配 SV packed struct（低地址为最后声明字段的 LSB），C 侧需按“逆序”声明
  // 低地址 -> 高地址：csr_waddr(2) csr_wdata(4) csr_wen(2) skip(2) commit(2) wdata(4) wen(2) rdIdx(2) inst(4) npc(4) pc(4)
  uint16_t csr_waddr;
  uint32_t csr_wdata;
  uint16_t csr_wen;
  uint16_t skip;
  uint16_t commit;
  uint32_t wdata;
  uint16_t wen;
  uint16_t rdIdx;
  uint32_t inst;
  uint32_t npc;
  uint32_t pc;
} difftest_info_t; 
#pragma pack(pop)

#ifdef __cplusplus
static_assert(sizeof(difftest_info_t) == 32, "difftest_info_t must be 32 bytes");
#endif

// typedef struct {
//   uint32_t pc;
//   uint32_t npc;
//   uint32_t inst;
//   uint16_t rdIdx;
//   uint16_t wen;
//   uint32_t wdata;
//   uint16_t commit;
//   uint16_t skip;
//   uint16_t csr_wen;
//   uint32_t csr_wdata;
//   uint16_t csr_waddr;
// } difftest_info_t;

#ifdef __cplusplus
extern "C" {
#endif

void difftest_init(const char *ref_so_file, const char *img_file) ;

#ifdef __cplusplus
}
#endif
#endif
