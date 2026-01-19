#ifndef _DIFFTEST_H_
#define _DIFFTEST_H_

#include "Vtop.h"
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

// // modify

// #define GPR0  (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_0)
// #define GPR1  (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_1)
// #define GPR2  (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_2)
// #define GPR3  (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_3)
// #define GPR4  (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_4)
// #define GPR5  (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_5)
// #define GPR6  (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_6)
// #define GPR7  (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_7)
// #define GPR8  (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_8)
// #define GPR9  (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_9)
// #define GPR10 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_10)
// #define GPR11 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_11)
// #define GPR12 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_12)
// #define GPR13 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_13)
// #define GPR14 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_14)
// #define GPR15 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_15)
// #define GPR16 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_16)
// #define GPR17 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_17)
// #define GPR18 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_18)
// #define GPR19 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_19)
// #define GPR20 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_20)
// #define GPR21 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_21)
// #define GPR22 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_22)
// #define GPR23 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_23)
// #define GPR24 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_24)
// #define GPR25 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_25)
// #define GPR26 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_26)
// #define GPR27 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_27)
// #define GPR28 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_28)
// #define GPR29 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_29)
// #define GPR30 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_30)
// #define GPR31 (topp->rootp->top__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_31)

#define GPR0  (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_0)
#define GPR1  (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_1)
#define GPR2  (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_2)
#define GPR3  (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_3)
#define GPR4  (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_4)
#define GPR5  (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_5)
#define GPR6  (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_6)
#define GPR7  (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_7)
#define GPR8  (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_8)
#define GPR9  (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_9)
#define GPR10 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_10)
#define GPR11 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_11)
#define GPR12 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_12)
#define GPR13 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_13)
#define GPR14 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_14)
#define GPR15 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_15)
#define GPR16 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_16)
#define GPR17 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_17)
#define GPR18 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_18)
#define GPR19 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_19)
#define GPR20 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_20)
#define GPR21 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_21)
#define GPR22 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_22)
#define GPR23 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_23)
#define GPR24 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_24)
#define GPR25 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_25)
#define GPR26 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_26)
#define GPR27 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_27)
#define GPR28 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_28)
#define GPR29 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_29)
#define GPR30 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_30)
#define GPR31 (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__issue__DOT__rf__DOT__regs_31)

#define GPRSTR(i) GPR ## i
#define GET_GPR(i) dut.gpr[i] = GPRSTR(i);

#define GET_ALL_GPR do { \
  GET_GPR(0) \
  GET_GPR(1) \
  GET_GPR(2) \
  GET_GPR(3) \
  GET_GPR(4) \
  GET_GPR(5) \
  GET_GPR(6) \
  GET_GPR(7) \
  GET_GPR(8) \
  GET_GPR(9) \
  GET_GPR(10) \
  GET_GPR(11) \
  GET_GPR(12) \
  GET_GPR(13) \
  GET_GPR(14) \
  GET_GPR(15) \
  GET_GPR(16) \
  GET_GPR(17) \
  GET_GPR(18) \
  GET_GPR(19) \
  GET_GPR(20) \
  GET_GPR(21) \
  GET_GPR(22) \
  GET_GPR(23) \
  GET_GPR(24) \
  GET_GPR(25) \
  GET_GPR(26) \
  GET_GPR(27) \
  GET_GPR(28) \
  GET_GPR(29) \
  GET_GPR(30) \
  GET_GPR(31) \
} while(0);

#define MEPC    0x341
#define MTVEC   0x305
#define MCAUSE  0x342
#define MSTATUS 0x300
#define MTVAL   0x343

// EXCEPTION Registers
#define MEPC_R     (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__csr__DOT__csr_rf__DOT__csr_mepc_q)
#define MTVEC_R    (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__csr__DOT__csr_rf__DOT__csr_mtvec_q)
#define MCAUSE_R   (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__csr__DOT__csr_rf__DOT__csr_mcause_q)
#define MSTATUS_R  (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__csr__DOT__csr_rf__DOT__csr_sr_q)
#define MTVAL_R    (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__csr__DOT__csr_rf__DOT__csr_mtval_q)
#define MIP_R      (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__csr__DOT__csr_rf__DOT__csr_mip_q)
#define MIE_R      (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__csr__DOT__csr_rf__DOT__csr_mie_q)
#define MSCRATCH_R (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__csr__DOT__csr_rf__DOT__csr_mscratch_q)
#define MPRIV_R    (topp->rootp->top__DOT__soc_top_u__DOT__u_CL3Top__DOT__core__DOT__csr__DOT__csr_rf__DOT__csr_mpriv_q)

#define GET_ALL_CSR do { \
  dut.csr[0] = MEPC_R; \
  dut.csr[1] = MCAUSE_R; \
  dut.csr[2] = MTVEC_R; \
  dut.csr[3] = MSTATUS_R; \
  dut.csr[4] = MTVAL_R; \
  dut.csr[5] = MIP_R; \
  dut.csr[6] = MIE_R; \
  dut.csr[7] = MSCRATCH_R; \
  dut.csr[8] = MPRIV_R; \ 
} while(0);

enum { DIFFTEST_TO_DUT, DIFFTEST_TO_REF };


typedef struct context {
  uint32_t gpr[GPR_NUM];
  uint32_t csr[CSR_NUM];
  uint32_t pc;
} core_context_t;

// Note: The layout of this struct must be kept consistent with the SystemVerilog definition.
// #pragma pack(push, 1)
// typedef struct _diff_info_t {
//   uint16_t irq_en;
//   uint16_t csr_waddr;
//   uint32_t csr_wdata;
//   uint16_t csr_wen;
//   uint16_t skip;
//   uint16_t commit;
//   uint32_t wdata;
//   uint16_t wen;
//   uint16_t rdIdx;
//   uint32_t inst;
//   uint32_t npc;
//   uint32_t pc;
// } difftest_info_t; 
// #pragma pack(pop)

#pragma pack(push, 1)
typedef struct {
  uint32_t pc;
  uint32_t npc;
  uint32_t inst;
  uint16_t rdIdx;
  uint16_t wen;
  uint32_t wdata;
  uint16_t commit;
  uint16_t skip;
  uint16_t csr_wen;
  uint32_t csr_wdata;
  uint16_t csr_waddr;
  uint16_t irq_en;
} difftest_info_t;
#pragma pack(pop)

// _Static_assert(sizeof(difftest_info_t) == 36, "difftest_info_t must be 36 bytes");


#ifdef __cplusplus
extern "C" {
#endif

void difftest_init(const Vtop *p, const char *ref_so_file, const char *img_file);

#ifdef __cplusplus
}
#endif
#endif
