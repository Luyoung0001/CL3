#include <stdint.h>
#include <stddef.h>
#include <stdalign.h>
#include <amtest.h>

// ====== CSR helpers ======
static inline unsigned long read_csr(const char* name){
  unsigned long val; (void)name; return 0;
}
static inline unsigned long csr_read_mcause(){ unsigned long x; asm volatile("csrr %0, mcause":"=r"(x)); return x; }
static inline unsigned long csr_read_mepc()  { unsigned long x; asm volatile("csrr %0, mepc"  :"=r"(x)); return x; }
static inline unsigned long csr_read_mtval() { unsigned long x; asm volatile("csrr %0, mtval" :"=r"(x)); return x; }
static inline void csr_write_mtvec(void (*handler)(void)){
  uintptr_t addr = (uintptr_t)handler; asm volatile("csrw mtvec, %0"::"r"(addr));
}
static inline void csr_write_mepc(uintptr_t pc){ asm volatile("csrw mepc, %0"::"r"(pc)); }

// ====== exception log ======
typedef struct {
  unsigned long mcause;
  unsigned long mepc;
  unsigned long mtval;
} exc_log_t;

#define LOG_MAX 16
volatile exc_log_t g_logs[LOG_MAX];
volatile int g_log_cnt = 0;

volatile uintptr_t g_resume_pc = 0;

// ====== Trap handler (Direct mode) ======
void trap_entry(void) __attribute__((interrupt));
void trap_entry(void) {
  unsigned int mcause = csr_read_mcause();
  unsigned int mepc   = csr_read_mepc();
  unsigned int mtval  = csr_read_mtval();

  int i = g_log_cnt;
  if (i < LOG_MAX) {
    g_logs[i].mcause = mcause;
    g_logs[i].mepc   = mepc;
    g_logs[i].mtval  = mtval;
    g_log_cnt = i + 1;
  }
  printf("#%d mcause=0x%x mepc=0x%x mtval=0x%x\n",
            i, g_logs[i].mcause, g_logs[i].mepc, g_logs[i].mtval);

  if(mcause == 1) 
    csr_write_mepc(g_resume_pc);
  else
    csr_write_mepc(mepc + 4);
}

static void test_instr_misaligned(void) {
    extern void after_instr_fault(void);
    g_resume_pc = (uintptr_t)&&after_instr_fault;

    uintptr_t base = (uintptr_t)&&here_instr;
    here_instr: ;

    uintptr_t bad = base | 0x1;

    asm volatile (
        "mv a0, %0 \n"
        "jalr x0, a0, 0 \n"
        :
        : "r"(bad)
        : "a0"
    );

after_instr_fault:
  g_resume_pc = 0;
}

static void test_load_misaligned(void) {
  extern void after_load_fault(void);
  g_resume_pc = (uintptr_t)&&after_load_fault;

  alignas(4) static volatile uint8_t buf[16];
  uintptr_t p = ((uintptr_t)buf) | 0x1;

  register unsigned int tmp;
  asm volatile (
    "lw %0, 0(%1)\n"
    : "=r"(tmp)
    : "r"(p)
  );

after_load_fault:
  (void)0;
  g_resume_pc = 0;
}

static void test_store_misaligned(void) {
  extern void after_store_fault(void);
  g_resume_pc = (uintptr_t)&&after_store_fault;

  alignas(4) static volatile uint8_t buf[16];
  uintptr_t p = ((uintptr_t)buf) | 0x1;

  register unsigned int val = 0x12345678u;
  asm volatile (
    "sw %0, 0(%1)\n"
    :
    : "r"(val), "r"(p)
  );

after_store_fault:
  (void)0;
  g_resume_pc = 0;
}

static void test_ebreak(void) {
  extern void after_ebreak(void);
  g_resume_pc = (uintptr_t)&&after_ebreak;

  asm volatile ("ebreak");

after_ebreak:
  g_resume_pc = 0;
}

static void test_illegal(void) {
  extern void after_illegal(void);
  g_resume_pc = (uintptr_t)&&after_illegal;

  asm volatile (
    ".balign 4        \n"
    ".word 0xFFFFFFFF \n"
  );

after_illegal:
  g_resume_pc = 0;
}

static void test_load_access_fault(void) {
    extern void after_load_access(void);
    g_resume_pc = (uintptr_t)&&after_load_access;

    volatile uint32_t *bad = (uint32_t *)0x40000000UL; // unmapped
    asm volatile ("lw x0, 0(%0)" :: "r"(bad));

  after_load_access:
    g_resume_pc = 0;
  }

  static void test_store_access_fault(void) {
    extern void after_store_access(void);
    g_resume_pc = (uintptr_t)&&after_store_access;

    volatile uint32_t *bad = (uint32_t *)0x40000000UL;
    asm volatile ("sw %0, 0(%1)" :: "r"(0x12345678u), "r"(bad));

  after_store_access:
    g_resume_pc = 0;
  }

static void test_instr_access_fault(void) __attribute__((noinline));
static void test_instr_access_fault(void) {
  extern void after_instr_access(void);

  register uintptr_t bad = 0x40000000u;
  asm volatile (
    "la t0, 1f       \n"
    "sw t0, (%[dst]) \n"
    "jalr x0, %[bad], 0 \n"
    "1:                  \n"
    :
    : [dst]"r"(&g_resume_pc), [bad]"r"(bad)
    : "t0", "memory"
  );

after_instr_access:
  g_resume_pc = 0;
}

int main(void){
    csr_write_mtvec(trap_entry);

    // 逐个触发
    // test_instr_misaligned();
    test_instr_access_fault();
    test_illegal();
    test_ebreak();
    test_load_access_fault();
    test_load_misaligned();
    test_store_misaligned();
    test_store_access_fault();

    return 0;
}
