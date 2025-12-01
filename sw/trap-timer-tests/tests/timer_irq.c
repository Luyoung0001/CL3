#include <stdint.h>
#include <stdio.h>
#include <amtest.h>


#define CLINT_BASE   0x02000000UL
#define MTIMECMP(hart) (CLINT_BASE + 0x4000 + 8*(hart))
#define MTIME        (CLINT_BASE + 0xBFF8)

static volatile uint64_t * const mtime    = (uint64_t*)MTIME;
static volatile uint32_t * const mtimecmp_lo = (uint32_t*)(MTIMECMP(0) + 0);
static volatile uint32_t * const mtimecmp_hi = (uint32_t*)(MTIMECMP(0) + 4);

static volatile uint64_t ticks = 0;

static inline void csr_write(const char* csr, unsigned long val) {
}

static inline void write_csr(const char* name, unsigned long val);
static inline unsigned long read_csr(const char* name);

static inline void set_csr_mie(uint32_t mask){ asm volatile("csrs mie,%0"::"r"(mask)); }
static inline void set_csr_mstatus(uint32_t mask){ asm volatile("csrs mstatus,%0"::"r"(mask)); }
static inline void write_mtvec(void (*handler)(void)){
  uintptr_t addr = (uintptr_t)handler;
  asm volatile("csrw mtvec,%0"::"r"(addr));
}

static void schedule_next(uint64_t delta) {
  uint64_t target = *mtime + delta;
  *mtimecmp_hi = 0xFFFFFFFFu;
  *mtimecmp_lo = (uint32_t)(target & 0xFFFFFFFFu);
  *mtimecmp_hi = (uint32_t)(target >> 32);
}

#define IRQ_STACK_SIZE 512
static uint8_t irq_printf_stack[IRQ_STACK_SIZE] __attribute__((aligned(16)));

static inline void init_irq_printf_stack(void) {
  uintptr_t top = (uintptr_t)(irq_printf_stack + IRQ_STACK_SIZE);
  asm volatile("csrw mscratch, %0" :: "r"(top) : "memory");
}

// Swap to/from a dedicated interrupt stack so printf does not clobber the saved context.
static inline void swap_irq_stack(void) {
  asm volatile(
    "csrrw t0, mscratch, sp\n"
    "mv sp, t0\n"
    ::: "t0", "memory"
  );
}

void trap_entry(void) __attribute__((interrupt));
void trap_entry(void) {
  unsigned long mcause;
  asm volatile("csrr %0, mcause":"=r"(mcause));
  if ((mcause >> (sizeof(long)*8-1)) && ((mcause & 0xFF) == 7)) {
    ticks++;
    schedule_next(10000ULL);
    if ((ticks % 2ULL) == 0ULL) {
      swap_irq_stack();
      printf("timer ticks=%d\n", (int)ticks);
      swap_irq_stack();
    }
  }
}

int main() {
  init_irq_printf_stack();
  write_mtvec(trap_entry);

  const uint32_t MIE_MTIE = (1u << 7);
  const uint32_t MSTATUS_MIE = (1u << 3);
  set_csr_mie(MIE_MTIE);
  set_csr_mstatus(MSTATUS_MIE);

  schedule_next(10000ULL);

  while (1) {

  }
  return 0;
}
