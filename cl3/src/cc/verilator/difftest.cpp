#include <Vtop.h>
#include <Vtop__Dpi.h>
#include <Vtop___024root.h>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <difftest.h>
#include <dlfcn.h>
#include <svdpi.h>

#include <pthread.h>
#include <sys/ioctl.h>
#include <unistd.h>

#ifdef __cplusplus
extern "C" {
#endif

static core_context_t dut;
static core_context_t ref;
static const Vtop *topp;

static void (*ref_difftest_memcpy)(unsigned int addr, void *buf, size_t n,
                                   bool direction) = NULL;
static void (*ref_difftest_regcpy)(void *dut, bool direction) = NULL;
static void (*ref_difftest_exec)(uint64_t n) = NULL;

static uint8_t mem[PMEM_SIZE];

static unsigned int load_img(const char *path) {
  assert(mem);
  assert(path);
  FILE *img = fopen(path, "rb");
  assert(img);

  fseek(img, 0, SEEK_END);
  unsigned int size = ftell(img);
  if (size > PMEM_SIZE) {
    fclose(img);
    return 0;
  }
  fseek(img, 0, SEEK_SET);
  int ret = fread(mem, size, 1, img);
  if (ret != 1) {
    fclose(img);
    return 0;
  }

  fclose(img);
  return size;
}

static inline void *pmem_ptr(uint32_t paddr) {
  assert(paddr >= RESET_VECTOR);
  uint32_t off = paddr - RESET_VECTOR;
  assert(off < PMEM_SIZE);
  return (void *)(mem + off);
}

static long load_file_to_pmem(const char *path, uint32_t paddr) {
  assert(mem);
  assert(path);

  FILE *fp = fopen(path, "rb");
  if (!fp) return -1;

  fseek(fp, 0, SEEK_END);
  long len = ftell(fp);
  fseek(fp, 0, SEEK_SET);

  // Boundary check: cannot exceed pmem window.
  if ((uint64_t)paddr < (uint64_t)RESET_VECTOR ||
      (uint64_t)paddr + (uint64_t)len > (uint64_t)RESET_VECTOR + (uint64_t)PMEM_SIZE) {
    fclose(fp);
    return 0;
  }

  void *dst = pmem_ptr(paddr);
  int ret = fread(dst, len, 1, fp);
  assert(ret == 1);

  fclose(fp);
  return len;
}

// Verilator only supports the longint DPI-C type for 64bit data.
long long mem_read(uint32_t raddr, uint32_t size) {

  if (raddr < RESET_VECTOR || (raddr - RESET_VECTOR >= PMEM_SIZE)) {
    // printf("raddr is %x\n", raddr);
    // assert(0);

    return 0;
    //  assert(0);
  }

  uint32_t aligned_addr = raddr & 0xfffffffc;
  uint32_t v0 = *(uint32_t *)(mem + aligned_addr - RESET_VECTOR);
  int64_t v1 = *(int64_t *)(mem + aligned_addr - RESET_VECTOR);

  return size == 3 ? v1 : v0;
}

void mem_write(uint32_t waddr, uint32_t mask, uint32_t wdata) {
  uint8_t *addr = mem + waddr - RESET_VECTOR;
  if (waddr < RESET_VECTOR || (waddr - RESET_VECTOR >= PMEM_SIZE)) {
    printf("waddr: %#x, mask:%x, data: %#x\n", waddr, mask, wdata);
    return;
  }
  switch (mask) {
  case 0x1:
    *(volatile uint8_t *)addr = wdata;
    break;
  case 0x2:
    *(volatile uint8_t *)addr = wdata >> 8;
    break;
  case 0x4:
    *(volatile uint8_t *)addr = wdata >> 16;
    break;
  case 0x8:
    *(volatile uint8_t *)addr = wdata >> 24;
    break;
  case 0x3:
    *(volatile uint16_t *)addr = wdata;
    break;
  case 0xc:
    *(volatile uint16_t *)addr = wdata >> 16;
    break;
  case 0xf:
    *(volatile uint32_t *)addr = wdata;
    break;
  default:
    assert(0);
  }
}


void difftest_init(const Vtop *p, const char *ref_so_file,
                   const char *img_file) {
// void difftest_init(const char *ref_so_file, const char *img_file) { // vcs
  printf("[DIFFTEST] init ...\n");
  printf("%s\n", img_file);
  topp = p;
  assert(topp);
  // Initialize all difftest function pointers
  void *handle;
  handle = dlopen(ref_so_file, RTLD_LAZY);
  assert(handle);
  ref_difftest_memcpy = DIFF_MEMCPY dlsym(handle, "difftest_memcpy");
  assert(ref_difftest_memcpy);
  ref_difftest_regcpy = DIFF_REGCPY dlsym(handle, "difftest_regcpy");
  assert(ref_difftest_regcpy);
  ref_difftest_exec = DIFF_EXEC dlsym(handle, "difftest_exec");
  void (*ref_difftest_init)(int) = DIFF_INIT dlsym(handle, "difftest_init");
  assert(ref_difftest_init);
  ref_difftest_init(80); // Don't care the port

  // Load image file and copy to REF
  size_t size = load_img(img_file);
  ref_difftest_memcpy(RESET_VECTOR, mem, size, DIFFTEST_TO_REF);

  // Optional DTB injection for environments that provide a DTB image.
  const char *dtb_file = getenv("CL3_DTB_FILE");
  const uint32_t DTB_ADDR = 0x80fff9f0;
  if (dtb_file && dtb_file[0]) {
    long dtb_len = load_file_to_pmem(dtb_file, DTB_ADDR);
    if (dtb_len > 0) {
      ref_difftest_memcpy(DTB_ADDR, pmem_ptr(DTB_ADDR), (size_t)dtb_len, DIFFTEST_TO_REF);
      ref.gpr[11] = DTB_ADDR;
      printf("[DIFFTEST] loaded dtb: %s (%ld bytes) @ %#x\n", dtb_file, dtb_len, DTB_ADDR);
    } else {
      printf("[DIFFTEST] WARN: failed to load dtb from %s, continue without dtb\n", dtb_file);
    }
  } else {
    printf("[DIFFTEST] dtb disabled (set CL3_DTB_FILE to enable)\n");
  }

  // Initialize DUT and local REF state
  dut.pc = RESET_VECTOR;
  dut.csr[3] = 0x1800;
  ref.pc = RESET_VECTOR;
  ref.csr[3] = 0x1800;
  // CL3 boots with a1 pointing to DTB address convention.
  ref.gpr[11] = DTB_ADDR;

  // Initialize remote REF state
  ref_difftest_regcpy((void *)&ref, DIFFTEST_TO_REF);
  printf("[DIFFTEST] finish initialization\n");
  printf("[DIFFTEST] image name: %s, image size : %ld\n", img_file, size);


}


void update_dut_state(){
  GET_ALL_GPR
  GET_ALL_CSR
}

void update_dut_csr(int wen, uint16_t waddr, uint32_t wdata) {
  if (wen) {
    switch (waddr) {
      case MEPC:
        dut.csr[0] = wdata;
        break;
      case MCAUSE:
        dut.csr[1] = wdata;
        break;
      case MTVEC:
        dut.csr[2] = wdata;
        break;
      case MSTATUS:
        dut.csr[3] = wdata;
        break;
      case MTVAL:
        dut.csr[4] = wdata;
        break;
      default:
        printf("CSR write addr error: %x\n", waddr);
        break;
    }
  }
  
}

static const char *gpr_name(int i) {
  static const char *names[32] = {
    "x0/zero","x1/ra","x2/sp","x3/gp","x4/tp",
    "x5/t0","x6/t1","x7/t2",
    "x8/s0","x9/s1",
    "x10/a0","x11/a1","x12/a2","x13/a3","x14/a4","x15/a5","x16/a6","x17/a7",
    "x18/s2","x19/s3","x20/s4","x21/s5","x22/s6","x23/s7","x24/s8","x25/s9","x26/s10","x27/s11",
    "x28/t3","x29/t4","x30/t5","x31/t6"
  };
  return (i >= 0 && i < 32) ? names[i] : "x?";
}

static void dump_all_regs(const char *tag,
                          const core_context_t *dut,
                          const core_context_t *ref) {

  printf(COLOR_RED "[DIFFTEST] ===== GPR (DUT vs REF) =====\n" COLOR_END);
  for (int i = 0; i < 32; i++) {
    uint32_t d = dut->gpr[i];
    uint32_t r = ref->gpr[i];
    if (d != r) {
      printf(COLOR_RED "  %-8s : DUT=%#010x  REF=%#010x  <== MISMATCH\n" COLOR_END,
             gpr_name(i), d, r);
    } else {
      printf("  %-8s : DUT=%#010x  REF=%#010x\n", gpr_name(i), d, r);
    }
  }
}

static inline uint32_t* arr_u32(svOpenArrayHandle h) { return (uint32_t*)svGetArrayPtr(h); }
static inline uint16_t* arr_u16(svOpenArrayHandle h) { return (uint16_t*)svGetArrayPtr(h); }

int difftest_step(
  int n,
  svOpenArrayHandle pc_h,
  svOpenArrayHandle npc_h,
  svOpenArrayHandle inst_h,
  svOpenArrayHandle rdIdx_h,
  svOpenArrayHandle wen_h,
  svOpenArrayHandle wdata_h,
  svOpenArrayHandle commit_h,
  svOpenArrayHandle skip_h,
  svOpenArrayHandle csr_wen_h,
  svOpenArrayHandle csr_wdata_h,
  svOpenArrayHandle csr_waddr_h,
  svOpenArrayHandle irq_en_h
) {
  uint32_t *pc      = arr_u32(pc_h);
  uint32_t *npc     = arr_u32(npc_h);
  uint32_t *inst    = arr_u32(inst_h);
  uint16_t *rdIdx   = arr_u16(rdIdx_h);
  uint16_t *wen     = arr_u16(wen_h);
  uint32_t *wdata   = arr_u32(wdata_h);
  uint16_t *commit  = arr_u16(commit_h);
  uint16_t *skip    = arr_u16(skip_h);
  uint16_t *csr_wen = arr_u16(csr_wen_h);
  uint32_t *csr_wdata = arr_u32(csr_wdata_h);
  uint16_t *csr_waddr = arr_u16(csr_waddr_h);
  uint16_t *irq_en  = arr_u16(irq_en_h);
  bool saw_commit = false;

  for (int i = 0; i < n; i++) {
    if (!commit[i]) continue;
    saw_commit = true;

    if (skip[i] || csr_waddr[i] == 0xf12 || csr_waddr[i] == 0xf13 || irq_en[i]) {
      update_dut_state();
      uint32_t last_npc = npc[i];
      for (int j = i + 1; j < n; j++) {
        if (commit[j]) {
          last_npc = npc[j];
        }
      }
      dut.pc = last_npc;
      ref_difftest_regcpy((void *)&dut, DIFFTEST_TO_REF);
      ref_difftest_regcpy((void *)&ref, DIFFTEST_TO_DUT);
      break;
    }

    uint32_t expect_npc = npc[i];

    ref_difftest_exec(1);
    ref_difftest_regcpy((void *)&ref, DIFFTEST_TO_DUT);
    int ret = 0;
    if (ref.pc != expect_npc) {
      printf(COLOR_RED "[DIFFTEST] PC mismatch at DUT pc=%#x: DUT npc=%#x REF pc=%#x\n" COLOR_END,
             pc[i], expect_npc, ref.pc);
      ret = 1;
    }

    if (wen[i] && rdIdx[i] != 0 && wdata[i] != ref.gpr[rdIdx[i]]) {
      printf(COLOR_RED "[DIFFTEST] GPR mismatch at pc=%#x: rd=%u DUT=%#x REF=%#x\n" COLOR_END,
             pc[i], rdIdx[i], wdata[i], ref.gpr[rdIdx[i]]);
      ret = 1;
    }
    if(ret == 1){
      update_dut_state();
      for (int i = 0; i < n; i++) {
        printf("[DIFFTEST] slot[%d]: commit=%u irq_en=%u pc=%#x npc=%#x inst=%#x\n",
              i, (unsigned)commit[i], (unsigned)irq_en[i], pc[i], npc[i], inst[i]);
      }
      dump_all_regs("DOUBLE_CHECK_FAIL", &dut, &ref);
      return 1;
    }

  }
  // Startup/warmup cycles may have no commits yet; skip strict state check.
  if (!saw_commit) {
    return 0;
  }
    // printf("[DIFF][%d] irq_en=%u csr_waddr=0x%04x csr_wdata=0x%08x csr_wen=%u "
    //        "skip=%u commit=%u wdata=0x%08x wen=%u rdIdx=%u inst=0x%08x npc=0x%08x pc=0x%08x\n",
    //        i, irq_en[i], csr_waddr[i], csr_wdata[i], csr_wen[i],
    //        skip[i], commit[i], wdata[i], wen[i], rdIdx[i], inst[i], npc[i], pc[i]);
  update_dut_state();

  int gpr_mask = 0;
  for (int i = 0; i < GPR_NUM; i++) {
    if (dut.gpr[i] != ref.gpr[i]) {
      printf(COLOR_RED
             "[DIFFTEST] GPR[%d]: DUT=%0#x REF=%0#x\n"
             COLOR_END,
             i, dut.gpr[i], ref.gpr[i]);
      gpr_mask |= (1U << i);
    }
  }

  int csr_mask = 0;
  for (int i = 0; i < 8; i++) {
    if (i == 5) continue; // skip mip
    if (dut.csr[i] != ref.csr[i]) {
      printf(COLOR_RED
             "[DIFFTEST] CSR[%d]: DUT=%0#x REF=%0#x\n"
             COLOR_END,
             i, dut.csr[i], ref.csr[i]);
      csr_mask |= (1U << (i + CSR_NUM));
    }
  }

  if (gpr_mask | csr_mask) {
    printf(COLOR_RED
           "[DIFFTEST] Mismatch in double check: DUT state changed unexpectedly.\n"
           COLOR_END);
    for (int i = 0; i < n; i++) {
      printf("[DIFFTEST] slot[%d]: commit=%u irq_en=%u pc=%#x npc=%#x inst=%#x\n",
            i, (unsigned)commit[i], (unsigned)irq_en[i], pc[i], npc[i], inst[i]);
    }
    dump_all_regs("DOUBLE_CHECK_FAIL", &dut, &ref);
    return 1;
  }

  return 0;
}

#ifdef __cplusplus
}
#endif


static int is_eofd;

static int ReadKBByte()
{
	if( is_eofd ) return 0xffffffff;
	char rxchar = 0;
	int rread = read(fileno(stdin), (char*)&rxchar, 1);

	if( rread > 0 ) // Tricky: getchar can't be used with arrow keys.
		return rxchar;
	else
		return -1;
}

static int IsKBHit()
{
	if( is_eofd ) return -1;
	int byteswaiting;
	ioctl(0, FIONREAD, &byteswaiting);
	if( !byteswaiting && write( fileno(stdin), 0, 0 ) != 0 ) { is_eofd = 1; return -1; } // Is end-of-file for 
	return !!byteswaiting;
}

uint32_t uart_read(uint32_t raddr, unsigned char valid){
  if(valid){
    if( raddr == 0x10000004 )
      return (0x60 | IsKBHit()) << 8;
    else if( raddr == 0x10000000 && IsKBHit() )
      return ReadKBByte();
  }
  return 0;
}

void uart_write(uint32_t waddr, unsigned char valid, uint32_t wdata){
  if(valid){
    if( waddr == 0x10000000 ) //UART 8250 / 16550 Data Buffer
    {
      printf( "%c", wdata );
      fflush( stdout );
    }
  }
  return;
}
