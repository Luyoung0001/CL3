// Verilated -*- C++ -*-
// DESCRIPTION: main() calling loop, created with Verilator --main

#include "Vtop.h"
#include "verilated.h"
#include "verilated_fst_c.h"
#include "Vtop___024root.h"
#include <difftest.h>
#include <getopt.h>
#include <iostream>
#include "lightsss.h"

static struct option long_options[] = {
    {"ref", required_argument, nullptr, 'r'},
    {"image", required_argument, nullptr, 'i'},
    {"help", no_argument, nullptr, 'h'},
    {"diff", no_argument, nullptr, 'd'},
    {"lightsss", no_argument, nullptr, 's'},
    {"fork-interval", required_argument, nullptr, 'I'},
    {nullptr, 0, nullptr, 0}};

void print_usage(const char *prog_name) {
  std::cerr << "Usage: " << prog_name << " [options]\n"
            << "Options:\n"
            << "  -r, --ref <file>     Reference file\n"
            << "  -i, --image <file>   Image file\n"
            << "  -d, --diff           Enable difftest\n"
            << "  -s, --lightsss       Enable LightSSS (fork snapshot)\n"
            << "  -I, --fork-interval <cycles>  Fork interval cycles for LightSSS\n"
            << "  -h, --help           Show this help message\n";
}

static LightSSS lightsss;
static bool fork_enable = false;
static uint64_t fork_interval_cycles = 100000; // 100 thousands cycles
static uint64_t last_fork_cycle = 0;
static bool fork_started = false;
static VerilatedFstC* tfp = nullptr;
static bool trace_opened = false;

int main(int argc, char **argv, char **) {

  // Setup context, defaults, and parse command line
  Verilated::debug(0);
  const std::unique_ptr<VerilatedContext> contextp{new VerilatedContext};
  contextp->traceEverOn(true);
  contextp->commandArgs(argc, argv);
  // Construct the Verilated model, from Vtop.h generated from Verilating
  const std::unique_ptr<Vtop> topp{new Vtop{contextp.get(), ""}};

  const char *ref_file = nullptr;
  const char *img_file = nullptr;
  bool diff_enable = false;

  int opt;
  int option_idx = 0;
  while ((opt = getopt_long(argc, argv, "r:i:hd", long_options, &option_idx)) !=
         -1) {
    switch (opt) {
    case 'r':
      ref_file = optarg;
      break;
    case 'i':
      img_file = optarg;
      break;
    case 'h':
      print_usage(argv[0]);
      return 0;
    case 'd':
      diff_enable = true;
      break;
    case 's':
      fork_enable = true;
      break;
    case 'I':
      fork_interval_cycles = (uint32_t)strtoull(optarg, nullptr, 10);
      break;
    case '?':
      print_usage(argv[0]);
      return 1;
    default:
      std::cerr << "Unknown error while parsing options\n";
      return 1;
    }
  }

  if (diff_enable) {
    difftest_init(topp.get(), ref_file, img_file);
  }

  // Simulate until $finish
  try {
    while (!contextp->gotFinish()) {
      // Evaluate model
      topp->eval();
      // Advance time
      if (!topp->eventsPending())
        break;
      contextp->time(topp->nextTimeSlot());
      
      uint64_t cur_cycle = topp->rootp->top__DOT__cnt;
      if(fork_enable){
        if (!fork_started && topp->rootp->top__DOT__rst_n) {
          fork_started = true;
          if (!lightsss.is_child()) {
            lightsss.do_fork();
            last_fork_cycle = cur_cycle;
          }
        }

        if (fork_started && !lightsss.is_child()) {
          if (cur_cycle - last_fork_cycle >= fork_interval_cycles) {
            lightsss.do_fork();
            last_fork_cycle = cur_cycle;
          }
        }
        if (lightsss.is_child() && !trace_opened){
          trace_opened = true;
          tfp = new VerilatedFstC;
          topp->trace(tfp, 5);
          char fname[256];
          snprintf(fname, sizeof(fname), "wave/top_child_%d.fst", getpid());
          tfp->open(fname);

          fprintf(stderr, "[CHILD] FST trace opened: %s (start cycle=%lu)\n",
                  fname, cur_cycle);
        }
      }

      if (trace_opened && tfp) {
        tfp->dump(contextp->time());
      }
    }
  } catch (const std::exception& e) {
    uint64_t cycles = topp->rootp->top__DOT__cnt;

    fprintf(stderr, "[%s] fatal at cycle=%lu pid=%d: %s\n",
          lightsss.is_child() ? "CHILD" : "PARENT",
          cycles, getpid(), e.what());
    
    if (fork_enable && !lightsss.is_child()) {
      lightsss.wakeup_child(cycles);
      printf("parnet process exit after waking up child at cycle=%lu\n",
             cycles);
    }
  }

  if (!contextp->gotFinish()) {
    VL_DEBUG_IF(VL_PRINTF("+ Exiting without $finish; no events left\n"););
  }
  // Execute 'final' processes
  if (fork_enable && !lightsss.is_child()) {
    lightsss.do_clear();
  }
  if (fork_enable && lightsss.is_child()) {
    if (tfp) { tfp->close(); delete tfp; tfp=nullptr; }
    fflush(stdout); fflush(stderr);
    _exit(0);
  }
  printf("Simulation finished at time %u cycles!\n", topp->rootp->top__DOT__cnt);
  topp->final();

  // Print statistical summary report
  contextp->statsPrintSummary();
  return 0;
}
