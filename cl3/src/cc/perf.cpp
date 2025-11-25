#include <svdpi.h>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <dlfcn.h>
#include <vector>

#ifdef __cplusplus
extern "C" {
#endif

struct ICachePerfSnapshot {
  uint64_t req_count;
  uint64_t hit_count;
  uint64_t miss_count;
  uint64_t refill_burst_count;
  uint64_t refill_beat_count;
  uint64_t refill_line_count;
  uint64_t stall_cycle_count;
  uint64_t miss_penalty_cycles;
  uint64_t flush_count;
  uint64_t invalidate_count;
  uint64_t axi_err_count;
};

static ICachePerfSnapshot g_icache_perf = {};

// Clear all cached counters in the host snapshot.

// Update the cached counters from SV. way_replace_counts is a 1-D open array
// of 64-bit values (one per way).
void perf_icache_show(uint64_t req_count, uint64_t miss_count, uint64_t refill_burst_count,
                        uint64_t refill_beat_count, uint64_t refill_line_count,
                        uint64_t stall_cycle_count,
                        uint64_t miss_penalty_cycles,
                        uint64_t axi_backpress_cycles, uint64_t flush_count,
                        uint64_t invalidate_count, uint64_t axi_err_count) {
  g_icache_perf.req_count = req_count;
  g_icache_perf.hit_count = req_count - miss_count;
  g_icache_perf.miss_count = miss_count;
  g_icache_perf.refill_burst_count = refill_burst_count;
  g_icache_perf.refill_beat_count = refill_beat_count;
  g_icache_perf.refill_line_count = refill_line_count;
  g_icache_perf.stall_cycle_count = stall_cycle_count;
  g_icache_perf.miss_penalty_cycles = miss_penalty_cycles;
  g_icache_perf.flush_count = flush_count;
  g_icache_perf.invalidate_count = invalidate_count;
  g_icache_perf.axi_err_count = axi_err_count;
  
  const auto &p = g_icache_perf;

  double hit_rate  = p.req_count ? (double)p.hit_count  * 100.0 / p.req_count : 0.0;
  double miss_rate = p.req_count ? (double)p.miss_count * 100.0 / p.req_count : 0.0;

  std::printf(
      "\n========== ICache Performance ==========\n"
      "  Requests         : %10lu\n"
      "  Hits             : %10lu  (%.2f%%)\n"
      "  Misses           : %10lu  (%.2f%%)\n"
      "  Refill bursts    : %10lu\n"
      "  Refill beats     : %10lu\n"
      "  Refill lines     : %10lu\n"
      "  Stall cycles     : %10lu\n"
      "  Miss penalty cyc : %10lu\n"
      "  Flushes          : %10lu\n"
      "  Invalidates      : %10lu\n"
      "========================================\n",
      p.req_count,
      p.hit_count,           hit_rate,
      p.miss_count,          miss_rate,
      p.refill_burst_count,
      p.refill_beat_count,
      p.refill_line_count,
      p.stall_cycle_count,
      p.miss_penalty_cycles,
      p.flush_count,
      p.invalidate_count
  );

}

// ========================= DCache perf =========================
struct DCachePerfSnapshot {
  uint64_t req_count;
  uint64_t read_req_count;
  uint64_t write_req_count;
  uint64_t hit_count;
  uint64_t miss_count;
  uint64_t refill_beat_count;
  uint64_t refill_line_count;
  uint64_t miss_penalty_cycles;
  uint64_t writeback_count;
  uint64_t amo_count;
};

static DCachePerfSnapshot g_dcache_perf = {};

// Update and print DCache counters.
void perf_dcache_show(uint64_t req_count,
                      uint64_t read_req_count,
                      uint64_t write_req_count,
                      uint64_t miss_count,
                      uint64_t refill_beat_count,
                      uint64_t refill_line_count,
                      uint64_t miss_penalty_cycles,
                      uint64_t writeback_count,
                      uint64_t amo_count) {
  g_dcache_perf.req_count           = req_count;
  g_dcache_perf.read_req_count      = read_req_count;
  g_dcache_perf.write_req_count     = write_req_count;
  g_dcache_perf.hit_count           = req_count - miss_count;
  g_dcache_perf.miss_count          = miss_count;
  g_dcache_perf.refill_beat_count   = refill_beat_count;
  g_dcache_perf.refill_line_count   = refill_line_count;
  g_dcache_perf.miss_penalty_cycles = miss_penalty_cycles;
  g_dcache_perf.writeback_count     = writeback_count;
  g_dcache_perf.amo_count           = amo_count;

  const auto &p = g_dcache_perf;
  double miss_rate = p.req_count ? (double)p.miss_count * 100.0 / p.req_count : 0.0;
  double hit_rate  = p.req_count ? (double)(p.hit_count) * 100.0 / p.req_count : 0.0;

  std::printf(
      "\n========== DCache Performance ==========\n"
      "  Requests         : %10lu (read=%lu, write=%lu)\n"
      "  Hits             : %10lu  (%.2f%%)\n"
      "  Misses           : %10lu  (%.2f%%)\n"
      "  Refill beats     : %10lu\n"
      "  Refill lines     : %10lu\n"
      "  Miss penalty cyc : %10lu\n"
      "  Writebacks       : %10lu\n"
      "  Atomic ops       : %10lu\n"
      "========================================\n",
      p.req_count, p.read_req_count, p.write_req_count,
      p.hit_count, hit_rate,
      p.miss_count, miss_rate,
      p.refill_beat_count,
      p.refill_line_count,
      p.miss_penalty_cycles,
      p.writeback_count,
      p.amo_count);
}

#ifdef __cplusplus
}
#endif
