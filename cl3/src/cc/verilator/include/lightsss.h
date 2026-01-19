/***************************************************************************************
* Copyright (c) 2020-2023 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* DiffTest is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

#ifndef __LIGHTSSS_H
#define __LIGHTSSS_H

#include <deque>
#include <list>
#include <signal.h>
#include <sys/ipc.h>
#include <sys/prctl.h>
#include <sys/shm.h>
#include <sys/wait.h>
#include <unistd.h>
#include <cstdint>
#include <cassert>
#include <cerrno>
#include <cinttypes>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sys/mman.h>
#include <sys/time.h>
#include <functional>
#include <stdexcept>

#define FAIT_EXIT    exit(EXIT_FAILURE);
#define WAIT_INTERVAL 5
#define SLOT_SIZE 2

typedef struct shinfo {
  bool flag;
  bool notgood;
  uint64_t endCycles;
  pid_t oldest;
} shinfo;

class ForkShareMemory {
private:
  key_t key_n;
  int shm_id;

public:
  shinfo *info;

  ForkShareMemory();
  ~ForkShareMemory();

  void shwait();
};

const int FORK_OK = 0;
const int FORK_ERROR = 1;
const int FORK_CHILD = 2;

class LightSSS {
  pid_t pid = -1;
  int slotCnt = 0;
  int waitProcess = 0;
  // front() is the newest. back() is the oldest.
  std::deque<pid_t> pidSlot = {};
  ForkShareMemory forkshm;

public:
  int do_fork();
  int wakeup_child(uint64_t cycles);
  bool is_child();
  int do_clear();
  uint64_t get_end_cycles() {
    return forkshm.info->endCycles;
  }
};

int eprintf(const char *fmt, ...);
typedef int (*eprintf_handle_t)(const char *fmt, va_list ap);

extern bool sim_verbose;
#define Info(...)           \
  do {                      \
    if (sim_verbose) {      \
      eprintf(__VA_ARGS__); \
    }                       \
  } while (0)

#define FORK_PRINTF(format, args...)                       \
  do {                                                     \
    Info("[FORK_INFO pid(%d)] " format, getpid(), ##args); \
    fflush(stdout);                                        \
  } while (0);

#endif
