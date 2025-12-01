#ifndef __AMUNIT_H__
#define __AMUNIT_H__

#include <am.h>
#include <klib.h>
#include <klib-macros.h>

#define IOE ({ ioe_init();  })
#define CTE(h) ({ Context *h(Event, Context *); cte_init(h); })

extern void (*entry)();

#define CASE(id, entry_, ...) \
  case id: { \
    void entry_(); \
    entry = entry_; \
    __VA_ARGS__; \
    entry_(); \
    break; \
  }

#endif
