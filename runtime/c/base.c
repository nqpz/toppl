#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <getopt.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctype.h>
#include <stdarg.h>
#include <time.h>
#include <sys/time.h>

#define VALUE_NO_CONSTRAINTS -1
#define VALUE_NO_PART -2
#define VALUE_INDIRECT -3 /* part1 contains the indirection */
#define VALUE_SUCCEED -4
#define VALUE_FAIL -5

typedef struct {
  int32_t id;
  int32_t part1;
  int32_t part2;
} value_t;

typedef struct {
  char* name;
  int n_params;
} predicate_t;

typedef struct {
  value_t* cells;
  size_t n_cells;
} memory_t;

typedef struct {
  char* variable;
  int32_t address;
} var_addr_mapping_t;

typedef struct {
  var_addr_mapping_t* mappings;
  size_t n_mappings;
} va_mappings_t;

typedef struct {
  int32_t address;
  char* string;
} addr_string_mapping_t;

/* For dynamic cases where the user creates a new atom or compound value. */
typedef struct {
  int32_t id;
  char* name;
} id_name_mapping_t;

typedef struct {
  id_name_mapping_t* mappings;
  size_t n_mappings;
} in_mappings_t;

typedef struct {
  char* name;
  int n_params;
  int32_t id;
} name_id_mapping_t;

typedef struct {
  name_id_mapping_t* mappings;
  size_t n_mappings;
} ni_mappings_t;

typedef struct {
  size_t length;
  memory_t* mems;
  int32_t* arg_addrs; /* Flat representation of two-dimensional array */
} runner_state_t;

typedef struct {
  void (*init)(runner_state_t *runner_state, int32_t* init_addrs, memory_t *init_mem);
  bool (*next)(runner_state_t *runner_state, memory_t* *results, size_t *n_results);
} runner_t;

static int64_t get_wall_time_ms() {
  struct timeval time;
  assert(gettimeofday(&time, NULL) == 0);
  return time.tv_sec * 1000000 + time.tv_usec;
}

DEBUG(int debug_print_nesting = 0;);
DEBUG(int debug_print_nesting_first = false;);
DEBUG(int debug_level = 2;);
DEBUG(int debug_active = true;);
DEBUG(size_t debug_n_nexts = 0;);
DEBUG(size_t debug_max_total_bytes = 0;);

DEBUG(void debug_print_nesting_inc() {
    debug_print_nesting += 1;
    debug_print_nesting_first = true;
  });

DEBUG(void debug_print_nesting_dec() {
    debug_print_nesting -= 1;
    debug_print_nesting_first = false;
  });

DEBUG(void debug_require_level(int level) {
    debug_active = (level <= debug_level);
  });

DEBUG(void debugb(char* fmt, ...) {
    if (debug_active) {
      va_list args;
      va_start(args, fmt);
      vfprintf(stderr, fmt, args);
      va_end(args);
    }
  });

DEBUG(void debug(char* fmt, ...) {
    if (debug_active) {
      for (int i = 0; i < debug_print_nesting; i++) {
        if (i == debug_print_nesting - 1 && debug_print_nesting_first) {
          debugb("'-");
          debug_print_nesting_first = false;
        } else {
          debugb("  ");
        }
      }
      va_list args;
      va_start(args, fmt);
      vfprintf(stderr, fmt, args);
      va_end(args);
    }
  });

DEBUG(void debug_print_human_size(size_t n_bytes) {
    if (n_bytes < 1024) {
      debugb("%lu B\n", n_bytes);
    } else if (n_bytes < 1024 * 1024) {
      debugb("%0.3lf KiB\n", (double) n_bytes / 1024);
    } else if (n_bytes < 1024 * 1024 * 1024) {
      debugb("%0.3lf MiB\n", (double) n_bytes / 1024 / 1024);
    } else {
      debugb("%0.3lf GiB\n", (double) n_bytes / 1024 / 1024 / 1024);
    }
  });

DEBUG(void debug_print_value(value_t v) {
    if (v.id == VALUE_NO_CONSTRAINTS) {
      debugb("_\n");
    } else if (v.id == VALUE_INDIRECT) {
      debugb("*%d\n", v.part1);
    } else if (v.part1 == VALUE_NO_PART) {
      debugb("%d\n", v.id);
    } else if (v.part2 == VALUE_NO_PART) {
      debugb("%d(%d)\n", v.id, v.part1);
    } else {
      debugb("%d(%d, %d)\n", v.id, v.part1, v.part2);
    }
  });

DEBUG(void debug_print_memory_usage_init(size_t cells_bytes, size_t args_bytes) {
    debug("cells: ");
    debug_print_human_size(cells_bytes);
    debug("args: ");
    debug_print_human_size(args_bytes);
    debug("total: ");
    size_t total_bytes = cells_bytes + args_bytes;
    debug_print_human_size(total_bytes);
    if (total_bytes > debug_max_total_bytes) {
      debug_max_total_bytes = total_bytes;
    }
  });

DEBUG(void debug_print_memory_usage_next(memory_t* mems, size_t mems_length, size_t args_bytes) {
    size_t cells_bytes = mems_length * sizeof(memory_t);
    for (size_t i = 0; i < mems_length; i++) {
      cells_bytes += mems[i].n_cells * sizeof(value_t);
    }
    debug("cells, average: ");
    debug_print_human_size(cells_bytes / mems_length);
    debug("cells: ");
    debug_print_human_size(cells_bytes);
    debug("args: ");
    debug_print_human_size(args_bytes);
    debug("total: ");
    size_t total_bytes = cells_bytes + args_bytes;
    debug_print_human_size(total_bytes);
    if (total_bytes > debug_max_total_bytes) {
      debug_max_total_bytes = total_bytes;
    }
  });

bool unify(value_t* cells, int32_t a_orig, int32_t b_orig) {
  int32_t a = a_orig;
  int32_t b = b_orig;

  value_t va = cells[a];
  value_t vb = cells[b];

  if (va.id == VALUE_INDIRECT) {
    a = va.part1;
    va = cells[a];
    if (va.id == VALUE_INDIRECT) {
      do {
        a = va.part1;
        va = cells[a];
      } while (va.id == VALUE_INDIRECT);
      cells[a_orig].part1 = a;
    }
  } else {
    a_orig = -1;
  }

  if (vb.id == VALUE_INDIRECT) {
    b = vb.part1;
    vb = cells[b];
    if (vb.id == VALUE_INDIRECT) {
      do {
        b = vb.part1;
        vb = cells[b];
      } while (vb.id == VALUE_INDIRECT);
      cells[b_orig].part1 = b;
    }
  }

  DEBUG
    ({
      debug("unify %d: ", a);
      debug_print_value(va);
      debug("      %d: ", b);
      debug_print_value(vb);
    })

  if (va.id >= 0 && vb.id == VALUE_NO_CONSTRAINTS) {
    cells[b] = va;
    DEBUG(debug("      success\n"));
    return true;
  }
  if (vb.id >= 0 && va.id == VALUE_NO_CONSTRAINTS) {
    cells[a] = vb;
    DEBUG(debug("      success\n"));
    return true;
  }
  if (va.id == VALUE_NO_CONSTRAINTS && vb.id == VALUE_NO_CONSTRAINTS) {
    cells[a].id = VALUE_INDIRECT;
    cells[a].part1 = b;
    if (a_orig >= 0) {
      cells[a_orig].part1 = b;
    }
    DEBUG(debug("      success\n"));
    return true;
  }

  if (va.id != vb.id) {
    DEBUG(debug("      failure\n"));
    return false;
  }
  if (va.part1 == VALUE_NO_PART && vb.part1 == VALUE_NO_PART) {
    DEBUG(debug("      success\n"));
    return true;
  }
  DEBUG(debug_print_nesting_inc());
  bool sub1_ok = unify(cells, va.part1, vb.part1);
  DEBUG(debug_print_nesting_dec());
  if (!sub1_ok) {
    DEBUG(debug("      failure\n"));
    return false;
  }
  if (va.part2 == VALUE_NO_PART && vb.part2 == VALUE_NO_PART) {
    DEBUG(debug("      success\n"));
    return true;
  }
  DEBUG(debug_print_nesting_inc());
  bool sub2_ok = unify(cells, va.part2, vb.part2);
  DEBUG(debug_print_nesting_dec());
  if (!sub2_ok) {
    DEBUG(debug("      failure\n"));
    return false;
  }
  DEBUG(debug("      success\n"));
  return true;
}

/* To be generated by the toppl compiler. */
char* id_to_name(int32_t id);
int32_t name_to_id(char* name, int n_parts);
int32_t static_ids_top;
char* find_predicate(const predicate_t *pred, runner_t* runner);
