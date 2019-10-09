char* id_to_name_dyn(const in_mappings_t* in_mappings, int32_t id) {
  if (id < static_ids_top) {
    return id_to_name(id);
  } else {
    for (size_t i = 0; i < in_mappings->n_mappings; i++) {
      if (in_mappings->mappings[i].id == id) {
        return in_mappings->mappings[i].name;
      }
    }
  }
  assert(0);
}

char* address_string(int32_t addr, const memory_t *mem,
                     const in_mappings_t* in_mappings, array_t* as_arr,
                     int *wildcard_counter, array_t* to_be_freed);

char* value_string(value_t val, const memory_t *mem,
                   const in_mappings_t* in_mappings, array_t* as_arr,
                   int *wildcard_counter, array_t* to_be_freed) {
  char* s;
  if (val.id == VALUE_NO_CONSTRAINTS) {
    s = (char*) malloc(sizeof(char) * (1 + 10 + 1));
    assert(s != NULL);
    sprintf(s, "_%d", *wildcard_counter);
    array_append(to_be_freed, &s);
    (*wildcard_counter)++;
    return s;
  }

  char* base = id_to_name_dyn(in_mappings, val.id);
  if (val.part1 == VALUE_NO_PART) {
    return base;
  } else if (val.part2 == VALUE_NO_PART) {
    char* part1 = address_string(val.part1, mem, in_mappings, as_arr, wildcard_counter, to_be_freed);
    s = (char*) malloc(sizeof(char) * (strlen(base) + 1 + strlen(part1) + 1 + 1));
    strcpy(s, base);
    strcpy(s + strlen(base), "(");
    strcpy(s + strlen(base) + 1, part1);
    strcpy(s + strlen(base) + 1 + strlen(part1), ")");
    array_append(to_be_freed, &s);
    return s;
  } else {
    array_t strings = { .elem_size = sizeof(char*) };
    array_init(&strings);
    array_append(&strings, &base);
    char* par_start = "(";
    array_append(&strings, &par_start);
    do {
      char* temp = address_string(val.part1, mem, in_mappings, as_arr, wildcard_counter, to_be_freed);
      array_append(&strings, &temp);
      char* comma = ", ";
      array_append(&strings, &comma);
      value_t val_new = mem->cells[val.part2];
      if (val_new.id == 0) {
        val = val_new;
      }
    } while (val.id == 0);
    char* temp = address_string(val.part2, mem, in_mappings, as_arr, wildcard_counter, to_be_freed);
    array_append(&strings, &temp);
    char* par_end = ")";
    array_append(&strings, &par_end);
    size_t s_len = 1;
    for (size_t i = 0; i < array_length(&strings); i++) {
      char* string;
      array_get(&strings, i, &string);
      s_len += strlen(string);
    }
    s = (char*) malloc(sizeof(char) * s_len);
    size_t offset = 0;
    for (size_t i = 0; i < array_length(&strings); i++) {
      char* string;
      array_get(&strings, i, &string);
      strcpy(s + offset, string);
      offset += strlen(string);
    }
    array_free(&strings);
    array_append(to_be_freed, &s);
    return s;
  }
}

char* address_string(int32_t addr, const memory_t *mem,
                     const in_mappings_t* in_mappings, array_t* as_arr,
                     int *wildcard_counter, array_t* to_be_freed) {
  addr_string_mapping_t as;
  for (size_t i = 0; i < array_length(as_arr); i++) {
    array_get(as_arr, i, &as);
    if (as.address == addr) {
      return as.string;
    }
  }

  size_t as_i = array_length(as_arr);
  as.address = addr;

  char* s = (char*) malloc(sizeof(char) * (1 + 10 + 6 + 1));
  assert(s != NULL);
  sprintf(s, "_%d_cycle", addr);
  array_append(to_be_freed, &s);

  as.string = s;
  array_append(as_arr, &as);

  value_t val = mem->cells[addr];
  char* representation;
  if (val.id == VALUE_INDIRECT) {
    addr = val.part1;
    representation = address_string(addr, mem, in_mappings, as_arr, wildcard_counter, to_be_freed);
  } else {
    representation = value_string(val, mem, in_mappings, as_arr, wildcard_counter, to_be_freed);
  }
  as.string = representation;
  array_set(as_arr, as_i, &as);
  return representation;
}

void print_values(const memory_t *mem, const va_mappings_t *va_mappings,
                  const in_mappings_t* in_mappings) {
  fputs("yes", stdout);
  if (va_mappings->n_mappings > 0) {
    putchar('{');
  }

  array_t as_arr = { .elem_size = sizeof(addr_string_mapping_t) };
  array_init(&as_arr);

  array_t to_be_freed = { .elem_size = sizeof(void*) };
  array_init(&to_be_freed);

  int wildcard_counter = 0;
  for (size_t i = 0; i < va_mappings->n_mappings; i++) {
    printf("%s=", va_mappings->mappings[i].variable);
    char* s = address_string(va_mappings->mappings[i].address, mem, in_mappings, &as_arr, &wildcard_counter, &to_be_freed);
    fputs(s, stdout);
    if (i < va_mappings->n_mappings - 1) {
      fputs(", ", stdout);
    }
  }
  for (size_t i = 0; i < array_length(&to_be_freed); i++) {
    void* p;
    array_get(&to_be_freed, i, &p);
    free(p);
  }
  array_free(&as_arr);
  array_free(&to_be_freed);
  if (va_mappings->n_mappings > 0) {
    putchar('}');
  }
  fputs("\n", stdout);
}

void print_results(memory_t *results, size_t n_results,
                   const va_mappings_t *va_mappings,
                   const in_mappings_t* in_mappings) {
  for (size_t i = 0; i < n_results; i++) {
    print_values(&results[i], va_mappings, in_mappings);
  }
}
