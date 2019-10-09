int32_t name_to_id_dyn(array_t *ni_mappings_arr, array_t *in_mappings_arr,
                       char* name, int n_params) {
  int32_t id;

  id = name_to_id(name, n_params);
  if (id > 0) {
    return id;
  }

  for (size_t i = 0; i < array_length(ni_mappings_arr); i++) {
    name_id_mapping_t ni;
    array_get(ni_mappings_arr, i, &ni);
    if (strcmp(ni.name, name) == 0 && ni.n_params == n_params) {
      return ni.id;
    }
  }

  id = static_ids_top + array_length(in_mappings_arr);
  name = strdup(name);
  assert(name != NULL);
  name_id_mapping_t ni = { .name = name, .n_params = n_params, .id = id };
  array_append(ni_mappings_arr, &ni);
  id_name_mapping_t in = { .id = id, name = name };
  array_append(in_mappings_arr, &in);
  return id;
}

int getchar_skipspace() {
  while (true) {
    int c = getchar();
    if (c == EOF || !isspace(c)) {
      return c;
    }
  }
}

char* parse_value(array_t *values_arr, array_t *va_mappings_arr,
                  array_t *ni_mappings_arr, array_t *in_mappings_arr,
                  int32_t *addr, bool *no_more_values) {
  int c;
  char cc;
  char* err;

  array_t name = { .elem_size = 1 };
  array_init(&name);

  // Used for compound values.
  array_t addrs_arr = { .elem_size = sizeof(int32_t) };

  c = getchar_skipspace();
  if (isupper(c) || (c != EOF && (char) c == '_')) {
    /* Variable */
    cc = (char) c;
    bool is_wildcard = (cc == '_');
    while (true) {
      if (isalnum(c) || (c != EOF && (char) c == '_')) {
        if (!is_wildcard) {
          array_append(&name, &cc);
        }
        c = getchar();
        cc = (char) c;
      } else {
        bool done = (c != EOF && ((char) c == ')' || (char) c == ','));
        if (!done && isspace(c)) {
          c = getchar_skipspace();
          done = (c != EOF && ((char) c == ')' || (char) c == ','));
        }
        if (done) {
          *no_more_values = (((char) c) == ')');
          var_addr_mapping_t va_mapping;
          bool found = false;
          for (size_t i = 0; i < array_length(va_mappings_arr); i++) {
            array_get(va_mappings_arr, i, &va_mapping);
            if (strcmp(va_mapping.variable, (char*) name.bytes) == 0) {
              *addr = va_mapping.address;
              found = true;
              break;
            }
          }
          if (!found) {
            *addr = array_length(values_arr);
            value_t no_constraints = { .id = VALUE_NO_CONSTRAINTS };
            array_append(values_arr, &no_constraints);
            if (!is_wildcard) {
              cc = '\0';
              array_append(&name, &cc);
              char* var = strdup((char*) name.bytes);
              assert(var != NULL);
              va_mapping.variable = var;
              va_mapping.address = *addr;
              array_append(va_mappings_arr, &va_mapping);
            }
          }
          break;
        } else {
          err = "expected letter, ')', or ','";
          goto err0;
        }
      }
    }
  } else if (islower(c)) {
    /* Atom or compound value */
    value_t val;

    cc = (char) c;
    while (true) {
      if (isalnum(c) || (c != EOF && (char) c == '_')) {
        array_append(&name, &cc);
        c = getchar();
        cc = (char) c;
      } else {
        bool done = (c != EOF && ((char) c == '(' || (char) c == ')' || (char) c == ','));
        if (!done && isspace(c)) {
          c = getchar_skipspace();
          done = (c != EOF && ((char) c == '(' || (char) c == ')' || (char) c == ','));
        }
        if (done) {
          cc = '\0';
          array_append(&name, &cc);
          if ((char) c == ')' || (char) c == ',') {
            /* Atom */
            *addr = array_length(values_arr);
            *no_more_values = ((char) c == ')');
            val.id = name_to_id_dyn(ni_mappings_arr, in_mappings_arr,
                                    (char*) name.bytes, 0);
            val.part1 = VALUE_NO_PART;
            array_append(values_arr, &val);
            break;
          } else if ((char) c == '(') {
            /* Compound value */
            array_init(&addrs_arr);
            bool no_more_values_sub = false;
            while (!no_more_values_sub) {
              int32_t addr;
              err = parse_value(values_arr, va_mappings_arr,
                                ni_mappings_arr, in_mappings_arr, &addr, &no_more_values_sub);
              if (err != NULL) {
                goto err1;
              }
              array_append(&addrs_arr, &addr);
            }
            size_t n_addrs = array_length(&addrs_arr);
            val.id = name_to_id_dyn(ni_mappings_arr, in_mappings_arr,
                                    (char*) name.bytes, n_addrs);

            if (n_addrs == 0) {
              val.part1 = VALUE_NO_PART;
              *addr = array_length(values_arr);
              array_append(values_arr, &val);
            } else if (n_addrs == 1) {
              array_get(&addrs_arr, 0, &val.part1);
              val.part2 = VALUE_NO_PART;
              *addr = array_length(values_arr);
              array_append(values_arr, &val);
            } else if (n_addrs == 2) {
              array_get(&addrs_arr, 0, &val.part1);
              array_get(&addrs_arr, 1, &val.part2);
              *addr = array_length(values_arr);
              array_append(values_arr, &val);
            } else {
              array_get(&addrs_arr, 0, &val.part1);
              val.part2 = (int32_t) array_length(values_arr);
              value_t val_temp = { .id = 0 };
              for (size_t i = 1; i < n_addrs - 2; i++) {
                array_get(&addrs_arr, i, &val_temp.part1);
                val_temp.part2 = (int32_t) array_length(values_arr) + 1;
                array_append(values_arr, &val_temp);
              }
              array_get(&addrs_arr, n_addrs - 2, &val_temp.part1);
              array_get(&addrs_arr, n_addrs - 1, &val_temp.part2);
              array_append(values_arr, &val_temp);
              *addr = array_length(values_arr);
              array_append(values_arr, &val);
            }
            array_free(&addrs_arr);

            c = getchar();
            bool done = (c != EOF && ((char) c == ')' || (char) c == ','));
            if (!done && isspace(c)) {
              c = getchar_skipspace();
              done = (c != EOF && ((char) c == ')' || (char) c == ','));
            }
            if (done) {
              *no_more_values = ((char) c == ')');
            } else {
              err = "expected ')' or ','";
              goto err0;
            }
            break;
          } else {
            err = "expected ')', ',', or '('";
            goto err0;
          }
        }
      }
    }
  } else {
    err = "expected letter or '_'";
    goto err0;
  }

  array_free(&name);
  return NULL;
 err0:
  array_free(&name);
  return err;
 err1:
  array_free(&name);
  array_free(&addrs_arr);
  return err;
}

char* parse_query(predicate_t *pred, memory_t *mem, int32_t* *addrs,
                  va_mappings_t *va_mappings, in_mappings_t* in_mappings,
                  bool *more_queries) {
  int c;
  char cc;
  char* err;

  c = getchar_skipspace();
  if (c == EOF) {
    *more_queries = false;
    return NULL;
  }

  array_t pred_name = { .elem_size = 1 };
  array_init(&pred_name);

  if (!islower(c)) {
    err = "expected lowercase letter";
    goto err0;
  }
  cc = (char) c;
  array_append(&pred_name, &cc);
  while (true) {
    c = getchar();
    if (isalnum(c) || (c != EOF && (char) c == '_')) {
      cc = (char) c;
      array_append(&pred_name, &cc);
    } else if ((c != EOF && ((char) c == '(' || (char) c == '.'))
               || (isspace(c) && ((char) (c = getchar_skipspace()) == '(' || (char) c == '.'))) {
      cc = '\0';
      array_append(&pred_name, &cc);
      array_compactify(&pred_name);
      pred->name = (char*) pred_name.bytes;
      pred->n_params = 0; // Update as we go.
      if ((char) c == '(') {
        break;
      } else {
        mem->n_cells = 0;
        mem->cells = malloc(0);
        *addrs = malloc(0);
        va_mappings->n_mappings = 0;
        va_mappings->mappings = malloc(0);
        in_mappings->n_mappings = 0;
        in_mappings->mappings = malloc(0);
        return NULL;
      }
    } else {
      err = "expected alphanumeric character, '_', '(', or '.'";
      goto err0;
    }
  }

  array_t values_arr = { .elem_size = sizeof(value_t) };
  array_init(&values_arr);
  array_t addrs_arr = { .elem_size = sizeof(int32_t) };
  array_init(&addrs_arr);
  array_t va_mappings_arr = { .elem_size = sizeof(var_addr_mapping_t) };
  array_init(&va_mappings_arr);
  array_t ni_mappings_arr = { .elem_size = sizeof(name_id_mapping_t) };
  array_init(&ni_mappings_arr);
  array_t in_mappings_arr = { .elem_size = sizeof(id_name_mapping_t) };
  array_init(&in_mappings_arr);

  bool no_more_values = false;
  int32_t addr;
  while (true) {
    err = parse_value(&values_arr, &va_mappings_arr,
                      &ni_mappings_arr, &in_mappings_arr,
                      &addr, &no_more_values);
    if (err != NULL) {
      goto err1;
    } else {
      pred->n_params++;
      array_append(&addrs_arr, &addr);
      if (no_more_values) {
        c = getchar_skipspace();
        if ((char) c != '.') {
          err = "expected '.'";
          goto err1;
        }
        break;
      }
    }
  }

  array_compactify(&values_arr);
  mem->n_cells = array_length(&values_arr);
  mem->cells = (value_t*) values_arr.bytes;

  array_compactify(&addrs_arr);
  *addrs = (int32_t*) addrs_arr.bytes;

  array_compactify(&va_mappings_arr);
  va_mappings->n_mappings = array_length(&va_mappings_arr);
  va_mappings->mappings = (var_addr_mapping_t*) va_mappings_arr.bytes;

  array_free(&ni_mappings_arr);

  array_compactify(&in_mappings_arr);
  in_mappings->n_mappings = array_length(&in_mappings_arr);
  in_mappings->mappings = (id_name_mapping_t*) in_mappings_arr.bytes;

  return NULL;

 err0:
  array_free(&pred_name);
  return err;

 err1:
  free(pred->name);
  array_free(&values_arr);
  array_free(&addrs_arr);
  array_free(&va_mappings_arr);
  array_free(&ni_mappings_arr);
  return err;
}
