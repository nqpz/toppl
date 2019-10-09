#define ARRAY_START_LENGTH 4

typedef struct {
  size_t elem_size; // User-specified

  uint8_t* bytes;
  size_t size_alloc;
  size_t size;
} array_t;

void array_init(array_t *arr) {
  arr->size_alloc = ARRAY_START_LENGTH * arr->elem_size;
  arr->bytes = (uint8_t*) malloc(arr->size_alloc);
  assert(arr->bytes != NULL);
  arr->size = 0;
}

void array_append(array_t *arr, const void *src) {
  if (arr->size == arr->size_alloc) {
    arr->size_alloc *= 2;
    arr->bytes = (uint8_t*) realloc(arr->bytes, arr->size_alloc);
    assert(arr->bytes != NULL);
  }
  memcpy(arr->bytes + arr->size, src, arr->elem_size);
  arr->size += arr->elem_size;
}

void array_compactify(array_t *arr) {
  if (arr->size != arr->size_alloc) {
    arr->size_alloc = arr->size;
    arr->bytes = (uint8_t*) realloc(arr->bytes, arr->size);
  }
}

void array_free(array_t *arr) {
  free(arr->bytes);
}

size_t array_length(array_t *arr) {
  return (arr->size / arr->elem_size);
}

void array_get(array_t *arr, size_t idx, void *dest) {
  memcpy(dest, arr->bytes + idx * arr->elem_size, arr->elem_size);
}

void array_set(array_t *arr, size_t idx, void *src) {
  memcpy(arr->bytes + idx * arr->elem_size, src, arr->elem_size);
}
