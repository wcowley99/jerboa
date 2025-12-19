#include <stdint.h>
#include <stdio.h>

int64_t max(int64_t a, int64_t b) {
  int64_t max = (a > b) ? a : b;
  fprintf(stderr, "~ max(%ld, %ld) -> %ld\n", a, b, max);

  return max;
}
