#include <stdint.h>
#include <stdio.h>

int64_t cmax(int64_t a, int64_t b) {
  int64_t cmax = (a > b) ? a : b;
  fprintf(stderr, "~ cmax(%ld, %ld) -> %ld\n", a, b, cmax);

  return cmax;
}
