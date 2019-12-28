#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct rgb {
  int r;
  int g;
  int b;
};

struct run_len {
  int len;
  struct rgb *color;
};


