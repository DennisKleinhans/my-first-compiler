#define _CRT_SECURE_NO_WARNINGS
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include "runtime.h"

// Read an integer from stdin
int64_t read_int() {
    int64_t i;
    scanf("%" SCNd64, &i);
    return i;
}

// print an integer to stdout
void print_int(int64_t x) {
    printf("%" PRId64, x);
}

