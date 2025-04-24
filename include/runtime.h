#include <stdint.h>

// Read an integer from stdin.
int64_t read_int() __asm__("read_int");

// Print an integer to stdout.
void print_int(int64_t x) __asm__("print_int");
