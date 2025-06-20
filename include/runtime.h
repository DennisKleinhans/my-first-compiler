#include <stdint.h>

#define HEAP_SIZE (64 * 1024 * 1024)

extern int64_t* heap_end asm("heap_end");
extern int64_t* free_ptr asm("free_ptr");

// Initialize the memory of the runtime with the initial heap size.
void initialize() asm("initialize");

// Obtain a piece of memory of the given size.
int64_t* allocate(uint64_t size_in_bytes) asm("allocate");

// Read an integer from stdin.
int64_t read_int() __asm__("read_int");

// Print an integer to stdout.
void print_int(int64_t x) __asm__("print_int");
