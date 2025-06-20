#define _CRT_SECURE_NO_WARNINGS
#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include "runtime.h"

int64_t* heap_end;
int64_t* free_ptr;

// Initialize the memory of the runtime with the initial heap size.
void initialize() {
    int64_t* heap_begin;
    if (!(heap_begin = malloc(HEAP_SIZE))) {
        printf("Failed to malloc %d byte heap\n", HEAP_SIZE);
        exit(EXIT_FAILURE);
    }
    heap_end = heap_begin + (HEAP_SIZE / sizeof(int64_t));
    free_ptr = heap_begin;
}

// Obtain a piece of memory of the given size.
int64_t* allocate(uint64_t size_in_bytes) {
    uint64_t words_needed = (size_in_bytes + sizeof(int64_t) - 1) / sizeof(int64_t);
    if (free_ptr + words_needed > heap_end) {
        fprintf(stderr, "Error: Heap exhausted! Requested %lu bytes\n", size_in_bytes);
        fprintf(stderr, "Current free_ptr: %p, heap_end: %p\n", free_ptr, heap_end);
        exit(1);
    }
    int64_t* allocated_ptr = free_ptr;
    free_ptr += words_needed;
    return allocated_ptr;
}

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

