#include "alloc.hpp"

#include <string.h>

void* alloc(usize size) {
    void* memory = sbrk(static_cast<isize>(size));
    EXIT_IF(memory == reinterpret_cast<void*>(-1));
    memset(memory, 0, size);
    return memory;
}
