#ifndef __BYTES_H__
#define __BYTES_H__

#include "vm.hpp"

#define MAGIC 0xDEADFADE

struct Header {
    u32 magic;
    u32 count_heap;
    u32 count_insts;
};

#endif
