#ifndef __VM_BYTES_H__
#define __VM_BYTES_H__

#include "vm.hpp"

#define MAGIC 0xDEADFADE

struct Header {
    u32 magic;
    u32 count_bytes;
    u32 count_inst;
};

#endif
