#ifndef __VM_BYTES_H__
#define __VM_BYTES_H__

#include "vm.h"

#define MAGIC 0xDEADFADE

typedef struct {
    u32 magic;
    u32 count_inst;
} Header;

#endif
