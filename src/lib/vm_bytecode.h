#ifndef __VM_BYTECODE_H__
#define __VM_BYTECODE_H__

#include "vm.h"

#define MAGIC 0xDEADFADE

typedef struct {
    u32 magic;
    u32 count_inst;
} Header;

static void set_insts_from_file(Vm* vm, const char* path) {
    File* file = fopen(path, "rb");
    EXIT_IF(!file);
    Header header;
    EXIT_IF(fread(&header, sizeof(Header), 1, file) != 1);
    EXIT_IF(header.magic != MAGIC);
    EXIT_IF(CAP_INST <= header.count_inst);
    EXIT_IF(fread(&vm->insts, sizeof(Inst), header.count_inst, file) !=
            header.count_inst);
    fclose(file);
}

static void set_insts_to_file(Vm* vm, u32 count_inst, const char* path) {
    EXIT_IF(CAP_INST <= count_inst);
    File* file = fopen(path, "wb");
    EXIT_IF(!file);
    Header header = {
        .magic = MAGIC,
        .count_inst = count_inst,
    };
    EXIT_IF(fwrite(&header, sizeof(Header), 1, file) != 1);
    EXIT_IF(fwrite(&vm->insts, sizeof(Inst), count_inst, file) != count_inst);
    fclose(file);
}

#endif
