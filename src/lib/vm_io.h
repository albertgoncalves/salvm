#ifndef __VM_IO_H__
#define __VM_IO_H__

#include "vm.h"

#define MAGIC 0xDEADFADE

static void run(Vm* vm) {
    while (vm->alive) {
#ifdef DEBUG_PRINT_VM
        {
            printf("\n"
                   "    | .index.inst       : %d\n"
                   "    |       .stack_top  : %d\n"
                   "    |       .stack_base : %d\n"
                   "    | .stack            : [ ",
                   vm->index.inst,
                   vm->index.stack_top,
                   vm->index.stack_base);
            for (i32 i = vm->index.stack_base; i < vm->index.stack_top; ++i) {
                printf("%d ", vm->stack[i].as_i32);
            }
            printf("]\n\n");
        }
#endif
        do_inst(vm);
    }
}

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
