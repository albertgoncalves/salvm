#include "vm_asm.h"
#include "vm_bytes.h"

static void insts_to_bytes(Vm* vm, u32 count_inst, const char* path) {
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

i32 main(i32 n, const char** args) {
    EXIT_IF(n < 3);
    {
        Memory* memory = calloc(1, sizeof(Memory));
        EXIT_IF(!memory);
        set_chars_from_file(memory, args[1]);
        set_tokens(memory);
        set_insts(memory);
        insts_to_bytes(&memory->vm, memory->len_pre_insts, args[2]);
        free(memory);
    }
    return EXIT_SUCCESS;
}
