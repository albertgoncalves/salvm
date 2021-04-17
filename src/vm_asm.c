#include "vm_asm.h"
#include "vm_bytes.h"

static void set_chars_from_file(Memory* memory, const char* path) {
    File* file = fopen(path, "r");
    EXIT_IF(!file);
    fseek(file, 0, SEEK_END);
    memory->len_chars = (u32)ftell(file);
    EXIT_IF(CAP_CHAR <= memory->len_chars);
    rewind(file);
    EXIT_IF(fread(memory->chars, sizeof(char), memory->len_chars, file) !=
            memory->len_chars);
    fclose(file);
}

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