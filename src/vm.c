#include "vm_bytes.h"
#include "vm_io.h"

static void insts_from_bytes(Vm* vm, const char* path) {
    File* file = fopen(path, "rb");
    EXIT_IF(!file);
    Header header;
    EXIT_IF(fread(&header, sizeof(Header), 1, file) != 1);
    EXIT_IF(header.magic != MAGIC);
    EXIT_IF(CAP_INSTS <= header.count_inst);
    EXIT_IF(fread(&vm->insts, sizeof(Inst), header.count_inst, file) !=
            header.count_inst);
    fclose(file);
}

i32 main(i32 n, const char** args) {
    EXIT_IF(n < 2);
    {
        Vm* vm = calloc(1, sizeof(Vm));
        EXIT_IF(!vm);
        insts_from_bytes(vm, args[1]);
        run(vm);
        free(vm);
    }
    return EXIT_SUCCESS;
}
