#include "vm_bytes.hpp"
#include "vm_io.hpp"

static void insts_from_bytes(Vm* vm, const char* path) {
    File* file = fopen(path, "rb");
    EXIT_IF(!file);
    Header header;
    EXIT_IF(fread(&header, sizeof(Header), 1, file) != 1);
    EXIT_IF(header.magic != MAGIC);
    EXIT_IF(CAP_HEAP8 <= header.count_bytes);
    EXIT_IF(CAP_INSTS <= header.count_inst);
    EXIT_IF(fread(&vm->heap, sizeof(i8), header.count_bytes, file) !=
            header.count_bytes);
    EXIT_IF(fread(&vm->insts, sizeof(Inst), header.count_inst, file) !=
            header.count_inst);
    fclose(file);
}

i32 main(i32 n, const char** args) {
    EXIT_IF(n < 2);
    {
        Vm* vm = reinterpret_cast<Vm*>(calloc(1, sizeof(Vm)));
        EXIT_IF(!vm);
        insts_from_bytes(vm, args[1]);
        run(vm);
        {
            const i32 len = vm->index.stack_top + vm->index.stack_base;
            printf("%d\n", len == 0 ? 0 : vm->stack[len - 1].as_i32);
        }
        free(vm);
    }
    return EXIT_SUCCESS;
}
