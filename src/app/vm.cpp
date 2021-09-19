#include "alloc.hpp"
#include "bytes.hpp"
#include "io.hpp"

#ifdef RELEASE
    #include "alloc.cpp"
    #include "inst.cpp"
    #include "io.cpp"
#endif

static void insts_from_bytes(Vm* vm, const char* path) {
    File* file = fopen(path, "rb");
    EXIT_IF(!file);
    Header header;
    EXIT_IF(fread(&header, sizeof(Header), 1, file) != 1);
    EXIT_IF(header.magic != MAGIC);
    EXIT_IF(CAP_HEAP8 <= header.count_heap);
    EXIT_IF(CAP_INSTS <= header.count_insts);
    EXIT_IF(fread(&vm->heap, sizeof(i8), header.count_heap, file) !=
            header.count_heap);
    EXIT_IF(fread(&vm->insts, sizeof(Inst), header.count_insts, file) !=
            header.count_insts);
    fclose(file);
}

i32 main(i32 n, const char** args) {
    EXIT_IF(n < 2);
    {
        Vm* vm = reinterpret_cast<Vm*>(alloc(sizeof(Vm)));
        insts_from_bytes(vm, args[1]);
        run(vm);
        {
            const i32 len = vm->index.stack_top + vm->index.stack_base;
            printf("%d\n", len == 0 ? 0 : vm->stack[len - 1].as_i32);
        }
    }
    return EXIT_SUCCESS;
}
