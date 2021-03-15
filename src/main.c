#include "vm_io.h"

i32 main(i32 n, const char** args) {
    EXIT_IF(n < 2);
    printf("sizeof(Inst)   : %zu\n"
           "sizeof(Header) : %zu\n"
           "sizeof(Vm)     : %zu\n",
           sizeof(Inst),
           sizeof(Header),
           sizeof(Vm));
    {
        Vm vm = {0};
        vm.insts[0].tag = INST_PUSH;
        vm.insts[0].op = 123456;
        vm.insts[1].tag = INST_HALT;
        set_insts_to_file(&vm, 2, args[1]);
    }
    {
        Vm vm = {0};
        vm.alive = TRUE;
        set_insts_from_file(&vm, args[1]);
        run(&vm);
    }
    return EXIT_SUCCESS;
}
