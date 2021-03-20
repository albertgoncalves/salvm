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
        Vm  vm = {0};
        u32 i = 0;

        vm.insts[i].tag = INST_TOP;
        vm.insts[i++].op = 1;

        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = 0;

        vm.insts[i].tag = INST_STORE;
        vm.insts[i++].op = 0;

        vm.insts[i].tag = INST_COPY;
        vm.insts[i++].op = 0;

        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = 5;

        vm.insts[i++].tag = INST_EQ;

        vm.insts[i].tag = INST_JPZ;
        vm.insts[i++].op = 10;

        vm.insts[i].tag = INST_COPY;
        vm.insts[i++].op = 0;

        vm.insts[i++].tag = INST_PRINTI;

        vm.insts[i].tag = INST_JUMP;
        vm.insts[i++].op = 24;

        vm.insts[i].tag = INST_COPY;
        vm.insts[i++].op = 0;

        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = 1;

        vm.insts[i++].tag = INST_ADDI;

        vm.insts[i].tag = INST_STORE;
        vm.insts[i++].op = 0;

        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = 3;

        vm.insts[i].tag = INST_COPY;
        vm.insts[i++].op = 0;

        vm.insts[i++].tag = INST_EQ;

        vm.insts[i].tag = INST_JPZ;
        vm.insts[i++].op = 21;

        vm.insts[i].tag = INST_JUMP;
        vm.insts[i++].op = 20;

        vm.insts[i].tag = INST_JUMP;
        vm.insts[i++].op = 18;

        vm.insts[i].tag = INST_JUMP;
        vm.insts[i++].op = 3;

        vm.insts[i].tag = INST_COPY;
        vm.insts[i++].op = 0;

        vm.insts[i++].tag = INST_PRINTI;

        vm.insts[i].tag = INST_JUMP;
        vm.insts[i++].op = 3;

        vm.insts[i++].tag = INST_HALT;

        set_insts_to_file(&vm, i, args[1]);
    }
    {
        Vm vm = {0};
        vm.alive = TRUE;
        set_insts_from_file(&vm, args[1]);
        run(&vm);
    }
    return EXIT_SUCCESS;
}
