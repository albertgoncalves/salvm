#include "vm.h"

i32 main(void) {
    printf("sizeof(Inst)   : %zu\n"
           "sizeof(Header) : %zu\n"
           "sizeof(Vm)     : %zu\n",
           sizeof(Inst),
           sizeof(Header),
           sizeof(Vm));
    Vm vm = {0};
    {
        vm.alive = TRUE;
        i32 i = 0;

        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = 12345;

        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = 543210;

        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = -2;

        vm.insts[i++].tag = INST_ADDI;

        vm.insts[i].tag = INST_DROP;
        vm.insts[i++].op = 2;

        vm.insts[i].tag = INST_RSRV;
        vm.insts[i++].op = 1;

        vm.insts[i].tag = INST_DROP;
        vm.insts[i++].op = 2;

        vm.insts[i].tag = INST_COPY;
        vm.insts[i++].op = 1;

        vm.insts[i].tag = INST_STORE;
        vm.insts[i++].op = 0;

        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = 1;

        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = 2;

        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = 3;

        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = 4;

        vm.insts[i].tag = INST_BURY;
        vm.insts[i++].op = 3;

        vm.insts[i++].tag = INST_EQ;

        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = 5;

        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = 5;

        vm.insts[i].tag = INST_JUMP;
        vm.insts[i++].op = 4;

        vm.insts[i++].tag = INST_SAVE;

        vm.insts[i].tag = INST_FRAME;
        vm.insts[i++].op = 5;

        vm.insts[i++].tag = INST_HALT;

        vm.insts[i++].tag = INST_EQ;

        vm.insts[i++].tag = INST_NOT;

        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = 0x7FFFFFFF;

        vm.insts[i++].tag = INST_NOT;

        vm.insts[i].tag = INST_JPZ;
        vm.insts[i++].op = -7;

        vm.insts[i++].tag = INST_HALT;
    }
    run(&vm);
    return EXIT_SUCCESS;
}
