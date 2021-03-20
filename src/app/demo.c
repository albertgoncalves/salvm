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

        EXIT_IF(i != 1);
        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = 0;

        EXIT_IF(i != 2);
        vm.insts[i].tag = INST_STORE;
        vm.insts[i++].op = 0;

        EXIT_IF(i != 3);
        vm.insts[i].tag = INST_COPY;
        vm.insts[i++].op = 0;

        EXIT_IF(i != 4);
        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = 5;

        EXIT_IF(i != 5);
        vm.insts[i++].tag = INST_EQ;

        EXIT_IF(i != 6);
        vm.insts[i].tag = INST_JPZ;
        vm.insts[i++].op = 10;

        EXIT_IF(i != 7);
        vm.insts[i].tag = INST_COPY;
        vm.insts[i++].op = 0;

        EXIT_IF(i != 8);
        vm.insts[i].tag = INST_NATIVE;
        vm.insts[i++].op = NATIVE_PRINTI;

        EXIT_IF(i != 9);
        vm.insts[i].tag = INST_JUMP;
        vm.insts[i++].op = 24;

        EXIT_IF(i != 10);
        vm.insts[i].tag = INST_COPY;
        vm.insts[i++].op = 0;

        EXIT_IF(i != 11);
        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = 1;

        EXIT_IF(i != 12);
        vm.insts[i++].tag = INST_ADDI;

        EXIT_IF(i != 13);
        vm.insts[i].tag = INST_STORE;
        vm.insts[i++].op = 0;

        EXIT_IF(i != 14);
        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = 3;

        EXIT_IF(i != 15);
        vm.insts[i].tag = INST_COPY;
        vm.insts[i++].op = 0;

        EXIT_IF(i != 16);
        vm.insts[i++].tag = INST_EQ;

        EXIT_IF(i != 17);
        vm.insts[i].tag = INST_JPZ;
        vm.insts[i++].op = 21;

        EXIT_IF(i != 18);
        vm.insts[i].tag = INST_JUMP;
        vm.insts[i++].op = 20;

        EXIT_IF(i != 19);
        vm.insts[i].tag = INST_JUMP;
        vm.insts[i++].op = 18;

        EXIT_IF(i != 20);
        vm.insts[i].tag = INST_JUMP;
        vm.insts[i++].op = 3;

        EXIT_IF(i != 21);
        vm.insts[i].tag = INST_COPY;
        vm.insts[i++].op = 0;

        EXIT_IF(i != 22);
        vm.insts[i].tag = INST_NATIVE;
        vm.insts[i++].op = NATIVE_PRINTI;

        EXIT_IF(i != 23);
        vm.insts[i].tag = INST_JUMP;
        vm.insts[i++].op = 3;

        EXIT_IF(i != 24);
        vm.insts[i].tag = INST_PUSH;
        vm.insts[i++].op = '\n';

        EXIT_IF(i != 25);
        vm.insts[i].tag = INST_NATIVE;
        vm.insts[i++].op = NATIVE_PRINTC;

        EXIT_IF(i != 26);
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
