#include "vm_inst.h"

#define TEST(fn, block)    \
    static void fn(void) { \
        block;             \
        printf(".");       \
    }

TEST(test_push, {
    Vm vm = {0};
    vm.insts[0].tag = INST_PUSH;
    vm.insts[0].op = 12345;
    do_inst(&vm);
    EXIT_IF(vm.index.inst != 1);
    EXIT_IF(vm.index.stack_top != 1);
    EXIT_IF(vm.stack[0].as_i32 != 12345);
})

TEST(test_top, {
    Vm vm = {0};
    vm.insts[0].tag = INST_TOP;
    vm.insts[0].op = 5;
    do_inst(&vm);
    EXIT_IF(vm.index.inst != 1);
    EXIT_IF(vm.index.stack_top != 5);
})

TEST(test_copy, {
    Vm vm = {0};
    vm.insts[0].tag = INST_COPY;
    vm.insts[0].op = 1;
    vm.stack[2].as_i32 = 15;
    vm.index.stack_top = 3;
    vm.index.stack_base = 1;
    do_inst(&vm);
    EXIT_IF(vm.index.inst != 1);
    EXIT_IF(vm.index.stack_top != 4);
    EXIT_IF(vm.stack[3].as_i32 != 15);
})

TEST(test_store, {
    Vm vm = {0};
    vm.insts[0].tag = INST_STORE;
    vm.insts[0].op = 1;
    vm.stack[3].as_i32 = 15;
    vm.index.stack_top = 4;
    vm.index.stack_base = 1;
    do_inst(&vm);
    EXIT_IF(vm.index.inst != 1);
    EXIT_IF(vm.index.stack_top != 3);
    EXIT_IF(vm.stack[2].as_i32 != 15);
})

TEST(test_call, {
    Vm vm = {0};
    vm.insts[0].tag = INST_CALL;
    vm.insts[0].op = 99;
    do_inst(&vm);
    EXIT_IF(vm.index.inst != 99);
    EXIT_IF(vm.index.stack_top != 1);
    EXIT_IF(vm.stack[0].as_i32 != 1);
})

TEST(test_ret, {
    Vm vm = {0};
    vm.insts[0].tag = INST_RET;
    vm.stack[0].as_i32 = 68;
    vm.index.stack_top = 1;
    do_inst(&vm);
    EXIT_IF(vm.index.inst != 68);
    EXIT_IF(vm.index.stack_top != 0);
})

TEST(test_save, {
    Vm vm = {0};
    vm.insts[0].tag = INST_SAVE;
    vm.index.stack_base = 76;
    do_inst(&vm);
    EXIT_IF(vm.index.inst != 1);
    EXIT_IF(vm.index.stack_top != 1);
    EXIT_IF(vm.stack[0].as_i32 != 76);
})

TEST(test_frame, {
    Vm vm = {0};
    vm.insts[0].tag = INST_FRAME;
    vm.insts[0].op = 5;
    vm.index.stack_top = 11;
    do_inst(&vm);
    EXIT_IF(vm.index.inst != 1);
    EXIT_IF(vm.index.stack_top != 11);
    EXIT_IF(vm.index.stack_base != 6);
})

TEST(test_reset, {
    Vm vm = {0};
    vm.insts[0].tag = INST_RESET;
    vm.index.stack_top = 29;
    vm.index.stack_base = 27;
    vm.stack[28].as_i32 = 10;
    do_inst(&vm);
    EXIT_IF(vm.index.inst != 1);
    EXIT_IF(vm.index.stack_top != 28);
    EXIT_IF(vm.index.stack_base != 10);
})

TEST(test_jpz, {
    Vm vm = {0};
    vm.insts[0].tag = INST_JPZ;
    vm.insts[0].op = 17;
    vm.index.stack_top = 1;
    vm.stack[0].as_i32 = 1;
    do_inst(&vm);
    EXIT_IF(vm.index.inst != 1);
    EXIT_IF(vm.index.stack_top != 0);
    vm.insts[1].tag = INST_JPZ;
    vm.insts[1].op = 23;
    vm.index.stack_top = 1;
    vm.stack[0].as_i32 = 0;
    do_inst(&vm);
    EXIT_IF(vm.index.inst != 23);
    EXIT_IF(vm.index.stack_top != 0);
})

TEST(test_jump, {
    Vm vm = {0};
    vm.index.inst = 101;
    vm.insts[101].tag = INST_JUMP;
    vm.insts[101].op = 9;
    do_inst(&vm);
    EXIT_IF(vm.index.inst != 9);
})

TEST(test_not, {
    Vm vm = {0};
    vm.insts[0].tag = INST_NOT;
    vm.index.stack_top = 1;
    vm.stack[0].as_i32 = 1;
    do_inst(&vm);
    EXIT_IF(vm.index.inst != 1);
    EXIT_IF(vm.index.stack_top != 1);
    EXIT_IF(vm.stack[0].as_i32 != 0);
    vm.insts[1].tag = INST_NOT;
    do_inst(&vm);
    EXIT_IF(vm.index.inst != 2);
    EXIT_IF(vm.index.stack_top != 1);
    EXIT_IF(vm.stack[0].as_i32 != 1);
})

TEST(test_eq, {
    Vm vm = {0};
    vm.insts[0].tag = INST_EQ;
    vm.index.stack_top = 2;
    vm.stack[0].as_i32 = 666;
    vm.stack[1].as_i32 = 0;
    do_inst(&vm);
    EXIT_IF(vm.index.inst != 1);
    EXIT_IF(vm.index.stack_top != 1);
    EXIT_IF(vm.stack[0].as_i32 != 0);
    vm.insts[1].tag = INST_EQ;
    vm.index.stack_top = 2;
    vm.stack[1].as_i32 = 0;
    do_inst(&vm);
    EXIT_IF(vm.index.inst != 2);
    EXIT_IF(vm.index.stack_top != 1);
    EXIT_IF(vm.stack[0].as_i32 != 1);
})

#define TEST_BINARY_OP(fn, inst_tag, in, out, expected) \
    TEST(fn, {                                          \
        Vm vm = {0};                                    \
        vm.insts[0].tag = inst_tag;                     \
        vm.index.stack_top = 2;                         \
        vm.stack[0].as_##in = 15;                       \
        vm.stack[1].as_##in = 9;                        \
        do_inst(&vm);                                   \
        EXIT_IF(vm.index.inst != 1);                    \
        EXIT_IF(vm.index.stack_top != 1);               \
        EXIT_IF(vm.stack[0].as_##out != (expected));    \
    })

TEST_BINARY_OP(test_addi, INST_ADDI, i32, i32, 24)
TEST_BINARY_OP(test_subi, INST_SUBI, i32, i32, 6)
TEST_BINARY_OP(test_muli, INST_MULI, i32, i32, 135)
TEST_BINARY_OP(test_divi, INST_DIVI, i32, i32, 1)

TEST_BINARY_OP(test_lti, INST_LTI, i32, i32, 0)
TEST_BINARY_OP(test_lei, INST_LEI, i32, i32, 0)
TEST_BINARY_OP(test_gti, INST_GTI, i32, i32, 1)
TEST_BINARY_OP(test_gei, INST_GEI, i32, i32, 1)

TEST_BINARY_OP(test_ltf, INST_LTF, f32, i32, 0)
TEST_BINARY_OP(test_lef, INST_LEF, f32, i32, 0)
TEST_BINARY_OP(test_gtf, INST_GTF, f32, i32, 1)
TEST_BINARY_OP(test_gef, INST_GEF, f32, i32, 1)

i32 main(void) {
    test_push();
    test_top();
    test_copy();
    test_store();
    test_call();
    test_ret();
    test_save();
    test_frame();
    test_reset();
    test_jpz();
    test_jump();
    test_not();
    test_eq();
    test_addi();
    test_subi();
    test_muli();
    test_divi();
    test_lti();
    test_lei();
    test_gti();
    test_gei();
    test_ltf();
    test_lef();
    test_gtf();
    test_gef();
    printf("\n");
    return EXIT_SUCCESS;
}
