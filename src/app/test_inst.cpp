#include "alloc.hpp"
#include "inst.hpp"
#include "test.hpp"

static Vm* VM;

#define RESET()         \
    {                   \
        VM->index = {}; \
    }

TEST(test_push, {
    RESET();
    VM->insts[0].tag = INST_PUSH;
    VM->insts[0].op = 12345;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 1);
    EXIT_IF(VM->stack[0].as_i32 != 12345);
})

TEST(test_top, {
    RESET();
    VM->insts[0].tag = INST_TOP;
    VM->insts[0].op = 5;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 5);
})

TEST(test_copy, {
    RESET();
    VM->insts[0].tag = INST_COPY;
    VM->insts[0].op = 1;
    VM->stack[2].as_i32 = 15;
    VM->index.stack_top = 3;
    VM->index.stack_base = 1;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 4);
    EXIT_IF(VM->stack[3].as_i32 != 15);
})

TEST(test_put, {
    RESET();
    VM->insts[0].tag = INST_PUT;
    VM->insts[0].op = 1;
    VM->stack[3].as_i32 = 15;
    VM->index.stack_top = 4;
    VM->index.stack_base = 1;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 3);
    EXIT_IF(VM->stack[2].as_i32 != 15);
})

TEST(test_call, {
    RESET();
    VM->insts[0].tag = INST_CALL;
    VM->insts[0].op = 99;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 99);
    EXIT_IF(VM->index.stack_top != 1);
    EXIT_IF(VM->stack[0].as_i32 != 1);
})

TEST(test_scl, {
    RESET();
    VM->insts[0].tag = INST_SCL;
    VM->stack[0].as_i32 = 99;
    VM->index.stack_top = 1;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 99);
    EXIT_IF(VM->index.stack_top != 1);
    EXIT_IF(VM->stack[0].as_i32 != 1);
})

TEST(test_ret, {
    RESET();
    VM->insts[0].tag = INST_RET;
    VM->stack[0].as_i32 = 68;
    VM->index.stack_top = 1;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 68);
    EXIT_IF(VM->index.stack_top != 0);
})

TEST(test_base, {
    RESET();
    VM->insts[0].tag = INST_BASE;
    VM->index.stack_base = 76;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 1);
    EXIT_IF(VM->stack[0].as_i32 != 76);
})

TEST(test_frame, {
    RESET();
    VM->insts[0].tag = INST_FRAME;
    VM->insts[0].op = 5;
    VM->index.stack_top = 11;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 11);
    EXIT_IF(VM->index.stack_base != 6);
})

TEST(test_reset, {
    RESET();
    VM->insts[0].tag = INST_RESET;
    VM->index.stack_top = 29;
    VM->index.stack_base = 27;
    VM->stack[28].as_i32 = 10;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 28);
    EXIT_IF(VM->index.stack_base != 10);
})

TEST(test_jpz, {
    RESET();
    VM->insts[0].tag = INST_JPZ;
    VM->insts[0].op = 17;
    VM->index.stack_top = 1;
    VM->stack[0].as_i32 = 1;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 0);
    VM->insts[1].tag = INST_JPZ;
    VM->insts[1].op = 23;
    VM->index.stack_top = 1;
    VM->stack[0].as_i32 = 0;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 23);
    EXIT_IF(VM->index.stack_top != 0);
})

TEST(test_jump, {
    RESET();
    VM->index.inst = 101;
    VM->insts[101].tag = INST_JUMP;
    VM->insts[101].op = 9;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 9);
})

TEST(test_rd8, {
    RESET();
    VM->insts[0].tag = INST_RD8;
    VM->index.stack_top = 1;
    VM->stack[0].as_i32 = 10;
    VM->heap[10] = -123;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 1);
    EXIT_IF(VM->stack[0].as_i32 != -123);
})

TEST(test_rd16, {
    RESET();
    VM->insts[0].tag = INST_RD16;
    VM->index.stack_top = 1;
    VM->stack[0].as_i32 = 7;
    reinterpret_cast<i16*>(VM->heap)[7] = -30000;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 1);
    EXIT_IF(VM->stack[0].as_i32 != -30000);
})

TEST(test_rd32, {
    RESET();
    VM->insts[0].tag = INST_RD32;
    VM->index.stack_top = 1;
    VM->stack[0].as_i32 = 3;
    reinterpret_cast<i32*>(VM->heap)[3] = -2000000123;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 1);
    EXIT_IF(VM->stack[0].as_i32 != -2000000123);
})

TEST(test_sv8, {
    RESET();
    VM->insts[0].tag = INST_SV8;
    VM->index.stack_top = 2;
    VM->stack[0].as_i32 = -113;
    VM->stack[1].as_i32 = 11;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 0);
    EXIT_IF(VM->heap[11] != -113);
})

TEST(test_sv16, {
    RESET();
    VM->insts[0].tag = INST_SV16;
    VM->index.stack_top = 2;
    VM->stack[0].as_i32 = -30012;
    VM->stack[1].as_i32 = 2;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 0);
    EXIT_IF(reinterpret_cast<i16*>(VM->heap)[2] != -30012);
})

TEST(test_sv32, {
    RESET();
    VM->insts[0].tag = INST_SV32;
    VM->index.stack_top = 2;
    VM->stack[0].as_i32 = -2000100123;
    VM->stack[1].as_i32 = 1;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 0);
    EXIT_IF(reinterpret_cast<i32*>(VM->heap)[1] != -2000100123);
})

TEST(test_rdf32, {
    RESET();
    VM->insts[0].tag = INST_RDF32;
    VM->index.stack_top = 1;
    VM->stack[0].as_i32 = 5;
    reinterpret_cast<f32*>(VM->heap)[5] = -0.1234567f;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 1);
    EXIT_IF(!EQ_F32(VM->stack[0].as_f32, -0.1234567f));
})

TEST(test_svf32, {
    RESET();
    VM->insts[0].tag = INST_SVF32;
    VM->index.stack_top = 2;
    VM->stack[0].as_f32 = 50.12345f;
    VM->stack[1].as_i32 = 9;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 0);
    EXIT_IF(!EQ_F32(reinterpret_cast<f32*>(VM->heap)[9], 50.12345f));
})

TEST(test_not, {
    RESET();
    VM->insts[0].tag = INST_NOT;
    VM->index.stack_top = 1;
    VM->stack[0].as_i32 = 1;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 1);
    EXIT_IF(VM->stack[0].as_i32 != 0);
    VM->insts[1].tag = INST_NOT;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 2);
    EXIT_IF(VM->index.stack_top != 1);
    EXIT_IF(VM->stack[0].as_i32 != 1);
})

TEST(test_eq, {
    RESET();
    VM->insts[0].tag = INST_EQ;
    VM->index.stack_top = 2;
    VM->stack[0].as_i32 = 666;
    VM->stack[1].as_i32 = 0;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 1);
    EXIT_IF(VM->stack[0].as_i32 != 0);
    VM->insts[1].tag = INST_EQ;
    VM->index.stack_top = 2;
    VM->stack[1].as_i32 = 0;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 2);
    EXIT_IF(VM->index.stack_top != 1);
    EXIT_IF(VM->stack[0].as_i32 != 1);
})

TEST(test_sigi, {
    RESET();
    VM->insts[0].tag = INST_SIGI;
    VM->index.stack_top = 1;
    VM->stack[0].as_i32 = 12345;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 1);
    EXIT_IF(VM->stack[0].as_i32 != -12345);
    VM->insts[1].tag = INST_SIGI;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 2);
    EXIT_IF(VM->index.stack_top != 1);
    EXIT_IF(VM->stack[0].as_i32 != 12345);
})

TEST(test_sigf, {
    RESET();
    VM->insts[0].tag = INST_SIGF;
    VM->index.stack_top = 1;
    VM->stack[0].as_f32 = 12345.0f;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
    EXIT_IF(VM->index.stack_top != 1);
    EXIT_IF(!EQ_F32(VM->stack[0].as_f32, -12345.0f));
    VM->insts[1].tag = INST_SIGF;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 2);
    EXIT_IF(VM->index.stack_top != 1);
    EXIT_IF(!EQ_F32(VM->stack[0].as_f32, 12345.0f));
})

#define TEST_BINARY_OP(fn, inst_tag, in, out, expected) \
    TEST(fn, {                                          \
        RESET();                                        \
        VM->insts[0].tag = inst_tag;                    \
        VM->index.stack_top = 2;                        \
        VM->stack[0].as_##in = 15;                      \
        VM->stack[1].as_##in = 9;                       \
        do_inst(VM);                                    \
        EXIT_IF(VM->index.inst != 1);                   \
        EXIT_IF(VM->index.stack_top != 1);              \
        EXIT_IF(VM->stack[0].as_##out != (expected));   \
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

TEST(test_native_nop, {
    RESET();
    VM->insts[0].tag = INST_NATIVE;
    VM->insts[0].op = 0;
    do_inst(VM);
    EXIT_IF(VM->index.inst != 1);
})

i32 main() {
    printf("sizeof(InstTag) : %zu\n"
           "sizeof(Inst)    : %zu\n"
           "sizeof(Index)   : %zu\n"
           "sizeof(Word)    : %zu\n"
           "sizeof(Vm)      : %zu\n"
           "sizeof(Natives) : %zu\n"
           "sizeof(Native)  : %zu\n",
           sizeof(InstTag),
           sizeof(Inst),
           sizeof(Index),
           sizeof(Word),
           sizeof(Vm),
           sizeof(Natives),
           sizeof(Native));
    VM = reinterpret_cast<Vm*>(alloc(sizeof(Vm)));
    test_push();
    test_top();
    test_copy();
    test_put();
    test_call();
    test_scl();
    test_ret();
    test_base();
    test_frame();
    test_reset();
    test_jpz();
    test_jump();
    test_rd8();
    test_rd16();
    test_rd32();
    test_sv8();
    test_sv16();
    test_sv32();
    test_rdf32();
    test_svf32();
    test_not();
    test_eq();
    test_sigi();
    test_sigf();
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
    test_native_nop();
    printf("\n\n");
    return EXIT_SUCCESS;
}
