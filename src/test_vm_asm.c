#include "vm_asm.h"

#define EPSILON 0.001f

static Memory* MEMORY;

#define INJECT(x)                      \
    {                                  \
        MEMORY->len_chars = 0;         \
        MEMORY->len_tokens = 0;        \
        MEMORY->len_pre_insts = 0;     \
        MEMORY->len_labels = 0;        \
        usize len = strlen(x);         \
        EXIT_IF(CAP_CHAR <= len);      \
        memcpy(MEMORY->chars, x, len); \
        MEMORY->len_chars = (u32)len;  \
        set_tokens(MEMORY);            \
        set_insts(MEMORY);             \
    }

TEST(test_halt, {
    INJECT("halt\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_HALT);
})

TEST(test_push, {
    INJECT("push 567890\npush -12345\n\npush -12345.6789\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_PUSH);
    EXIT_IF(MEMORY->vm.insts[0].op != 567890);
    EXIT_IF(MEMORY->vm.insts[1].tag != INST_PUSH);
    EXIT_IF(MEMORY->vm.insts[1].op != -12345);
    EXIT_IF(MEMORY->vm.insts[2].tag != INST_PUSH);
    f32 op = *((f32*)(&MEMORY->vm.insts[2].op));
    EXIT_IF(op < (-12345.6789f - EPSILON));
    EXIT_IF((-12345.6789f + EPSILON) < op);
})

TEST(test_top, {
    INJECT("top 10\ntop -5\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_TOP);
    EXIT_IF(MEMORY->vm.insts[0].op != 10);
    EXIT_IF(MEMORY->vm.insts[1].tag != INST_TOP);
    EXIT_IF(MEMORY->vm.insts[1].op != -5);
})

TEST(test_copy, {
    INJECT("copy 2\ncopy -1\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_COPY);
    EXIT_IF(MEMORY->vm.insts[0].op != 2);
    EXIT_IF(MEMORY->vm.insts[1].tag != INST_COPY);
    EXIT_IF(MEMORY->vm.insts[1].op != -1);
})

TEST(test_put, {
    INJECT("put -3\nput 4\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_PUT);
    EXIT_IF(MEMORY->vm.insts[0].op != -3);
    EXIT_IF(MEMORY->vm.insts[1].tag != INST_PUT);
    EXIT_IF(MEMORY->vm.insts[1].op != 4);
})

TEST(test_call, {
    INJECT("\n\ncall f\nf: halt\ncall 4");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_CALL);
    EXIT_IF(MEMORY->vm.insts[0].op != 1);
    EXIT_IF(MEMORY->vm.insts[1].tag != INST_HALT);
    EXIT_IF(MEMORY->vm.insts[2].tag != INST_CALL);
    EXIT_IF(MEMORY->vm.insts[2].op != 4);
})

TEST(test_ret, {
    INJECT("ret\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_RET);
})

TEST(test_save, {
    INJECT("save\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_SAVE);
})

TEST(test_frame, {
    INJECT("frame 321\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_FRAME);
    EXIT_IF(MEMORY->vm.insts[0].op != 321);
})

TEST(test_reset, {
    INJECT("reset\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_RESET);
})

TEST(test_jpz, {
    INJECT("f: jpz 9\njpz f");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_JPZ);
    EXIT_IF(MEMORY->vm.insts[0].op != 9);
    EXIT_IF(MEMORY->vm.insts[1].tag != INST_JPZ);
    EXIT_IF(MEMORY->vm.insts[1].op != 0);
})

TEST(test_jump, {
    INJECT("jump 8\nf: jump f");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_JUMP);
    EXIT_IF(MEMORY->vm.insts[0].op != 8);
    EXIT_IF(MEMORY->vm.insts[1].tag != INST_JUMP);
    EXIT_IF(MEMORY->vm.insts[1].op != 1);
})

TEST(test_not, {
    INJECT("not\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_NOT);
})

TEST(test_eq, {
    INJECT("eq\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_EQ);
})

TEST(test_addi, {
    INJECT("addi\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_ADDI);
})

TEST(test_subi, {
    INJECT("subi\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_SUBI);
})

TEST(test_muli, {
    INJECT("muli\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_MULI);
})

TEST(test_divi, {
    INJECT("divi\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_DIVI);
})

TEST(test_addf, {
    INJECT("addf\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_ADDF);
})

TEST(test_subf, {
    INJECT("subf\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_SUBF);
})

TEST(test_mulf, {
    INJECT("mulf\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_MULF);
})

TEST(test_divf, {
    INJECT("divf\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_DIVF);
})

TEST(test_lti, {
    INJECT("lti\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_LTI);
})

TEST(test_lei, {
    INJECT("lei\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_LEI);
})

TEST(test_gti, {
    INJECT("gti\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_GTI);
})

TEST(test_gei, {
    INJECT("gei\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_GEI);
})

TEST(test_ltf, {
    INJECT("ltf\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_LTF);
})

TEST(test_lef, {
    INJECT("lef\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_LEF);
})

TEST(test_gtf, {
    INJECT("gtf\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_GTF);
})

TEST(test_gef, {
    INJECT("gef\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_GEF);
})

TEST(test_native, {
    INJECT("native 1\n");
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_NATIVE);
    EXIT_IF(MEMORY->vm.insts[0].op != 1);
})

i32 main(void) {
    MEMORY = calloc(1, sizeof(Memory));
    test_halt();
    test_push();
    test_top();
    test_copy();
    test_put();
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
    test_addf();
    test_subf();
    test_mulf();
    test_divf();
    test_lti();
    test_lei();
    test_gti();
    test_gei();
    test_ltf();
    test_lef();
    test_gtf();
    test_gef();
    test_native();
    free(MEMORY);
    printf("\n");
    return EXIT_SUCCESS;
}
