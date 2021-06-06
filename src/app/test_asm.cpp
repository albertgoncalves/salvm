#include "asm.hpp"
#include "test.hpp"

#define EPSILON 0.001f

static Memory* MEMORY;

#define INJECT(literal)                                  \
    {                                                    \
        EXIT_IF(CAP_CHARS < sizeof(literal));            \
        memcpy(MEMORY->chars, literal, sizeof(literal)); \
        MEMORY->len_chars = sizeof(literal) - 1;         \
        set_tokens(MEMORY);                              \
        set_insts(MEMORY);                               \
    }

TEST(test_plus_chars, {
    INJECT("+\"\\\"abcd\\\"\\n\"\n+\"!\n\"\n");
    EXIT_IF(MEMORY->len_bytes != 9);
    EXIT_IF(MEMORY->vm.heap[0] != '"');
    EXIT_IF(MEMORY->vm.heap[1] != 'a');
    EXIT_IF(MEMORY->vm.heap[2] != 'b');
    EXIT_IF(MEMORY->vm.heap[3] != 'c');
    EXIT_IF(MEMORY->vm.heap[4] != 'd');
    EXIT_IF(MEMORY->vm.heap[5] != '"');
    EXIT_IF(MEMORY->vm.heap[6] != '\n');
    EXIT_IF(MEMORY->vm.heap[7] != '!');
    EXIT_IF(MEMORY->vm.heap[8] != '\n');
})

TEST(test_plus_i8, {
    INJECT("+i8[-128 127 99 100]");
    EXIT_IF(MEMORY->len_bytes != 4);
    EXIT_IF(MEMORY->vm.heap[0] != -128);
    EXIT_IF(MEMORY->vm.heap[1] != 127);
    EXIT_IF(MEMORY->vm.heap[2] != 'c');
    EXIT_IF(MEMORY->vm.heap[3] != 'd');
})

TEST(test_plus_i16, {
    INJECT("+i16[ 32767 -32768 99 -100 ]\n");
    EXIT_IF(MEMORY->len_bytes != 8);
    EXIT_IF(reinterpret_cast<i16*>(MEMORY->vm.heap)[0] != 32767);
    EXIT_IF(reinterpret_cast<i16*>(MEMORY->vm.heap)[1] != -32768);
    EXIT_IF(reinterpret_cast<i16*>(MEMORY->vm.heap)[2] != 99);
    EXIT_IF(reinterpret_cast<i16*>(MEMORY->vm.heap)[3] != -100);
})

TEST(test_plus_i32, {
    INJECT("+i32[\n    -97\n    98\n    2147483647\n    -2147483648\n]\n");
    EXIT_IF(MEMORY->len_bytes != 16);
    EXIT_IF(reinterpret_cast<i32*>(MEMORY->vm.heap)[0] != -97);
    EXIT_IF(reinterpret_cast<i32*>(MEMORY->vm.heap)[1] != 98);
    EXIT_IF(reinterpret_cast<i32*>(MEMORY->vm.heap)[2] != 2147483647);
    EXIT_IF(reinterpret_cast<i32*>(MEMORY->vm.heap)[3] != -2147483648);
})

TEST(test_halt, {
    INJECT("halt\n");
    EXIT_IF(MEMORY->len_chars != 5);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_HALT);
})

TEST(test_push, {
    INJECT("push 2147483647\n"
           "push -2147483648\n"
           "\n"
           "f: push -12345.6789\n"
           "push f\n");
    EXIT_IF(MEMORY->len_chars != 61);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_PUSH);
    EXIT_IF(MEMORY->vm.insts[0].op != 2147483647);
    EXIT_IF(MEMORY->vm.insts[1].tag != INST_PUSH);
    EXIT_IF(MEMORY->vm.insts[1].op != -2147483648);
    EXIT_IF(MEMORY->vm.insts[2].tag != INST_PUSH);
    const f32 op = *(reinterpret_cast<f32*>(&MEMORY->vm.insts[2].op));
    EXIT_IF((op < (-12345.6789f - EPSILON)) ||
            ((-12345.6789f + EPSILON) < op));
    EXIT_IF(MEMORY->vm.insts[3].tag != INST_PUSH);
    EXIT_IF(MEMORY->vm.insts[3].op != 2);
})

TEST(test_top, {
    INJECT("top 10\ntop -5\n");
    EXIT_IF(MEMORY->len_chars != 14);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_TOP);
    EXIT_IF(MEMORY->vm.insts[0].op != 10);
    EXIT_IF(MEMORY->vm.insts[1].tag != INST_TOP);
    EXIT_IF(MEMORY->vm.insts[1].op != -5);
})

TEST(test_copy, {
    INJECT("copy 2\ncopy -1\n");
    EXIT_IF(MEMORY->len_chars != 15);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_COPY);
    EXIT_IF(MEMORY->vm.insts[0].op != 2);
    EXIT_IF(MEMORY->vm.insts[1].tag != INST_COPY);
    EXIT_IF(MEMORY->vm.insts[1].op != -1);
})

TEST(test_put, {
    INJECT("put -3\nput 4\n");
    EXIT_IF(MEMORY->len_chars != 13);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_PUT);
    EXIT_IF(MEMORY->vm.insts[0].op != -3);
    EXIT_IF(MEMORY->vm.insts[1].tag != INST_PUT);
    EXIT_IF(MEMORY->vm.insts[1].op != 4);
})

TEST(test_call, {
    INJECT("\n\ncall f\nf: halt\ncall 4");
    EXIT_IF(MEMORY->len_chars != 23);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_CALL);
    EXIT_IF(MEMORY->vm.insts[0].op != 1);
    EXIT_IF(MEMORY->vm.insts[1].tag != INST_HALT);
    EXIT_IF(MEMORY->vm.insts[2].tag != INST_CALL);
    EXIT_IF(MEMORY->vm.insts[2].op != 4);
})

TEST(test_scl, {
    INJECT("scl\n");
    EXIT_IF(MEMORY->len_chars != 4);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_SCL);
})

TEST(test_ret, {
    INJECT("ret\n");
    EXIT_IF(MEMORY->len_chars != 4);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_RET);
})

TEST(test_base, {
    INJECT("base\n");
    EXIT_IF(MEMORY->len_chars != 5);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_BASE);
})

TEST(test_frame, {
    INJECT("frame 321\n");
    EXIT_IF(MEMORY->len_chars != 10);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_FRAME);
    EXIT_IF(MEMORY->vm.insts[0].op != 321);
})

TEST(test_reset, {
    INJECT("reset\n");
    EXIT_IF(MEMORY->len_chars != 6);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_RESET);
})

TEST(test_jpz, {
    INJECT("f: jpz 9\njpz f");
    EXIT_IF(MEMORY->len_chars != 14);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_JPZ);
    EXIT_IF(MEMORY->vm.insts[0].op != 9);
    EXIT_IF(MEMORY->vm.insts[1].tag != INST_JPZ);
    EXIT_IF(MEMORY->vm.insts[1].op != 0);
})

TEST(test_jump, {
    INJECT("jump 8\nf: jump f");
    EXIT_IF(MEMORY->len_chars != 16);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_JUMP);
    EXIT_IF(MEMORY->vm.insts[0].op != 8);
    EXIT_IF(MEMORY->vm.insts[1].tag != INST_JUMP);
    EXIT_IF(MEMORY->vm.insts[1].op != 1);
})

TEST(test_rd8, {
    INJECT("rd8\n");
    EXIT_IF(MEMORY->len_chars != 4);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_RD8);
})

TEST(test_rd16, {
    INJECT("rd16\n");
    EXIT_IF(MEMORY->len_chars != 5);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_RD16);
})

TEST(test_rd32, {
    INJECT("rd32\n");
    EXIT_IF(MEMORY->len_chars != 5);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_RD32);
})

TEST(test_sv8, {
    INJECT("sv8\n");
    EXIT_IF(MEMORY->len_chars != 4);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_SV8);
})

TEST(test_sv16, {
    INJECT("sv16\n");
    EXIT_IF(MEMORY->len_chars != 5);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_SV16);
})

TEST(test_sv32, {
    INJECT("sv32\n");
    EXIT_IF(MEMORY->len_chars != 5);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_SV32);
})

TEST(test_not, {
    INJECT("not\n");
    EXIT_IF(MEMORY->len_chars != 4);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_NOT);
})

TEST(test_eq, {
    INJECT("eq\n");
    EXIT_IF(MEMORY->len_chars != 3);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_EQ);
})

TEST(test_addi, {
    INJECT("addi\n");
    EXIT_IF(MEMORY->len_chars != 5);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_ADDI);
})

TEST(test_subi, {
    INJECT("subi\n");
    EXIT_IF(MEMORY->len_chars != 5);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_SUBI);
})

TEST(test_muli, {
    INJECT("muli\n");
    EXIT_IF(MEMORY->len_chars != 5);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_MULI);
})

TEST(test_divi, {
    INJECT("divi\n");
    EXIT_IF(MEMORY->len_chars != 5);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_DIVI);
})

TEST(test_addf, {
    INJECT("addf\n");
    EXIT_IF(MEMORY->len_chars != 5);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_ADDF);
})

TEST(test_subf, {
    INJECT("subf\n");
    EXIT_IF(MEMORY->len_chars != 5);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_SUBF);
})

TEST(test_mulf, {
    INJECT("mulf\n");
    EXIT_IF(MEMORY->len_chars != 5);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_MULF);
})

TEST(test_divf, {
    INJECT("divf\n");
    EXIT_IF(MEMORY->len_chars != 5);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_DIVF);
})

TEST(test_lti, {
    INJECT("lti\n");
    EXIT_IF(MEMORY->len_chars != 4);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_LTI);
})

TEST(test_lei, {
    INJECT("lei\n");
    EXIT_IF(MEMORY->len_chars != 4);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_LEI);
})

TEST(test_gti, {
    INJECT("gti\n");
    EXIT_IF(MEMORY->len_chars != 4);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_GTI);
})

TEST(test_gei, {
    INJECT("gei\n");
    EXIT_IF(MEMORY->len_chars != 4);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_GEI);
})

TEST(test_ltf, {
    INJECT("ltf\n");
    EXIT_IF(MEMORY->len_chars != 4);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_LTF);
})

TEST(test_lef, {
    INJECT("lef\n");
    EXIT_IF(MEMORY->len_chars != 4);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_LEF);
})

TEST(test_gtf, {
    INJECT("gtf\n");
    EXIT_IF(MEMORY->len_chars != 4);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_GTF);
})

TEST(test_gef, {
    INJECT("gef\n");
    EXIT_IF(MEMORY->len_chars != 4);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_GEF);
})

TEST(test_native, {
    INJECT("native 1");
    EXIT_IF(MEMORY->len_chars != 8);
    EXIT_IF(MEMORY->vm.insts[0].tag != INST_NATIVE);
    EXIT_IF(MEMORY->vm.insts[0].op != 1);
})

#define TEST_STR(x, literal, len_)               \
    {                                            \
        const String result = to_string(x);      \
        EXIT_IF(result.len != len_);             \
        const String expected = {literal, len_}; \
        EXIT_IF(!EQ_STR(result, expected));      \
    }

TEST(test_halt_as_string, { TEST_STR(INST_HALT, "halt", 4); })

TEST(test_scl_as_string, { TEST_STR(INST_SCL, "scl", 3); })

TEST(test_eq_as_string, { TEST_STR(INST_EQ, "eq", 2); })

TEST(test_jpz_as_string, { TEST_STR(INST_JPZ, "jpz", 3); })

TEST(test_native_as_string, { TEST_STR(INST_NATIVE, "native", 6); })

TEST(test_size_i32_as_string, { TEST_STR(SIZE_I32, "i32", 3); })

i32 main() {
    printf("sizeof(TokenTag)  : %zu\n"
           "sizeof(String)    : %zu\n"
           "sizeof(TokenBody) : %zu\n"
           "sizeof(Token)     : %zu\n"
           "sizeof(PreInst)   : %zu\n"
           "sizeof(Label)     : %zu\n"
           "sizeof(Memory)    : %zu\n",
           sizeof(TokenTag),
           sizeof(String),
           sizeof(TokenBody),
           sizeof(Token),
           sizeof(PreInst),
           sizeof(Label),
           sizeof(Memory));
    MEMORY = reinterpret_cast<Memory*>(calloc(1, sizeof(Memory)));
    EXIT_IF(!MEMORY);
    test_plus_chars();
    test_plus_i8();
    test_plus_i16();
    test_plus_i32();
    test_halt();
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
    test_halt_as_string();
    test_eq_as_string();
    test_jpz_as_string();
    test_native_as_string();
    test_scl_as_string();
    test_size_i32_as_string();
    free(MEMORY);
    printf("\n\n");
    return EXIT_SUCCESS;
}
