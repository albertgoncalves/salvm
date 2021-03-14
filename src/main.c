#include "prelude.h"

#define CAP_INST  64
#define CAP_STACK 128

typedef enum {
    INST_HALT = 0,

    INST_PUSH,

    INST_RSRV,
    INST_DROP,

    INST_COPY,
    INST_STORE,

    INST_BURY,

    INST_EQ,
    INST_NOT,

    INST_JPZ,
    INST_JUMP,

    INST_CALL,
    INST_RET,

    INST_SAVE,
    INST_FRAME,
    INST_RESET,

    INST_ADDI,
    INST_SUBI,
    INST_MULI,
    INST_DIVI,

    INST_ADDF,
    INST_SUBF,
    INST_MULF,
    INST_DIVF,

    INST_LTI,
    INST_LEI,
    INST_GTI,
    INST_GEI,

    INST_LTF,
    INST_LEF,
    INST_GTF,
    INST_GEF,

    COUNT_INST_TAG,
} InstTag;

typedef union {
    u32 as_u32;
    u16 as_u16;
    i16 as_i16;
    u8  as_u8;
    u8  as_pair_u8[2];
} InstOp;

typedef struct {
    InstOp  op;
    InstTag tag;
} Inst;

typedef struct {
    u32 magic;
    u32 count_inst;
} Header;

typedef struct {
    u32 inst;
    u32 stack_top;
    u32 stack_base;
} Index;

typedef struct {
    Inst  insts[CAP_INST];
    u32   stack[CAP_STACK];
    Index index;
    Bool  alive;
} Vm;

i32 main(void) {
    printf("sizeof(Inst)   : %zu\n"
           "sizeof(Header) : %zu\n"
           "sizeof(Vm)     : %zu\n",
           sizeof(Inst),
           sizeof(Header),
           sizeof(Vm));
    return EXIT_SUCCESS;
}
