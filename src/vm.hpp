#ifndef __VM_H__
#define __VM_H__

#include "prelude.hpp"

static_assert(sizeof(i32) == sizeof(f32), "sizeof(i32) != sizeof(f32)");
static_assert(alignof(i8) == 1, "alignof(i8) != 1");
static_assert(alignof(i16) == 2, "alignof(i16) != 2");
static_assert(alignof(i32) == 4, "alignof(i32) != 4");

#define CAP_INSTS (2 << 7)
#define CAP_STACK (2 << 14)
#define CAP_HEAP8 (2 << 7)

enum InstTag {
    INST_HALT = 0,

    INST_PUSH,

    INST_TOP,

    INST_COPY,
    INST_PUT,

    INST_CALL,
    INST_SCL,
    INST_RET,

    INST_BASE,
    INST_FRAME,
    INST_RESET,

    INST_RD8,
    INST_RD16,
    INST_RD32,

    INST_SV8,
    INST_SV16,
    INST_SV32,

    INST_JPZ,
    INST_JUMP,

    INST_NOT,
    INST_EQ,

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

    INST_NATIVE,

    COUNT_INST_TAG,
};

struct Inst {
    i32     op;
    InstTag tag;
};

struct Index {
    i32 inst;
    i32 stack_top;
    i32 stack_base;
};

union Word {
    i32 as_i32;
    f32 as_f32;
};

struct Vm {
    Inst  insts[CAP_INSTS];
    Word  stack[CAP_STACK];
    i8    heap[CAP_HEAP8];
    Index index;
    Bool  alive;
};

enum Natives {
    NATIVE_NOP = 0,
    NATIVE_PRINTC,
    NATIVE_PRINTI,
    NATIVE_PRINTF,
    NATIVE_PRINTS,
    COUNT_NATIVE,
};

typedef void (*Native)(Vm*);

#endif
