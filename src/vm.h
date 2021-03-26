#ifndef __VM_H__
#define __VM_H__

#include "prelude.h"

STATIC_ASSERT(sizeof(i32) == sizeof(f32), "sizeof(i32) != sizeof(f32)");
STATIC_ASSERT(alignof(i8) == 1, "alignof(i8) != 1");
STATIC_ASSERT(alignof(i16) == 2, "alignof(i16) != 2");
STATIC_ASSERT(alignof(i32) == 4, "alignof(i32) != 4");

#define CAP_INST  256
#define CAP_STACK 256
#define CAP_HEAP8 256

typedef enum {
    INST_HALT = 0,

    INST_PUSH,

    INST_TOP,

    INST_COPY,
    INST_PUT,

    INST_CALL,
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
} InstTag;

typedef struct {
    i32     op;
    InstTag tag;
} Inst;

typedef struct {
    i32 inst;
    i32 stack_top;
    i32 stack_base;
} Index;

typedef union {
    i32 as_i32;
    f32 as_f32;
} Word;

typedef struct {
    Inst  insts[CAP_INST];
    Word  stack[CAP_STACK];
    i8    heap[CAP_HEAP8];
    Index index;
    Bool  alive;
} Vm;

typedef enum {
    NATIVE_NOP = 0,
    NATIVE_PRINTC,
    NATIVE_PRINTI,
    NATIVE_PRINTF,
    NATIVE_PRINTS,
    COUNT_NATIVE,
} Natives;

typedef void (*Native)(Vm*);

#endif
