#ifndef __VM_H__
#define __VM_H__

#include "prelude.h"

#define CAP_INST  256
#define CAP_STACK 256

typedef enum {
    INST_HALT = 0,

    INST_PUSH,

    INST_TOP,

    INST_COPY,
    INST_PUT,

    INST_CALL,
    INST_RET,

    INST_SAVE,
    INST_FRAME,
    INST_RESET,

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

STATIC_ASSERT(sizeof(i32) == sizeof(f32), "sizeof(i32) != sizeof(f32)");

typedef struct {
    Inst  insts[CAP_INST];
    Word  stack[CAP_STACK];
    Index index;
    Bool  alive;
} Vm;

typedef enum {
    NATIVE_PRINTC = 0,
    NATIVE_PRINTI,
    NATIVE_PRINTF,
    COUNT_NATIVE,
} Natives;

typedef void (*Native)(Vm*);

#endif
