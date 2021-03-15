#ifndef __VM_H__
#define __VM_H__

#include "prelude.h"

#define CAP_INST  256
#define CAP_STACK 256

typedef enum {
    INST_HALT = 0,

    INST_PUSH,

    INST_RSRV,
    INST_DROP,

    INST_COPY,
    INST_STORE,

    INST_BURY,

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

    COUNT_INST_TAG,
} InstTag;

typedef struct {
    i32     op;
    InstTag tag;
} Inst;

typedef struct {
    u32 magic;
    u32 count_inst;
} Header;

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
    Index index;
    Bool  alive;
} Vm;

#ifdef DEBUG_BOUNDS_CHECK
    #define DEBUG_BOUNDS_CHECK_STACK(index) \
        {                                   \
            EXIT_IF(CAP_STACK <= (index));  \
            EXIT_IF((index) < 0);           \
        }
    #define BINARY_OP(vm, op, in, out)                             \
        {                                                          \
            EXIT_IF(vm->index.stack_top < 2);                      \
            vm->stack[vm->index.stack_top].as_##out =              \
                vm->stack[vm->index.stack_top - 2]                 \
                    .as_##in op vm->stack[vm->index.stack_top - 1] \
                    .as_##in;                                      \
            --vm->index.stack_top;                                 \
            ++vm->index.inst;                                      \
            break;                                                 \
        }
#else
    #define DEBUG_BOUNDS_CHECK_STACK(_) \
        {}
    #define BINARY_OP(vm, op, in, out)                             \
        {                                                          \
            vm->stack[vm->index.stack_top - 2].as_##out =          \
                vm->stack[vm->index.stack_top - 2]                 \
                    .as_##in op vm->stack[vm->index.stack_top - 1] \
                    .as_##in;                                      \
            --vm->index.stack_top;                                 \
            ++vm->index.inst;                                      \
            break;                                                 \
        }
#endif

static void do_inst(Vm* vm) {
#ifdef DEBUG_BOUNDS_CHECK
    {
        EXIT_IF(CAP_INST <= vm->index.inst);
        EXIT_IF(vm->index.inst < 0);
    }
#endif
    Inst inst = vm->insts[vm->index.inst];
    switch (inst.tag) {
    case INST_HALT: {
        vm->alive = FALSE;
        break;
    }
    case INST_PUSH: {
        DEBUG_BOUNDS_CHECK_STACK(vm->index.stack_top);
        vm->stack[vm->index.stack_top++].as_i32 = inst.op;
        ++vm->index.inst;
        break;
    }
    case INST_RSRV: {
        vm->index.stack_top += inst.op;
        ++vm->index.inst;
        break;
    }
    case INST_DROP: {
        vm->index.stack_top -= inst.op;
        ++vm->index.inst;
        break;
    }
    case INST_COPY: {
        DEBUG_BOUNDS_CHECK_STACK(vm->index.stack_top);
        DEBUG_BOUNDS_CHECK_STACK(vm->index.stack_base + inst.op);
        vm->stack[vm->index.stack_top++].as_i32 =
            vm->stack[vm->index.stack_base + inst.op].as_i32;
        ++vm->index.inst;
        break;
    }
    case INST_STORE: {
        DEBUG_BOUNDS_CHECK_STACK(vm->index.stack_base + inst.op);
        --vm->index.stack_top;
        DEBUG_BOUNDS_CHECK_STACK(vm->index.stack_top);
        vm->stack[vm->index.stack_base + inst.op].as_i32 =
            vm->stack[vm->index.stack_top].as_i32;
        ++vm->index.inst;
        break;
    }
    case INST_BURY: {
        i32 n = vm->index.stack_top - 1;
        i32 x = vm->stack[n].as_i32;
        for (i32 i = n - inst.op; i < vm->index.stack_top; ++i) {
            DEBUG_BOUNDS_CHECK_STACK(i);
            i32 t = vm->stack[i].as_i32;
            vm->stack[i].as_i32 = x;
            x = t;
        }
        ++vm->index.inst;
        break;
    }
    case INST_CALL: {
        DEBUG_BOUNDS_CHECK_STACK(vm->index.stack_top);
        vm->stack[vm->index.stack_top++].as_i32 = vm->index.inst + 1;
        vm->index.inst = inst.op;
        break;
    }
    case INST_RET: {
        --vm->index.stack_top;
        DEBUG_BOUNDS_CHECK_STACK(vm->index.stack_top);
        vm->index.inst = vm->stack[vm->index.stack_top].as_i32;
        break;
    }
    case INST_SAVE: {
        DEBUG_BOUNDS_CHECK_STACK(vm->index.stack_top);
        vm->stack[vm->index.stack_top++].as_i32 = vm->index.stack_base;
        ++vm->index.inst;
        break;
    }
    case INST_FRAME: {
        vm->index.stack_base = vm->index.stack_top - inst.op;
        ++vm->index.inst;
        break;
    }
    case INST_RESET: {
        --vm->index.stack_top;
        DEBUG_BOUNDS_CHECK_STACK(vm->index.stack_top);
        vm->index.stack_base = vm->stack[vm->index.stack_top].as_i32;
        ++vm->index.inst;
        break;
    }
    case INST_JPZ: {
        --vm->index.stack_top;
        DEBUG_BOUNDS_CHECK_STACK(vm->index.stack_top);
        if (vm->stack[vm->index.stack_top].as_i32 == 0) {
            vm->index.inst += inst.op;
        } else {
            ++vm->index.inst;
        }
        break;
    }
    case INST_JUMP: {
        vm->index.inst += inst.op;
        break;
    }
    case INST_NOT: {
        DEBUG_BOUNDS_CHECK_STACK(vm->index.stack_top);
        DEBUG_BOUNDS_CHECK_STACK(vm->index.stack_top - 1);
        vm->stack[vm->index.stack_top].as_i32 =
            !vm->stack[vm->index.stack_top - 1].as_i32;
        ++vm->index.stack_top;
        ++vm->index.inst;
        break;
    }
    case INST_EQ: {
        DEBUG_BOUNDS_CHECK_STACK(vm->index.stack_top - 1);
        DEBUG_BOUNDS_CHECK_STACK(vm->index.stack_top - 2);
        vm->stack[vm->index.stack_top - 2].as_i32 =
            vm->stack[vm->index.stack_top - 2].as_i32 ==
                    vm->stack[vm->index.stack_top - 1].as_i32
                ? 1
                : 0;
        --vm->index.stack_top;
        ++vm->index.inst;
        break;
    }
    case INST_ADDI: {
        BINARY_OP(vm, +, i32, i32)
    }
    case INST_SUBI: {
        BINARY_OP(vm, -, i32, i32)
    }
    case INST_MULI: {
        BINARY_OP(vm, *, i32, i32)
    }
    case INST_DIVI: {
        BINARY_OP(vm, /, i32, i32)
    }
    case INST_ADDF: {
        BINARY_OP(vm, +, f32, f32)
    }
    case INST_SUBF: {
        BINARY_OP(vm, -, f32, f32)
    }
    case INST_MULF: {
        BINARY_OP(vm, *, f32, f32)
    }
    case INST_DIVF: {
        BINARY_OP(vm, /, f32, f32)
    }
    case INST_LTI: {
        BINARY_OP(vm, <, i32, i32)
    }
    case INST_LEI: {
        BINARY_OP(vm, <=, i32, i32)
    }
    case INST_GTI: {
        BINARY_OP(vm, >, i32, i32)
    }
    case INST_GEI: {
        BINARY_OP(vm, >=, i32, i32)
    }
    case INST_LTF: {
        BINARY_OP(vm, <, f32, i32)
    }
    case INST_LEF: {
        BINARY_OP(vm, <=, f32, i32)
    }
    case INST_GTF: {
        BINARY_OP(vm, >, f32, i32)
    }
    case INST_GEF: {
        BINARY_OP(vm, >=, f32, i32)
    }
    case COUNT_INST_TAG:
    default: {
        ERROR();
    }
    }
}

#endif
