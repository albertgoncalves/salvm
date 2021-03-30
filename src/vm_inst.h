#ifndef __VM_INST_H__
#define __VM_INST_H__

#include "vm.h"

#define BOUNDS_CHECK_STACK(index)      \
    {                                  \
        EXIT_IF(CAP_STACK <= (index)); \
        EXIT_IF((index) < 0);          \
    }

#define BOUNDS_CHECK_HEAP8(index)      \
    {                                  \
        EXIT_IF(CAP_HEAP8 <= (index)); \
        EXIT_IF((index) < 0);          \
    }

#define BINARY_OP(vm, op, in, out)                                            \
    {                                                                         \
        EXIT_IF(vm->index.stack_top < 2);                                     \
        i32 i = vm->index.stack_top - 1;                                      \
        i32 j = vm->index.stack_top - 2;                                      \
        EXIT_IF(CAP_STACK <= i)                                               \
        vm->stack[j].as_##out = vm->stack[j].as_##in op vm->stack[i].as_##in; \
        --vm->index.stack_top;                                                \
        ++vm->index.inst;                                                     \
        break;                                                                \
    }

#define NATIVE_1(fn, block)                      \
    static void fn(Vm* vm) {                     \
        --vm->index.stack_top;                   \
        BOUNDS_CHECK_STACK(vm->index.stack_top); \
        block;                                   \
    }

NATIVE_1(native_printc,
         { printf("%c", vm->stack[vm->index.stack_top].as_i32); })

NATIVE_1(native_printi,
         { printf("%d", vm->stack[vm->index.stack_top].as_i32); })

NATIVE_1(native_printf,
         { printf("%.2f", (f64)vm->stack[vm->index.stack_top].as_f32); })

static void native_prints(Vm* vm) {
    EXIT_IF(vm->index.stack_top < 2);
    i32 i = vm->index.stack_top - 1;
    i32 j = vm->index.stack_top - 2;
    EXIT_IF(CAP_STACK <= i)
    i32 k = vm->stack[i].as_i32;
    BOUNDS_CHECK_HEAP8(k);
    i32 l = vm->stack[j].as_i32;
    EXIT_IF(CAP_HEAP8 <= (k + l));
    printf("%.*s", l, (char*)&vm->heap[k]);
    vm->index.stack_top -= 2;
}

static void native_nop(Vm* _) {
}

static const Native NATIVES[COUNT_NATIVE] = {
    [NATIVE_NOP] = native_nop,
    [NATIVE_PRINTC] = native_printc,
    [NATIVE_PRINTI] = native_printi,
    [NATIVE_PRINTF] = native_printf,
    [NATIVE_PRINTS] = native_prints,
};

static void do_inst(Vm* vm) {
    {
        EXIT_IF(CAP_INST <= vm->index.inst);
        EXIT_IF(vm->index.inst < 0);
    }
    Inst inst = vm->insts[vm->index.inst];
    switch (inst.tag) {
    case INST_HALT: {
        vm->alive = FALSE;
        break;
    }
    case INST_PUSH: {
        BOUNDS_CHECK_STACK(vm->index.stack_top);
        vm->stack[vm->index.stack_top++].as_i32 = inst.op;
        ++vm->index.inst;
        break;
    }
    case INST_TOP: {
        vm->index.stack_top += inst.op;
        ++vm->index.inst;
        break;
    }
    case INST_COPY: {
        BOUNDS_CHECK_STACK(vm->index.stack_top);
        i32 i = vm->index.stack_base + inst.op;
        BOUNDS_CHECK_STACK(i);
        vm->stack[vm->index.stack_top++].as_i32 = vm->stack[i].as_i32;
        ++vm->index.inst;
        break;
    }
    case INST_PUT: {
        EXIT_IF(vm->index.stack_top < 2);
        --vm->index.stack_top;
        BOUNDS_CHECK_STACK(vm->index.stack_top);
        i32 i = vm->index.stack_base + inst.op;
        BOUNDS_CHECK_STACK(i);
        vm->stack[i].as_i32 = vm->stack[vm->index.stack_top].as_i32;
        ++vm->index.inst;
        break;
    }
    case INST_CALL: {
        BOUNDS_CHECK_STACK(vm->index.stack_top);
        vm->stack[vm->index.stack_top++].as_i32 = vm->index.inst + 1;
        vm->index.inst = inst.op;
        break;
    }
    case INST_SCL: {
        i32 i = vm->index.stack_top - 1;
        BOUNDS_CHECK_STACK(i);
        i32 j = vm->stack[i].as_i32;
        vm->stack[i].as_i32 = vm->index.inst + 1;
        vm->index.inst = j;
        break;
    }
    case INST_RET: {
        --vm->index.stack_top;
        BOUNDS_CHECK_STACK(vm->index.stack_top);
        vm->index.inst = vm->stack[vm->index.stack_top].as_i32;
        break;
    }
    case INST_BASE: {
        BOUNDS_CHECK_STACK(vm->index.stack_top);
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
        BOUNDS_CHECK_STACK(vm->index.stack_top);
        vm->index.stack_base = vm->stack[vm->index.stack_top].as_i32;
        ++vm->index.inst;
        break;
    }
    case INST_RD8: {
        i32 i = vm->index.stack_top - 1;
        BOUNDS_CHECK_STACK(i);
        i32 j = vm->stack[i].as_i32;
        BOUNDS_CHECK_HEAP8(j);
        vm->stack[i].as_i32 = (i32)(vm->heap[j]);
        ++vm->index.inst;
        break;
    }
    case INST_RD16: {
        i32 i = vm->index.stack_top - 1;
        BOUNDS_CHECK_STACK(i);
        i32 j = vm->stack[i].as_i32;
        BOUNDS_CHECK_HEAP8(j * 2);
        i16* heap = (i16*)vm->heap;
        vm->stack[i].as_i32 = (i32)(heap[j]);
        ++vm->index.inst;
        break;
    }
    case INST_RD32: {
        i32 i = vm->index.stack_top - 1;
        BOUNDS_CHECK_STACK(i);
        i32 j = vm->stack[i].as_i32;
        BOUNDS_CHECK_HEAP8(j * 4);
        i32* heap = (i32*)vm->heap;
        vm->stack[i].as_i32 = heap[j];
        ++vm->index.inst;
        break;
    }
    case INST_SV8: {
        EXIT_IF(vm->index.stack_top < 2);
        i32 i = vm->index.stack_top - 1;
        i32 j = vm->index.stack_top - 2;
        EXIT_IF(CAP_STACK <= i)
        i32 l = vm->stack[i].as_i32;
        BOUNDS_CHECK_HEAP8(l);
        vm->heap[l] = (i8)(vm->stack[j].as_i32);
        vm->index.stack_top -= 2;
        ++vm->index.inst;
        break;
    }
    case INST_SV16: {
        EXIT_IF(vm->index.stack_top < 2);
        i32 i = vm->index.stack_top - 1;
        i32 j = vm->index.stack_top - 2;
        EXIT_IF(CAP_STACK <= i)
        i32 l = vm->stack[i].as_i32;
        BOUNDS_CHECK_HEAP8(l * 2);
        i16* heap = (i16*)vm->heap;
        heap[l] = (i16)(vm->stack[j].as_i32);
        vm->index.stack_top -= 2;
        ++vm->index.inst;
        break;
    }
    case INST_SV32: {
        EXIT_IF(vm->index.stack_top < 2);
        i32 i = vm->index.stack_top - 1;
        i32 j = vm->index.stack_top - 2;
        EXIT_IF(CAP_STACK <= i)
        i32 l = vm->stack[i].as_i32;
        BOUNDS_CHECK_HEAP8(l * 4);
        i32* heap = (i32*)vm->heap;
        heap[l] = vm->stack[j].as_i32;
        vm->index.stack_top -= 2;
        ++vm->index.inst;
        break;
    }
    case INST_JPZ: {
        --vm->index.stack_top;
        BOUNDS_CHECK_STACK(vm->index.stack_top);
        if (vm->stack[vm->index.stack_top].as_i32 == 0) {
            vm->index.inst = inst.op;
        } else {
            ++vm->index.inst;
        }
        break;
    }
    case INST_JUMP: {
        vm->index.inst = inst.op;
        break;
    }
    case INST_NOT: {
        i32 i = vm->index.stack_top - 1;
        BOUNDS_CHECK_STACK(i);
        vm->stack[i].as_i32 = !vm->stack[i].as_i32;
        ++vm->index.inst;
        break;
    }
    case INST_EQ: {
        EXIT_IF(vm->index.stack_top < 2);
        i32 i = vm->index.stack_top - 1;
        i32 j = vm->index.stack_top - 2;
        EXIT_IF(CAP_STACK <= i)
        vm->stack[j].as_i32 =
            vm->stack[j].as_i32 == vm->stack[i].as_i32 ? 1 : 0;
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
    case INST_NATIVE: {
        {
            EXIT_IF(COUNT_NATIVE <= inst.op);
            EXIT_IF(inst.op < 0);
        }
        NATIVES[inst.op](vm);
        ++vm->index.inst;
        break;
    }
    case COUNT_INST_TAG: {
        ERROR();
    }
    }
}

#endif
