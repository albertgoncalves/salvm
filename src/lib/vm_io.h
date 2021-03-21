#ifndef __VM_IO_H__
#define __VM_IO_H__

#include "vm.h"

static void run(Vm* vm) {
    vm->index.inst = 0;
    vm->index.stack_top = 0;
    vm->index.stack_base = 0;
    vm->alive = TRUE;
    while (vm->alive) {
#ifdef DEBUG_PRINT_VM
        {
            printf("\n"
                   "    | .index.inst       : %d\n"
                   "    |       .stack_top  : %d\n"
                   "    |       .stack_base : %d\n"
                   "    | .stack            : [ ",
                   vm->index.inst,
                   vm->index.stack_top,
                   vm->index.stack_base);
            for (i32 i = vm->index.stack_base; i < vm->index.stack_top; ++i) {
                printf("%d ", vm->stack[i].as_i32);
            }
            printf("]\n\n");
        }
#endif
        do_inst(vm);
    }
}

#endif
