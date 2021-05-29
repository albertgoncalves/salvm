#ifndef __VM_IO_H__
#define __VM_IO_H__

#include "vm_inst.hpp"

#ifdef DEBUG_PRINT_VM
    #include "vm_inst_string.hpp"
#endif

static void run(Vm* vm) {
    vm->index = {};
    vm->alive = TRUE;
    while (vm->alive) {
#ifdef DEBUG_PRINT_VM
        {
            const String inst =
                get_inst_tag_as_string(vm->insts[vm->index.inst].tag);
            printf("\n\n"
                   "    | .index.inst       : %d\n"
                   "    |       .stack_top  : %d\n"
                   "    |       .stack_base : %d\n"
                   "    | .inst.tag         : " FMT_STR "\n"
                   "    |      .op          : %d\n"
                   "    | .stack            : [ ",
                   vm->index.inst,
                   vm->index.stack_top,
                   vm->index.stack_base,
                   inst.len,
                   inst.chars,
                   vm->insts[vm->index.inst].op);
            for (i32 i = vm->index.stack_base; i < vm->index.stack_top; ++i) {
                printf("%d ", vm->stack[i].as_i32);
            }
            printf("]\n");
        }
#endif
        do_inst(vm);
    }
}

#endif
