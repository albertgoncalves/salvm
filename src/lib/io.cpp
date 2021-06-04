#include "io.hpp"

void run(Vm* vm) {
    vm->index = {};
    vm->alive = true;
    while (vm->alive) {
#ifdef DEBUG
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
                   ARG_STR(inst),
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
