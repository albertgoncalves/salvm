#include "vm_asm.h"
#include "vm_io.h"

i32 main(i32 n, const char** args) {
    EXIT_IF(n < 2);
    printf("sizeof(Inst)    : %zu\n"
           "sizeof(Vm)      : %zu\n"
           "sizeof(String)  : %zu\n"
           "sizeof(Token)   : %zu\n"
           "sizeof(Memory)  : %zu\n"
           "sizeof(PreInst) : %zu\n"
           "sizeof(Label)   : %zu\n",
           sizeof(Inst),
           sizeof(Vm),
           sizeof(String),
           sizeof(Token),
           sizeof(Memory),
           sizeof(PreInst),
           sizeof(Label));
    {
        Memory* memory = calloc(1, sizeof(Memory));
        EXIT_IF(!memory);
        {
            set_chars_from_file(memory, args[1]);
            set_tokens(memory);
            set_insts(memory);
            reset(&memory->vm);
            run(&memory->vm);
        }
        free(memory);
    }
    return EXIT_SUCCESS;
}
