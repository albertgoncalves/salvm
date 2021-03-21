#include "vm_asm.h"
#include "vm_io.h"

i32 main(i32 n, const char** args) {
    EXIT_IF(n < 2);
    {
        Memory* memory = calloc(1, sizeof(Memory));
        EXIT_IF(!memory);
        set_chars_from_file(memory, args[1]);
        set_tokens(memory);
        set_insts(memory);
        run(&memory->vm);
        free(memory);
    }
    return EXIT_SUCCESS;
}
