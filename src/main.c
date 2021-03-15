#include "vm.h"

i32 main(void) {
    printf("sizeof(Inst)   : %zu\n"
           "sizeof(Header) : %zu\n"
           "sizeof(Vm)     : %zu\n",
           sizeof(Inst),
           sizeof(Header),
           sizeof(Vm));
    return EXIT_SUCCESS;
}
