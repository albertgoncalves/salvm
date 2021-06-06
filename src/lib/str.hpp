#ifndef __STR_H__
#define __STR_H__

#include "vm.hpp"

struct String {
    const char* chars;
    u32         len;
};

#define TO_STR(literal)      \
    ((String){               \
        literal,             \
        sizeof(literal) - 1, \
    })

#define EQ_STR(a, b) ((a.len == b.len) && (!memcmp(a.chars, b.chars, a.len)))

#define FMT_STR "%.*s"

#define ARG_STR(string) string.len, string.chars

#define PRINT_STR(stream, string) fprintf(stream, FMT_STR, ARG_STR(string))

String to_string(InstTag);

#endif
