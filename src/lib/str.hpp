#ifndef __STR_H__
#define __STR_H__

#include "vm.hpp"

struct String {
    const char* chars;
    u32         len;
};

#define TO_STRING(literal)   \
    ((String){               \
        literal,             \
        sizeof(literal) - 1, \
    })

#define EQ_STRINGS(a, b) \
    ((a.len == b.len) && (!memcmp(a.chars, b.chars, a.len)))

#define FMT_STR "%.*s"

#define PRINT_STR(stream, string) \
    fprintf(stream, FMT_STR, string.len, string.chars)

String get_inst_tag_as_string(InstTag);

#endif
