#ifndef __VM_INST_STRING_H__
#define __VM_INST_STRING_H__

#include "vm.h"

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

#define PRINTLN_STR(stream, string) \
    fprintf(stream, "`" FMT_STR "`\n", string.len, string.chars)

static String get_inst_tag_as_string(InstTag tag) {
    switch (tag) {
    case INST_HALT: {
        return TO_STRING("halt");
    }

    case INST_PUSH: {
        return TO_STRING("push");
    }

    case INST_TOP: {
        return TO_STRING("top");
    }

    case INST_COPY: {
        return TO_STRING("copy");
    }
    case INST_PUT: {
        return TO_STRING("put");
    }

    case INST_CALL: {
        return TO_STRING("call");
    }
    case INST_SCL: {
        return TO_STRING("scl");
    }
    case INST_RET: {
        return TO_STRING("ret");
    }

    case INST_BASE: {
        return TO_STRING("base");
    }
    case INST_FRAME: {
        return TO_STRING("frame");
    }
    case INST_RESET: {
        return TO_STRING("reset");
    }

    case INST_RD8: {
        return TO_STRING("rd8");
    }
    case INST_RD16: {
        return TO_STRING("rd16");
    }
    case INST_RD32: {
        return TO_STRING("rd32");
    }

    case INST_SV8: {
        return TO_STRING("sv8");
    }
    case INST_SV16: {
        return TO_STRING("sv16");
    }
    case INST_SV32: {
        return TO_STRING("sv32");
    }

    case INST_JPZ: {
        return TO_STRING("jpz");
    }
    case INST_JUMP: {
        return TO_STRING("jump");
    }

    case INST_NOT: {
        return TO_STRING("not");
    }
    case INST_EQ: {
        return TO_STRING("eq");
    }

    case INST_ADDI: {
        return TO_STRING("addi");
    }
    case INST_SUBI: {
        return TO_STRING("subi");
    }
    case INST_MULI: {
        return TO_STRING("muli");
    }
    case INST_DIVI: {
        return TO_STRING("divi");
    }

    case INST_ADDF: {
        return TO_STRING("addf");
    }
    case INST_SUBF: {
        return TO_STRING("subf");
    }
    case INST_MULF: {
        return TO_STRING("mulf");
    }
    case INST_DIVF: {
        return TO_STRING("divf");
    }

    case INST_LTI: {
        return TO_STRING("lti");
    }
    case INST_LEI: {
        return TO_STRING("lei");
    }
    case INST_GTI: {
        return TO_STRING("gti");
    }
    case INST_GEI: {
        return TO_STRING("gei");
    }

    case INST_LTF: {
        return TO_STRING("ltf");
    }
    case INST_LEF: {
        return TO_STRING("lef");
    }
    case INST_GTF: {
        return TO_STRING("gtf");
    }
    case INST_GEF: {
        return TO_STRING("gef");
    }

    case INST_NATIVE: {
        return TO_STRING("native");
    }

    case COUNT_INST_TAG:
    default: {
        ERROR();
    }
    }
}

#endif
