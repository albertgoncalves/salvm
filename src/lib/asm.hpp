#ifndef __ASM_H__
#define __ASM_H__

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-W#warnings"

#include <string.h>

#pragma GCC diagnostic pop

#include "str.hpp"

#define CAP_CHARS  (2 << 11)
#define CAP_TOKENS (2 << 7)
#define CAP_LABELS (2 << 5)

enum TokenTag {
    TOKEN_INST,

    TOKEN_STR,
    TOKEN_U32,
    TOKEN_F32,

    TOKEN_COLON,
    TOKEN_MINUS,
};

union TokenBody {
    String  as_string;
    InstTag as_inst_tag;
    u32     as_u32;
    f32     as_f32;
};

struct Token {
    TokenBody body;
    u32       line;
    TokenTag  tag;
};

struct PreInst {
    Inst   inst;
    String label;
    bool   resolved;
};

struct Label {
    String string;
    i32    index_inst;
};

struct Memory {
    Vm      vm;
    char    chars[CAP_CHARS];
    Token   tokens[CAP_TOKENS];
    i8      bytes[CAP_HEAP8];
    PreInst pre_insts[CAP_INSTS];
    Label   labels[CAP_LABELS];
    u32     len_chars;
    u32     len_tokens;
    u32     len_bytes;
    u32     len_pre_insts;
    u32     len_labels;
};

void set_tokens(Memory*);
void set_insts(Memory*);

#endif
