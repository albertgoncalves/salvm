#ifndef __VM_ASM_H__
#define __VM_ASM_H__

#include "vm.h"

#define CAP_CHAR  4096
#define CAP_TOKEN 256
#define CAP_LABEL 64

typedef enum {
    TOKEN_INST,

    TOKEN_STR,
    TOKEN_I32,
    TOKEN_F32,

    TOKEN_COLON,
    TOKEN_MINUS,
} TokenTag;

typedef struct {
    const char* chars;
    u32         len;
} String;

typedef union {
    String  as_string;
    InstTag as_inst_tag;
    i32     as_i32;
    f32     as_f32;
} TokenBody;

typedef struct {
    TokenBody body;
    u32       line;
    TokenTag  tag;
} Token;

typedef struct {
    Inst   inst;
    String label;
    Bool   resolved;
} PreInst;

typedef struct {
    String string;
    i32    index_inst;
} Label;

typedef struct {
    Vm      vm;
    char    chars[CAP_CHAR];
    Token   tokens[CAP_TOKEN];
    PreInst pre_insts[CAP_INST];
    Label   labels[CAP_LABEL];
    u32     len_chars;
    u32     len_tokens;
    u32     len_pre_insts;
    u32     len_labels;
} Memory;

#define EQ_STRINGS(a, b) \
    ((a.len == b.len) && (!memcmp(a.chars, b.chars, a.len)))

#define PRINTLN_STR(stream, string) \
    fprintf(stream, "\"%.*s\"\n", (i32)string.len, string.chars)

#define IS_ALPHA(x) \
    ((('A' <= (x)) && ((x) <= 'Z')) || (('a' <= (x)) && ((x) <= 'z')))

#define IS_DIGIT(x) (('0' <= (x)) && ((x) <= '9'))

#define IS_PUNCT(x) (((x) == '_') || ((x) == '.'))

#define IS_ALPHA_OR_DIGIT_OR_PUNCT(x) \
    (IS_ALPHA(x) || IS_DIGIT(x) || IS_PUNCT(x))

#define ALLOC_MEMORY(fn, cap, field, type)            \
    static type* fn(Memory* memory) {                 \
        EXIT_IF((cap) <= memory->len_##field);        \
        return &memory->field[memory->len_##field++]; \
    }

ALLOC_MEMORY(alloc_token, CAP_TOKEN, tokens, Token)
ALLOC_MEMORY(alloc_pre_inst, CAP_INST, pre_insts, PreInst)
ALLOC_MEMORY(alloc_label, CAP_LABEL, labels, Label)

#define ERROR_TOKEN(token)            \
    {                                 \
        println_token(stderr, token); \
        ERROR();                      \
    }

static String get_inst_tag_as_string(InstTag tag) {
    switch (tag) {

    case INST_HALT: {
        return (String){.chars = "halt", .len = 4};
    }

    case INST_PUSH: {
        return (String){.chars = "push", .len = 4};
    }

    case INST_TOP: {
        return (String){.chars = "top", .len = 3};
    }

    case INST_COPY: {
        return (String){.chars = "copy", .len = 4};
    }
    case INST_PUT: {
        return (String){.chars = "put", .len = 3};
    }

    case INST_CALL: {
        return (String){.chars = "call", .len = 4};
    }
    case INST_RET: {
        return (String){.chars = "ret", .len = 3};
    }

    case INST_BASE: {
        return (String){.chars = "base", .len = 4};
    }
    case INST_FRAME: {
        return (String){.chars = "frame", .len = 5};
    }
    case INST_RESET: {
        return (String){.chars = "reset", .len = 5};
    }

    case INST_RD8: {
        return (String){.chars = "rd8", .len = 3};
    }
    case INST_RD16: {
        return (String){.chars = "rd16", .len = 4};
    }
    case INST_RD32: {
        return (String){.chars = "rd32", .len = 4};
    }

    case INST_SV8: {
        return (String){.chars = "sv8", .len = 3};
    }
    case INST_SV16: {
        return (String){.chars = "sv16", .len = 4};
    }
    case INST_SV32: {
        return (String){.chars = "sv32", .len = 4};
    }

    case INST_JPZ: {
        return (String){.chars = "jpz", .len = 3};
    }
    case INST_JUMP: {
        return (String){.chars = "jump", .len = 4};
    }

    case INST_NOT: {
        return (String){.chars = "not", .len = 3};
    }
    case INST_EQ: {
        return (String){.chars = "eq", .len = 2};
    }

    case INST_ADDI: {
        return (String){.chars = "addi", .len = 4};
    }
    case INST_SUBI: {
        return (String){.chars = "subi", .len = 4};
    }
    case INST_MULI: {
        return (String){.chars = "muli", .len = 4};
    }
    case INST_DIVI: {
        return (String){.chars = "divi", .len = 4};
    }

    case INST_ADDF: {
        return (String){.chars = "addf", .len = 4};
    }
    case INST_SUBF: {
        return (String){.chars = "subf", .len = 4};
    }
    case INST_MULF: {
        return (String){.chars = "mulf", .len = 4};
    }
    case INST_DIVF: {
        return (String){.chars = "divf", .len = 4};
    }

    case INST_LTI: {
        return (String){.chars = "lti", .len = 3};
    }
    case INST_LEI: {
        return (String){.chars = "lei", .len = 3};
    }
    case INST_GTI: {
        return (String){.chars = "gti", .len = 3};
    }
    case INST_GEI: {
        return (String){.chars = "gei", .len = 3};
    }

    case INST_LTF: {
        return (String){.chars = "ltf", .len = 3};
    }
    case INST_LEF: {
        return (String){.chars = "lef", .len = 3};
    }
    case INST_GTF: {
        return (String){.chars = "gtf", .len = 3};
    }
    case INST_GEF: {
        return (String){.chars = "gef", .len = 3};
    }

    case INST_NATIVE: {
        return (String){.chars = "native", .len = 6};
    }

    case COUNT_INST_TAG: {
        break;
    }
    }
    ERROR();
}

static void println_token(File* stream, Token token) {
    switch (token.tag) {
    case TOKEN_INST: {
        String string = get_inst_tag_as_string(token.body.as_inst_tag);
        PRINTLN_STR(stream, string);
        break;
    }
    case TOKEN_STR: {
        PRINTLN_STR(stream, token.body.as_string);
        break;
    }
    case TOKEN_I32: {
        fprintf(stream, "%d\n", token.body.as_i32);
        break;
    }
    case TOKEN_F32: {
        fprintf(stream, "%.2f\n", (f64)token.body.as_f32);
        break;
    }
    case TOKEN_COLON: {
        fprintf(stream, ":\n");
        break;
    }
    case TOKEN_MINUS: {
        fprintf(stream, "-\n");
        break;
    }
    }
}

static i32 parse_digits_i32(const char* chars, u32* i) {
    i32 a = 0;
    while (IS_DIGIT(chars[*i])) {
        i32 b = (a * 10) + ((i32)(chars[(*i)++] - '0'));
        EXIT_IF(b < a);
        a = b;
    }
    return a;
}

static f32 parse_decimal_f32(const char* chars, u32* i) {
    f32 a = 0.0f;
    f32 b = 1.0f;
    while (IS_DIGIT(chars[*i])) {
        a = (a * 10.0f) + ((f32)(chars[(*i)++] - '0'));
        b *= 10.0f;
    }
    return a / b;
}

static void set_tokens(Memory* memory) {
    u32 line = 1;
    for (u32 i = 0; i < memory->len_chars;) {
        switch (memory->chars[i]) {
        case ' ':
        case '\t': {
            ++i;
            break;
        };
        case '\n': {
            ++line;
            ++i;
            break;
        }
        case ';': {
            for (; i < memory->len_chars; ++i) {
                if (memory->chars[i] == '\n') {
                    break;
                }
            }
            break;
        }
        case ':': {
            Token* token = alloc_token(memory);
            token->tag = TOKEN_COLON;
            token->line = line;
            ++i;
            break;
        }
        case '-': {
            Token* token = alloc_token(memory);
            token->tag = TOKEN_MINUS;
            token->line = line;
            ++i;
            break;
        }
        default: {
            Token* token = alloc_token(memory);
            token->line = line;
            if (IS_DIGIT(memory->chars[i])) {
                i32 x = parse_digits_i32(memory->chars, &i);
                if (memory->chars[i] == '.') {
                    ++i;
                    token->body.as_f32 =
                        (f32)x + parse_decimal_f32(memory->chars, &i);
                    token->tag = TOKEN_F32;
                } else {
                    token->body.as_i32 = x;
                    token->tag = TOKEN_I32;
                }
                continue;
            }
            u32 j = i;
            for (; j < memory->len_chars; ++j) {
                if (!(IS_ALPHA_OR_DIGIT_OR_PUNCT(memory->chars[j]))) {
                    break;
                }
            }
            EXIT_IF(i == j);
            String token_string = {
                .chars = &memory->chars[i],
                .len = j - i,
            };
            for (InstTag t = 0; t < COUNT_INST_TAG; ++t) {
                String inst_string = get_inst_tag_as_string(t);
                if (EQ_STRINGS(token_string, inst_string)) {
                    token->body.as_inst_tag = t;
                    token->tag = TOKEN_INST;
                    goto loop_end;
                }
            }
            token->body.as_string = token_string;
            token->tag = TOKEN_STR;
        loop_end:
            i = j;
        }
        }
    }
}

static void set_insts(Memory* memory) {
    i32 index_inst = 0;
    for (u32 i = 0; i < memory->len_tokens; ++i) {
        Token token = memory->tokens[i];
        switch (token.tag) {
        case TOKEN_INST: {
            PreInst* pre_inst = alloc_pre_inst(memory);
            pre_inst->inst.tag = token.body.as_inst_tag;
            ++index_inst;
            switch (token.body.as_inst_tag) {
            case INST_HALT:
            case INST_RET:
            case INST_BASE:
            case INST_RESET:
            case INST_RD8:
            case INST_RD16:
            case INST_RD32:
            case INST_SV8:
            case INST_SV16:
            case INST_SV32:
            case INST_NOT:
            case INST_EQ:

            case INST_ADDI:
            case INST_SUBI:
            case INST_MULI:
            case INST_DIVI:

            case INST_ADDF:
            case INST_SUBF:
            case INST_MULF:
            case INST_DIVF:

            case INST_LTI:
            case INST_LEI:
            case INST_GTI:
            case INST_GEI:

            case INST_LTF:
            case INST_LEF:
            case INST_GTF:
            case INST_GEF: {
                pre_inst->resolved = TRUE;
                break;
            }
            case INST_PUSH: {
                token = memory->tokens[++i];
                switch (token.tag) {
                case TOKEN_I32:
                case TOKEN_F32: {
                    pre_inst->inst.op = token.body.as_i32;
                    pre_inst->resolved = TRUE;
                    break;
                }
                case TOKEN_MINUS: {
                    token = memory->tokens[++i];
                    switch (token.tag) {
                    case TOKEN_I32: {
                        pre_inst->inst.op = -token.body.as_i32;
                        pre_inst->resolved = TRUE;
                        break;
                    }
                    case TOKEN_F32: {
                        token.body.as_f32 = -token.body.as_f32;
                        pre_inst->inst.op = token.body.as_i32;
                        pre_inst->resolved = TRUE;
                        break;
                    }
                    case TOKEN_INST:
                    case TOKEN_STR:
                    case TOKEN_COLON:
                    case TOKEN_MINUS: {
                        ERROR_TOKEN(token);
                    }
                    }
                    break;
                }
                case TOKEN_INST:
                case TOKEN_STR:
                case TOKEN_COLON: {
                    ERROR_TOKEN(token);
                }
                }
                break;
            }
            case INST_TOP:
            case INST_COPY:
            case INST_PUT:
            case INST_FRAME: {
                token = memory->tokens[++i];
                switch (token.tag) {
                case TOKEN_I32: {
                    pre_inst->inst.op = token.body.as_i32;
                    pre_inst->resolved = TRUE;
                    break;
                }
                case TOKEN_MINUS: {
                    token = memory->tokens[++i];
                    switch (token.tag) {
                    case TOKEN_I32: {
                        pre_inst->inst.op = -token.body.as_i32;
                        pre_inst->resolved = TRUE;
                        break;
                    }
                    case TOKEN_F32:
                    case TOKEN_INST:
                    case TOKEN_STR:
                    case TOKEN_COLON:
                    case TOKEN_MINUS: {
                        ERROR_TOKEN(token);
                    }
                    }
                    break;
                }
                case TOKEN_F32:
                case TOKEN_INST:
                case TOKEN_STR:
                case TOKEN_COLON: {
                    ERROR_TOKEN(token);
                }
                }
                break;
            }
            case INST_NATIVE: {
                token = memory->tokens[++i];
                switch (token.tag) {
                case TOKEN_I32: {
                    pre_inst->inst.op = token.body.as_i32;
                    pre_inst->resolved = TRUE;
                    break;
                }
                case TOKEN_F32:
                case TOKEN_INST:
                case TOKEN_STR:
                case TOKEN_COLON:
                case TOKEN_MINUS: {
                    ERROR_TOKEN(token);
                }
                }
                break;
            }
            case INST_CALL:
            case INST_JPZ:
            case INST_JUMP: {
                token = memory->tokens[++i];
                switch (token.tag) {
                case TOKEN_I32: {
                    pre_inst->inst.op = token.body.as_i32;
                    pre_inst->resolved = TRUE;
                    break;
                }
                case TOKEN_STR: {
                    pre_inst->label = token.body.as_string;
                    pre_inst->resolved = FALSE;
                    break;
                }
                case TOKEN_F32:
                case TOKEN_INST:
                case TOKEN_COLON:
                case TOKEN_MINUS: {
                    ERROR_TOKEN(token);
                }
                }
                break;
            }
            case COUNT_INST_TAG: {
                ERROR_TOKEN(token);
            }
            }
            break;
        }
        case TOKEN_STR: {
            String string = token.body.as_string;
            EXIT_IF(memory->tokens[++i].tag != TOKEN_COLON);
            Label* label = alloc_label(memory);
            label->string = string;
            label->index_inst = index_inst;
            break;
        }
        case TOKEN_I32:
        case TOKEN_F32:
        case TOKEN_COLON:
        case TOKEN_MINUS: {
            ERROR_TOKEN(token);
        }
        }
    }
    for (u32 i = 0; i < memory->len_pre_insts; ++i) {
        PreInst* pre_inst = &memory->pre_insts[i];
        if (!pre_inst->resolved) {
            for (u32 j = 0; j < memory->len_labels; ++j) {
                Label label = memory->labels[j];
                if (EQ_STRINGS(label.string, pre_inst->label)) {
                    pre_inst->inst.op = label.index_inst;
                    pre_inst->resolved = TRUE;
                    break;
                }
            }
        }
        EXIT_IF(!pre_inst->resolved);
        memory->vm.insts[i] = pre_inst->inst;
    }
}

#endif