#ifndef __VM_ASM_H__
#define __VM_ASM_H__

#include "vm_inst_string.hpp"

#define CAP_CHARS  (2 << 11)
#define CAP_TOKENS (2 << 7)
#define CAP_LABELS (2 << 5)

enum TokenTag {
    TOKEN_INST,

    TOKEN_STR,
    TOKEN_I32,
    TOKEN_F32,

    TOKEN_COLON,
    TOKEN_MINUS,
};

union TokenBody {
    String  as_string;
    InstTag as_inst_tag;
    i32     as_i32;
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
    Bool   resolved;
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

ALLOC_MEMORY(alloc_token, CAP_TOKENS, tokens, Token)
ALLOC_MEMORY(alloc_pre_inst, CAP_INSTS, pre_insts, PreInst)
ALLOC_MEMORY(alloc_label, CAP_LABELS, labels, Label)

#define ERROR_TOKEN(token)            \
    {                                 \
        println_token(stderr, token); \
        ERROR();                      \
    }

static void println_token(File* stream, Token token) {
    fprintf(stream, "%u:", token.line);
    switch (token.tag) {
    case TOKEN_INST: {
        const String string = get_inst_tag_as_string(token.body.as_inst_tag);
        PRINTLN_STR(stream, string);
        break;
    }
    case TOKEN_STR: {
        PRINTLN_STR(stream, token.body.as_string);
        break;
    }
    case TOKEN_I32: {
        fprintf(stream, "`%d`\n", token.body.as_i32);
        break;
    }
    case TOKEN_F32: {
        fprintf(stream, "`%.2f`\n", static_cast<f64>(token.body.as_f32));
        break;
    }
    case TOKEN_COLON: {
        fprintf(stream, "`:`\n");
        break;
    }
    case TOKEN_MINUS: {
        fprintf(stream, "`-`\n");
        break;
    }
    default: {
        ERROR();
    }
    }
}

template <typename T> static T parse_digits(const char* chars, u32* i) {
    T a = 0;
    while (IS_DIGIT(chars[*i])) {
        const T b = (a * 10) + static_cast<T>(chars[(*i)++] - '0');
        EXIT_IF(b < a);
        a = b;
    }
    return a;
}

static f32 parse_decimal_f32(const char* chars, u32* i) {
    f32 a = 0.0f;
    f32 b = 1.0f;
    while (IS_DIGIT(chars[*i])) {
        a = (a * 10.0f) + static_cast<f32>(chars[(*i)++] - '0');
        b *= 10.0f;
    }
    return a / b;
}

#define SET_HEAP_BYTES(memory, line, i, j, block) \
    {                                             \
        ++i;                                      \
        EXIT_IF(memory->len_chars <= i);          \
        EXIT_IF(memory->chars[i] != '[');         \
        j = i + 1;                                \
        EXIT_IF(memory->len_chars <= j);          \
        for (; memory->chars[j] != ']';) {        \
            switch (memory->chars[j]) {           \
            case ' ':                             \
            case '\t': {                          \
                ++j;                              \
                break;                            \
            }                                     \
            case '\n': {                          \
                ++line;                           \
                ++j;                              \
                break;                            \
            }                                     \
            default: {                            \
                block;                            \
            }                                     \
            }                                     \
            EXIT_IF(memory->len_chars <= j);      \
        }                                         \
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
        case '+': {
            ++i;
            EXIT_IF(memory->len_chars <= i);
            u32 j;
            switch (memory->chars[i]) {
            case '"': {
                j = i + 1;
                EXIT_IF(memory->len_chars <= j);
                for (; memory->chars[j] != '"';) {
                    Bool escaped = FALSE;
                    switch (memory->chars[j]) {
                    case '\n': {
                        ++line;
                        break;
                    }
                    case '\\': {
                        escaped = TRUE;
                        ++j;
                        EXIT_IF(memory->len_chars <= j);
                        break;
                    }
                    }
                    EXIT_IF(CAP_HEAP8 <= memory->len_bytes);
                    if ((memory->chars[j] == 'n') && escaped) {
                        memory->vm.heap[memory->len_bytes++] = '\n';
                    } else {
                        memory->vm.heap[memory->len_bytes++] =
                            memory->chars[j];
                    }
                    ++j;
                    EXIT_IF(memory->len_chars <= j);
                }
                break;
            }
            case '1': {
                SET_HEAP_BYTES(memory, line, i, j, {
                    EXIT_IF(!IS_DIGIT(memory->chars[j]));
                    EXIT_IF(CAP_HEAP8 <= memory->len_bytes);
                    memory->vm.heap[memory->len_bytes++] =
                        static_cast<i8>(parse_digits<u8>(memory->chars, &j));
                })
                break;
            }
            case '2': {
                SET_HEAP_BYTES(memory, line, i, j, {
                    EXIT_IF(!IS_DIGIT(memory->chars[j]));
                    const u32 n = memory->len_bytes + 2;
                    EXIT_IF(CAP_HEAP8 < n);
                    const u16 x = parse_digits<u16>(memory->chars, &j);
                    memcpy(&memory->vm.heap[memory->len_bytes],
                           &x,
                           sizeof(u16));
                    memory->len_bytes = n;
                })
                break;
            }
            case '4': {
                SET_HEAP_BYTES(memory, line, i, j, {
                    EXIT_IF(!IS_DIGIT(memory->chars[j]));
                    const u32 n = memory->len_bytes + 4;
                    EXIT_IF(CAP_HEAP8 < n);
                    const u32 x = parse_digits<u32>(memory->chars, &j);
                    memcpy(&memory->vm.heap[memory->len_bytes],
                           &x,
                           sizeof(u32));
                    memory->len_bytes = n;
                })
                break;
            }
            default: {
                ERROR();
            }
            }
            i = j + 1;
            break;
        }
        default: {
            Token* token = alloc_token(memory);
            token->line = line;
            if (IS_DIGIT(memory->chars[i])) {
                const i32 x = parse_digits<i32>(memory->chars, &i);
                if (memory->chars[i] == '.') {
                    ++i;
                    token->body.as_f32 = static_cast<f32>(x) +
                                         parse_decimal_f32(memory->chars, &i);
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
            const String token_string = {&memory->chars[i], j - i};
            for (u32 k = 0; k < COUNT_INST_TAG; ++k) {
                const InstTag tag = static_cast<InstTag>(k);
                const String  inst_string = get_inst_tag_as_string(tag);
                if (EQ_STRINGS(token_string, inst_string)) {
                    token->body.as_inst_tag = tag;
                    token->tag = TOKEN_INST;
                    goto end;
                }
            }
            token->body.as_string = token_string;
            token->tag = TOKEN_STR;
        end:
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
            case INST_SCL:
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
                    case TOKEN_MINUS:
                    default: {
                        ERROR_TOKEN(token);
                    }
                    }
                    break;
                }
                case TOKEN_STR: {
                    pre_inst->label = token.body.as_string;
                    pre_inst->resolved = FALSE;
                    break;
                }
                case TOKEN_INST:
                case TOKEN_COLON:
                default: {
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
                    case TOKEN_MINUS:
                    default: {
                        ERROR_TOKEN(token);
                    }
                    }
                    break;
                }
                case TOKEN_F32:
                case TOKEN_INST:
                case TOKEN_STR:
                case TOKEN_COLON:
                default: {
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
                case TOKEN_MINUS:
                default: {
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
                case TOKEN_MINUS:
                default: {
                    ERROR_TOKEN(token);
                }
                }
                break;
            }
            case COUNT_INST_TAG:
            default: {
                ERROR_TOKEN(token);
            }
            }
            break;
        }
        case TOKEN_STR: {
            const String string = token.body.as_string;
            if (memory->tokens[++i].tag != TOKEN_COLON) {
                ERROR_TOKEN(token);
            }
            Label* label = alloc_label(memory);
            label->string = string;
            label->index_inst = index_inst;
            break;
        }
        case TOKEN_I32:
        case TOKEN_F32:
        case TOKEN_COLON:
        case TOKEN_MINUS:
        default: {
            ERROR_TOKEN(token);
        }
        }
    }
    for (u32 i = 0; i < memory->len_pre_insts; ++i) {
        PreInst* pre_inst = &memory->pre_insts[i];
        if (!pre_inst->resolved) {
            for (u32 j = 0; j < memory->len_labels; ++j) {
                const Label label = memory->labels[j];
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
