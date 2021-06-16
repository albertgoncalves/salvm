#include "asm.hpp"

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

#define ERROR_PRINT(x)    \
    {                     \
        print(stderr, x); \
        ERROR();          \
    }

String to_string(SizeTag tag) {
    switch (tag) {
    case SIZE_I8: {
        return TO_STR("i8");
    }
    case SIZE_I16: {
        return TO_STR("i16");
    }
    case SIZE_I32: {
        return TO_STR("i32");
    }
    case SIZE_F32: {
        return TO_STR("f32");
    }
    case COUNT_SIZE_TAG:
    default: {
        ERROR();
    }
    }
}

static void print(File* stream, Token token) {
    fprintf(stream, "%u:", token.line);
    switch (token.tag) {
    case TOKEN_INST: {
        const String string = to_string(token.body.as_inst_tag);
        fprintf(stream, "`");
        PRINT_STR(stream, string);
        fprintf(stream, "`\n");
        break;
    }
    case TOKEN_STR: {
        fprintf(stream, "`");
        PRINT_STR(stream, token.body.as_string);
        fprintf(stream, "`\n");
        break;
    }
    case TOKEN_U32: {
        fprintf(stream, "`%u`\n", token.body.as_u32);
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
    case TOKEN_PLUS: {
        fprintf(stream, "`+`\n");
        break;
    }
    case TOKEN_SIZE: {
        const String string = to_string(token.body.as_size_tag);
        fprintf(stream, "`");
        PRINT_STR(stream, string);
        fprintf(stream, "`\n");
        break;
    }
    case TOKEN_QUOTE: {
        fprintf(stream, "`\"`\n");
        break;
    }
    case TOKEN_LBRACKET: {
        fprintf(stream, "`[`\n");
        break;
    }
    case TOKEN_RBRACKET: {
        fprintf(stream, "`]`\n");
        break;
    }
    default: {
        ERROR();
    }
    }
}

static u32 to_digits(const char* chars, u32* i) {
    u32 a = 0;
    while (IS_DIGIT(chars[*i])) {
        const u32 b = (a * 10) + static_cast<u32>(chars[(*i)++] - '0');
        EXIT_IF(b < a);
        a = b;
    }
    return a;
}

static f32 to_decimal(const char* chars, u32* i) {
    f32 a = 0.0f;
    f32 b = 1.0f;
    while (IS_DIGIT(chars[*i])) {
        a = (a * 10.0f) + static_cast<f32>(chars[(*i)++] - '0');
        b *= 10.0f;
    }
    return a / b;
}

template <TokenTag X>
static void set_tag(Memory* memory, u32 line, u32* i) {
    Token* token = alloc_token(memory);
    token->tag = X;
    token->line = line;
    ++(*i);
}

void set_tokens(Memory* memory) {
    memory->len_tokens = 0;
    memory->len_bytes = 0;
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
            set_tag<TOKEN_COLON>(memory, line, &i);
            break;
        }
        case '-': {
            set_tag<TOKEN_MINUS>(memory, line, &i);
            break;
        }
        case '+': {
            set_tag<TOKEN_PLUS>(memory, line, &i);
            break;
        }
        case '[': {
            set_tag<TOKEN_LBRACKET>(memory, line, &i);
            break;
        }
        case ']': {
            set_tag<TOKEN_RBRACKET>(memory, line, &i);
            break;
        }
        case '"': {
            set_tag<TOKEN_QUOTE>(memory, line, &i);
            {
                EXIT_IF(memory->len_chars <= i);
                u32  j = i;
                bool escaped = false;
                for (; escaped || (memory->chars[j] != '"');) {
                    escaped = false;
                    if (memory->chars[j] == '\n') {
                        ++line;
                    }
                    if (memory->chars[j] == '\\') {
                        escaped = true;
                    }
                    ++j;
                    EXIT_IF(memory->len_chars <= j);
                }
                Token* token = alloc_token(memory);
                token->body.as_string = {&memory->chars[i], j - i};
                token->tag = TOKEN_STR;
                i = j;
            }
            set_tag<TOKEN_QUOTE>(memory, line, &i);
            break;
        }
        default: {
            Token* token = alloc_token(memory);
            token->line = line;
            if (IS_DIGIT(memory->chars[i])) {
                const u32 x = to_digits(memory->chars, &i);
                if ((i < memory->len_chars) && (memory->chars[i] == '.')) {
                    ++i;
                    EXIT_IF(memory->len_chars <= i);
                    token->body.as_f32 =
                        static_cast<f32>(x) + to_decimal(memory->chars, &i);
                    token->tag = TOKEN_F32;
                } else {
                    token->body.as_u32 = x;
                    token->tag = TOKEN_U32;
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
                const String  inst_string = to_string(tag);
                if (EQ_STR(token_string, inst_string)) {
                    token->body.as_inst_tag = tag;
                    token->tag = TOKEN_INST;
                    goto end;
                }
            }
            for (u32 k = 0; k < COUNT_SIZE_TAG; ++k) {
                const SizeTag tag = static_cast<SizeTag>(k);
                const String  size_string = to_string(tag);
                if (EQ_STR(token_string, size_string)) {
                    token->body.as_size_tag = tag;
                    token->tag = TOKEN_SIZE;
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

static void set_pre_inst_i32(Token token, PreInst* pre_inst) {
    EXIT_IF(static_cast<u32>(I32_MAX) < token.body.as_u32);
    pre_inst->inst.op = static_cast<i32>(token.body.as_u32);
    pre_inst->resolved = true;
}

static void set_pre_inst_negative_i32(Token token, PreInst* pre_inst) {
    EXIT_IF((static_cast<u32>(I32_MAX) + 1) < token.body.as_u32);
    pre_inst->inst.op = -static_cast<i32>(token.body.as_u32);
    pre_inst->resolved = true;
}

static void set_pre_inst_f32(Token token, PreInst* pre_inst) {
    pre_inst->inst.op = static_cast<i32>(token.body.as_u32);
    pre_inst->resolved = true;
}

static void set_pre_inst_string(Token token, PreInst* pre_inst) {
    pre_inst->label = token.body.as_string;
    pre_inst->resolved = false;
}

static void set_heap_char(Memory* memory, Token token) {
    const String string = token.body.as_string;
    for (u32 j = 0; j < string.len; ++j) {
        bool escaped = false;
        if (string.chars[j] == '\\') {
            escaped = true;
            ++j;
            EXIT_IF(string.len <= j);
        }
        EXIT_IF(CAP_HEAP8 <= memory->len_bytes);
        if (escaped && (string.chars[j] == 'n')) {
            memory->vm.heap[memory->len_bytes++] = '\n';
        } else {
            memory->vm.heap[memory->len_bytes++] = string.chars[j];
        }
    }
}

#define SET_NEXT(memory, token, i)       \
    {                                    \
        ++(i);                           \
        if (memory->len_tokens <= (i)) { \
            ERROR_PRINT(token);          \
        }                                \
        token = memory->tokens[(i)];     \
    }

template <typename T, u32 N>
static void set_heap(Token token, Memory* memory, u32* i) {
    if (token.tag == TOKEN_U32) {
        const u32 n = memory->len_bytes + sizeof(T);
        EXIT_IF(CAP_HEAP8 < n);
        if (N < token.body.as_u32) {
            ERROR_PRINT(token);
        }
        const i32 x = static_cast<T>(token.body.as_u32);
        memcpy(&memory->vm.heap[memory->len_bytes], &x, sizeof(T));
        memory->len_bytes = n;
        return;
    } else if (token.tag == TOKEN_MINUS) {
        SET_NEXT(memory, token, *i);
        if (token.tag == TOKEN_U32) {
            const u32 n = memory->len_bytes + sizeof(T);
            EXIT_IF(CAP_HEAP8 < n);
            if ((N + 1) < token.body.as_u32) {
                ERROR_PRINT(token);
            }
            const i32 x = -static_cast<T>(token.body.as_u32);
            memcpy(&memory->vm.heap[memory->len_bytes], &x, sizeof(T));
            memory->len_bytes = n;
            return;
        }
    }
    ERROR_PRINT(token);
}

static void set_heap_f32(Token token, Memory* memory, u32* i) {
    if (token.tag == TOKEN_F32) {
        const u32 n = memory->len_bytes + sizeof(f32);
        EXIT_IF(CAP_HEAP8 < n);
        const i32 x = static_cast<i32>(token.body.as_u32);
        memcpy(&memory->vm.heap[memory->len_bytes], &x, sizeof(f32));
        memory->len_bytes = n;
        return;
    } else if (token.tag == TOKEN_MINUS) {
        SET_NEXT(memory, token, *i);
        if (token.tag == TOKEN_F32) {
            const u32 n = memory->len_bytes + sizeof(f32);
            EXIT_IF(CAP_HEAP8 < n);
            token.body.as_f32 = -token.body.as_f32;
            const i32 x = static_cast<i32>(token.body.as_u32);
            memcpy(&memory->vm.heap[memory->len_bytes], &x, sizeof(f32));
            memory->len_bytes = n;
            return;
        }
    }
    ERROR_PRINT(token);
}

void set_insts(Memory* memory) {
    memory->len_pre_insts = 0;
    memory->len_labels = 0;
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

            case INST_RDF32:
            case INST_SVF32:

            case INST_NOT:
            case INST_EQ:

            case INST_SIGI:
            case INST_SIGF:

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
                pre_inst->resolved = true;
                break;
            }
            case INST_PUSH: {
                SET_NEXT(memory, token, i);
                switch (token.tag) {
                case TOKEN_U32: {
                    set_pre_inst_i32(token, pre_inst);
                    break;
                }
                case TOKEN_F32: {
                    set_pre_inst_f32(token, pre_inst);
                    break;
                }
                case TOKEN_MINUS: {
                    SET_NEXT(memory, token, i);
                    switch (token.tag) {
                    case TOKEN_U32: {
                        set_pre_inst_negative_i32(token, pre_inst);
                        break;
                    }
                    case TOKEN_F32: {
                        token.body.as_f32 = -token.body.as_f32;
                        set_pre_inst_f32(token, pre_inst);
                        break;
                    }
                    case TOKEN_INST:
                    case TOKEN_STR:
                    case TOKEN_COLON:
                    case TOKEN_MINUS:
                    case TOKEN_PLUS:
                    case TOKEN_SIZE:
                    case TOKEN_QUOTE:
                    case TOKEN_LBRACKET:
                    case TOKEN_RBRACKET:
                    default: {
                        ERROR_PRINT(token);
                    }
                    }
                    break;
                }
                case TOKEN_STR: {
                    set_pre_inst_string(token, pre_inst);
                    break;
                }
                case TOKEN_INST:
                case TOKEN_COLON:
                case TOKEN_PLUS:
                case TOKEN_SIZE:
                case TOKEN_QUOTE:
                case TOKEN_LBRACKET:
                case TOKEN_RBRACKET:
                default: {
                    ERROR_PRINT(token);
                }
                }
                break;
            }
            case INST_TOP:
            case INST_COPY:
            case INST_PUT:
            case INST_FRAME: {
                SET_NEXT(memory, token, i);
                switch (token.tag) {
                case TOKEN_U32: {
                    set_pre_inst_i32(token, pre_inst);
                    break;
                }
                case TOKEN_MINUS: {
                    SET_NEXT(memory, token, i);
                    switch (token.tag) {
                    case TOKEN_U32: {
                        set_pre_inst_negative_i32(token, pre_inst);
                        break;
                    }
                    case TOKEN_INST:
                    case TOKEN_STR:
                    case TOKEN_F32:
                    case TOKEN_COLON:
                    case TOKEN_MINUS:
                    case TOKEN_PLUS:
                    case TOKEN_SIZE:
                    case TOKEN_QUOTE:
                    case TOKEN_LBRACKET:
                    case TOKEN_RBRACKET:
                    default: {
                        ERROR_PRINT(token);
                    }
                    }
                    break;
                }
                case TOKEN_INST:
                case TOKEN_STR:
                case TOKEN_F32:
                case TOKEN_COLON:
                case TOKEN_PLUS:
                case TOKEN_SIZE:
                case TOKEN_QUOTE:
                case TOKEN_LBRACKET:
                case TOKEN_RBRACKET:
                default: {
                    ERROR_PRINT(token);
                }
                }
                break;
            }
            case INST_NATIVE: {
                SET_NEXT(memory, token, i);
                switch (token.tag) {
                case TOKEN_U32: {
                    set_pre_inst_i32(token, pre_inst);
                    break;
                }
                case TOKEN_INST:
                case TOKEN_STR:
                case TOKEN_F32:
                case TOKEN_COLON:
                case TOKEN_MINUS:
                case TOKEN_PLUS:
                case TOKEN_SIZE:
                case TOKEN_QUOTE:
                case TOKEN_LBRACKET:
                case TOKEN_RBRACKET:
                default: {
                    ERROR_PRINT(token);
                }
                }
                break;
            }
            case INST_CALL:
            case INST_JPZ:
            case INST_JUMP: {
                SET_NEXT(memory, token, i);
                switch (token.tag) {
                case TOKEN_U32: {
                    set_pre_inst_i32(token, pre_inst);
                    break;
                }
                case TOKEN_STR: {
                    set_pre_inst_string(token, pre_inst);
                    break;
                }
                case TOKEN_INST:
                case TOKEN_F32:
                case TOKEN_COLON:
                case TOKEN_MINUS:
                case TOKEN_PLUS:
                case TOKEN_SIZE:
                case TOKEN_QUOTE:
                case TOKEN_LBRACKET:
                case TOKEN_RBRACKET:
                default: {
                    ERROR_PRINT(token);
                }
                }
                break;
            }
            case COUNT_INST_TAG:
            default: {
                ERROR_PRINT(token);
            }
            }
            break;
        }
        case TOKEN_STR: {
            const String string = token.body.as_string;
            SET_NEXT(memory, token, i);
            if (token.tag != TOKEN_COLON) {
                ERROR_PRINT(token);
            }
            Label* label = alloc_label(memory);
            label->string = string;
            label->index_inst = index_inst;
            break;
        }
        case TOKEN_PLUS: {
            SET_NEXT(memory, token, i);
            switch (token.tag) {
            case TOKEN_QUOTE: {
                SET_NEXT(memory, token, i);
                set_heap_char(memory, token);
                SET_NEXT(memory, token, i);
                if (token.tag != TOKEN_QUOTE) {
                    ERROR_PRINT(token);
                }
                break;
            }
            case TOKEN_SIZE: {
                const SizeTag tag = token.body.as_size_tag;
                SET_NEXT(memory, token, i);
                if (token.tag != TOKEN_LBRACKET) {
                    ERROR_PRINT(token);
                }
                SET_NEXT(memory, token, i);
                switch (tag) {
                case SIZE_I8: {
                    while (token.tag != TOKEN_RBRACKET) {
                        set_heap<i8, 0x7F>(token, memory, &i);
                        SET_NEXT(memory, token, i);
                    }
                    break;
                }
                case SIZE_I16: {
                    while (token.tag != TOKEN_RBRACKET) {
                        set_heap<i16, 0x7FFF>(token, memory, &i);
                        SET_NEXT(memory, token, i);
                    }
                    break;
                }
                case SIZE_I32: {
                    while (token.tag != TOKEN_RBRACKET) {
                        set_heap<i32, I32_MAX>(token, memory, &i);
                        SET_NEXT(memory, token, i);
                    }
                    break;
                }
                case SIZE_F32: {
                    while (token.tag != TOKEN_RBRACKET) {
                        set_heap_f32(token, memory, &i);
                        SET_NEXT(memory, token, i);
                    }
                    break;
                }
                case COUNT_SIZE_TAG:
                default: {
                    ERROR_PRINT(token);
                }
                }
                break;
            }
            case TOKEN_STR:
            case TOKEN_U32:
            case TOKEN_F32:
            case TOKEN_INST:
            case TOKEN_COLON:
            case TOKEN_MINUS:
            case TOKEN_PLUS:
            case TOKEN_LBRACKET:
            case TOKEN_RBRACKET:
            default: {
                ERROR_PRINT(token);
            }
            }
            break;
        }
        case TOKEN_U32:
        case TOKEN_F32:
        case TOKEN_COLON:
        case TOKEN_MINUS:
        case TOKEN_SIZE:
        case TOKEN_QUOTE:
        case TOKEN_LBRACKET:
        case TOKEN_RBRACKET:
        default: {
            ERROR_PRINT(token);
        }
        }
    }
    for (u32 i = 0; i < memory->len_pre_insts; ++i) {
        PreInst* pre_inst = &memory->pre_insts[i];
        if (!pre_inst->resolved) {
            for (u32 j = 0; j < memory->len_labels; ++j) {
                const Label label = memory->labels[j];
                if (EQ_STR(label.string, pre_inst->label)) {
                    pre_inst->inst.op = label.index_inst;
                    pre_inst->resolved = true;
                    break;
                }
            }
        }
        EXIT_IF(!pre_inst->resolved);
        memory->vm.insts[i] = pre_inst->inst;
    }
}
