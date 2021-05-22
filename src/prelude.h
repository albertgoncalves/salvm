#ifndef __PRELUDE_H__
#define __PRELUDE_H__

#include <assert.h>
#include <stdalign.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef uint32_t u32;
typedef size_t   usize;

typedef int8_t  i8;
typedef int16_t i16;
typedef int32_t i32;

typedef float  f32;

typedef enum {
    FALSE = 0,
    TRUE,
} Bool;

typedef FILE File;

#define STATIC_ASSERT _Static_assert

#define ERROR()                                                      \
    {                                                                \
        fprintf(stderr, "%s:%s:%d\n", __FILE__, __func__, __LINE__); \
        exit(EXIT_FAILURE);                                          \
    }

#define EXIT_IF(condition)         \
    if (condition) {               \
        fprintf(stderr,            \
                "%s:%s:%d `%s`\n", \
                __FILE__,          \
                __func__,          \
                __LINE__,          \
                #condition);       \
        exit(EXIT_FAILURE);        \
    }

#define TEST(fn, block)    \
    static void fn(void) { \
        block;             \
        printf(".");       \
    }

#endif
