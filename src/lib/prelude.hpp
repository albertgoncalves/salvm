#ifndef __PRELUDE_H__
#define __PRELUDE_H__

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-W#warnings"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#pragma GCC diagnostic pop

typedef uint32_t u32;

typedef int8_t  i8;
typedef int16_t i16;
typedef int32_t i32;

typedef float  f32;
typedef double f64;

typedef FILE File;

#define I32_MAX 0x7FFFFFFF

template <typename T>
struct Vec2 {
    T x;
    T y;
};

#define EXIT()                                                       \
    {                                                                \
        fprintf(stderr, "%s:%s:%d\n", __FILE__, __func__, __LINE__); \
        exit(EXIT_FAILURE);                                          \
    }

#define EXIT_WITH(x)                                                         \
    {                                                                        \
        fprintf(stderr, "%s:%s:%d `%s`\n", __FILE__, __func__, __LINE__, x); \
        exit(EXIT_FAILURE);                                                  \
    }

#define EXIT_IF(condition)     \
    if (condition) {           \
        EXIT_WITH(#condition); \
    }

#define STATIC_ASSERT(condition) static_assert(condition, "!(" #condition ")")

#endif
