#ifndef __PRELUDE_H__
#define __PRELUDE_H__

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint32_t u32;
typedef int32_t  i32;
typedef float    f32;
typedef double   f64;

typedef enum {
    FALSE = 0,
    TRUE,
} Bool;

typedef FILE File;

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

#endif
