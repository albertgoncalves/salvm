#ifndef __TEST_H__
#define __TEST_H__

#define EPSILON 0.0000001f

#define EQ_F32(l, r) (((l) <= ((r) + EPSILON)) && (((r) + -EPSILON) <= (l)))

#define TEST(fn, block) \
    static void fn() {  \
        block;          \
        printf(".");    \
    }

#endif
