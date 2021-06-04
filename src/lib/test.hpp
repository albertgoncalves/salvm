#ifndef __TEST_H__
#define __TEST_H__

#define TEST(fn, block) \
    static void fn() {  \
        block;          \
        printf(".");    \
    }

#endif
