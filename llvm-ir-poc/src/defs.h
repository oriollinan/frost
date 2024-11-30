#include <stdint.h>
#include <stdio.h>

#ifndef __DEFS_H__
    #define __DEFS_H__

    #define ADD_FUNC_NAME  "add"
    #define ADD_FUNC_PROTO "i32 @add(i32 %a, i32 %b)"

int32_t add(int32_t a, int32_t b);

#endif /* __DEFS_H__ */
