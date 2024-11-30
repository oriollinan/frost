#include "defs.h"

int main(void)
{
    int32_t a = 10;
    int32_t b = 20;
    int32_t c = add(a, b);

    printf("%s\n", ADD_FUNC_PROTO);
    printf("call %s(%d, %d)\n\n", ADD_FUNC_NAME, a, b);

    printf("a = %d\n", a);
    printf("b = %d\n\n", b);

    printf("a + b = %d\n", c);

    return 0;
}
