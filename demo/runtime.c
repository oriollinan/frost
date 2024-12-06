#include <stdio.h>
#include <stdlib.h>
#include "defs.h"

__attribute__((warn_unused_result)) char *read_contents(const char *file_name)
{
    size_t size = 0;
    char *contents = NULL;

    FILE *lf = fopen(file_name, "r");
    if (!lf) {
        perror("fopen");
        return NULL;
    }

    fseek(lf, 0, SEEK_END);
    size = ftell(lf);
    rewind(lf);

    contents = malloc(size + 1);
    if (!contents) {
        perror("malloc");
        fclose(lf);
        return NULL;
    }

    fread(contents, 1, size, lf);
    contents[size] = '\0';
    fclose(lf);

    return contents;
}

void cleanup(char **ptr)
{
    if (*ptr) {
        free(*ptr);
        *ptr = NULL;
    }
}

int main(void)
{
    __attribute__((cleanup(cleanup))) char *lc = read_contents("add.scm");
    __attribute__((cleanup(cleanup))) char *gc = read_contents("generated.ll");

    if (!lc || !gc) {
        return EXIT_FAILURE;
    }

    printf("--- Scheme code ---\n%s\n", lc);
    printf("--- Generated code ---\n%s\n", gc);

    int result = $$generated();

    printf("--- Result ---\n%d\n", result);
}
