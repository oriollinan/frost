#include <getopt.h>
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

#include <ctype.h>
#include <stdio.h>
#include <string.h>

void trim_whitespace(char *str)
{
    char *end;

    while (isspace((unsigned char) *str)) {
        str++;
    }

    end = str + strlen(str) - 1;
    while (end > str && isspace((unsigned char) *end)) {
        end--;
    }

    *(end + 1) = '\0';
}

void pretty_print(const char *llvm_code)
{
    char buffer[1024];
    int inside_block = 0;

    strncpy(buffer, llvm_code, sizeof(buffer) - 1);
    buffer[sizeof(buffer) - 1] = '\0';

    char *line = strtok(buffer, "\n");
    while (line != NULL) {
        trim_whitespace(line);

        if (strlen(line) == 0) {
            line = strtok(NULL, "\n");
            continue;
        }

        if (strchr(line, '}')) {
            inside_block--;
        }

        for (int i = 0; i < inside_block; i++) {
            printf("    ");
        }
        printf("%s\n", line);

        if (strchr(line, '{')) {
            inside_block++;
        }

        line = strtok(NULL, "\n");
    }
}

void cleanup(char **ptr)
{
    if (*ptr) {
        free(*ptr);
        *ptr = NULL;
    }
}

int main(int argc, char *argv[])
{
    char *source_file = NULL;
    char *llvm_file = NULL;

    struct option long_options[] = {
        {"source", required_argument, 0, 's'},
        {"llvm", required_argument, 0, 'l'},
        {0, 0, 0, 0},
    };

    int opt;
    int option_index = 0;

    while ((opt = getopt_long(argc, argv, "s:l:", long_options, &option_index))
        != -1) {
        switch (opt) {
            case 's': source_file = optarg; break;
            case 'l': llvm_file = optarg; break;
            default:
                fprintf(stderr,
                    "Usage: %s [--source file.scm] [--llvm file.ll]\n",
                    argv[0]);
                return EXIT_FAILURE;
        }
    }

    if (source_file) {
        printf("--- Scheme source file ---\n%s\n", source_file);
    }

    if (llvm_file) {
        printf("--- LLVM file ---\n%s\n", llvm_file);
    }

    __attribute__((cleanup(cleanup))) char *lc =
        source_file ? read_contents(source_file) : NULL;
    __attribute__((cleanup(cleanup))) char *gc =
        llvm_file ? read_contents(llvm_file) : NULL;

    if ((source_file && !lc) || (llvm_file && !gc)) {
        return EXIT_FAILURE;
    }

    if (lc) {
        printf("--- Scheme code ---\n%s\n", lc);
    }

    if (gc) {
        printf("--- Generated code ---\n");
        pretty_print(gc);
        printf("\n");
    }

    int result = $$generated();

    printf("--- Result ---\n%d\n", result);

    return EXIT_SUCCESS;
}
