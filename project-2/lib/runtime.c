#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printInt(int x) {
    printf("%d\n", x);
}

void printString(char *s) {
    printf("%s\n", s);
}

void error() {
    printf("runtime error\n");
    exit(1);
}

int readInt() {
    int x;
    scanf("%d", &x);
    return x;
}

char *readString() {
    size_t bufferSize = 1024, length = 0;
    char *buffer = malloc(bufferSize);
    if (!buffer) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }
    
    int c;
    while ((c = getchar()) != '\n' && c != EOF) {
        if (length + 1 >= bufferSize) {
            bufferSize *= 2;
            buffer = realloc(buffer, bufferSize);
            if (!buffer) {
                fprintf(stderr, "Memory allocation failed\n");
                exit(1);
            }
        }
        buffer[length++] = (char) c;
    }
    buffer[length] = '\0';
    
    return buffer;
}
