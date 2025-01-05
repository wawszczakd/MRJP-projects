#include <stdbool.h>
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
    fprintf(stderr, "runtime error\n");
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

bool __equString(char *s1, char *s2) {
    return strcmp(s1, s2) == 0;
}

bool __neString(char *s1, char *s2) {
    return strcmp(s1, s2) != 0;
}

char *__concatString(char *s1, char *s2) {
    size_t len1 = strlen(s1);
    size_t len2 = strlen(s2);
    size_t len = len1 + len2;
    char *buffer = malloc(len + 1);
    if (!buffer) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }
    
    memcpy(buffer, s1, len1);
    memcpy(buffer + len1, s2, len2);
    buffer[len] = '\0';
    
    return buffer;
}
