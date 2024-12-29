#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int readInt() {
    int x;
    scanf("%d", &x);
    return x;
}

char *readString() {
    char buffer[1024];
    scanf("%1023s", buffer);
    char *result = malloc(strlen(buffer) + 1);
    strcpy(result, buffer);
    return result;
}

void printInt(int x) {
    printf("%d\n", x);
}

void printString(char *s) {
    printf("%s\n", s);
}
