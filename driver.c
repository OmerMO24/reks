#include <stdio.h>
#include <stdlib.h>

// Rename your LLVM main to avoid conflict
extern int my_main(int a);  // Matches your LLVM IR: define i32 @main(i32)

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <number>\n", argv[0]);
        return 1;
    }
    int input = atoi(argv[1]);  // Convert argument to integer
    int result = my_main(input);
    printf("Result: %d\n", result);
    return result;  // Optional: use as exit code
}
