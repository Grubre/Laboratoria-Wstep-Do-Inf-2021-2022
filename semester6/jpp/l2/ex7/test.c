#include <stdio.h>
typedef struct {
    int x;
    int y;
    int d;
} ExtendedGcd;

typedef struct {
    int x;
    int y;
} DiophantineEq;

int factorial(int n);
int gcd(int a, int b);
ExtendedGcd extendedGcd(int a, int b);
DiophantineEq diophantineEq(int a, int b, int c);

int main() {
    printf("%d", factorial(5));
}
