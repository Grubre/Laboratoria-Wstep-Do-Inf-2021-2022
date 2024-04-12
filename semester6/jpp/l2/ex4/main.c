#include <stdio.h>

typedef struct {
    int x;
    int y;
    int d;
} ExtendedGcdRet;

typedef struct {
    int x;
    int y;
} DiophantineEqRet;

extern void adainit (void);
extern void adafinal (void);
extern int factorial(int n);
extern int gcd(int a, int b);
extern ExtendedGcdRet extendedGcd(int a, int b);
extern DiophantineEqRet diophantineEq(int a, int b, int c);

int main() {
    adainit();

    printf("%d! = %d\n", 5, factorial(5));
    ExtendedGcdRet z = extendedGcd(10, 6);
    printf("gcd(10, 6) = %d\n", z.d);

    adafinal();
    return 0;
}
