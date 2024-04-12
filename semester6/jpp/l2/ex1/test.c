#include "ex1.h"
#include <stdio.h>

int main() {
    int num1 = 1, num2 = 2, num3 = 3;

    printf("exgcd(7,31) = %d\n", extendedGcd(7, 31).d);

    printf("%d! = %d\n", num1, factorial(num1));
    printf("GCD(%d, %d) = %d\n", num1, num2, gcd(num1, num2));

    ExtendedGcd result = extendedGcd(num1, num2);
    printf("ExGCD(%d, %d) = (x = %d, y = %d, gcd = %d)\n", num1, num2, result.x, result.y, result.d);

    DiophantineEq diophantineResult = diophantineEq(num1, num2, 0);
    printf("DiophantineEq(%d, %d, %d) = (x = %d, y = %d)\n", num1, num2, num3, diophantineResult.x, diophantineResult.y);

    return 0;
}
