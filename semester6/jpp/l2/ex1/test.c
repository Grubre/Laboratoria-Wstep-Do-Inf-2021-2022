#include "ex1.h"
#include <stdio.h>

int main() {
    int num1 = 5, num2 = 15;
    
    printf("%d! = %d\n", num1, factorial(num1));
    printf("GCD(%d, %d) = %d\n", num1, num2, gcd(num1, num2));
    
    ExtendedGcd result = extendedGcd(num1, num2);
    printf("ExGCD(%d, %d) = (x = %d, y = %d, gcd = %d)\n", num1, num2, result.x, result.y, result.d);

    return 0;
}
