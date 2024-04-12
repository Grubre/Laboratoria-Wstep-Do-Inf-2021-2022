#include "ex1.h"
#include <stdlib.h>

int factorial(int n) {
    int result = 1;
    for (int i = 1; i <= n; i++) {
        result *= i;
    }
    return result;
}

int gcd(int a, int b) {
    int temp;
    while (b != 0) {
        temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

ExtendedGcd extendedGcd(int a, int b) {
    int x0 = 1, y0 = 0, x1 = 0, y1 = 1, temp;
    while (b != 0) {
        int q = a / b;
        int r = a % b;
        
        a = b;
        b = r;
        
        temp = x1;
        x1 = x0 - q * x1;
        x0 = temp;
        
        temp = y1;
        y1 = y0 - q * y1;
        y0 = temp;
    }
    ExtendedGcd result = {x0, y0, a};
    return result;
}

DiophantineEq diophantineEq(int a, int b, int c) {
    ExtendedGcd gcdResult = extendedGcd(a, b);
    if (c % gcdResult.d == 0) {
        DiophantineEq result = {gcdResult.x * (c / gcdResult.d), gcdResult.y * (c / gcdResult.d)};
        return result;
    } else {
        exit(1);
    }
}
