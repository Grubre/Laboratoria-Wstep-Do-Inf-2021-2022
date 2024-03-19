#include "ex1.h"

int factorial(int n) {
    if (n == 0) {
        return 1;
    }
    return n * factorial(n - 1);
}

int gcd(int a, int b) {
    if (b == 0)
        return a;
    else
        return gcd(b, a % b);
}

ExtendedGcd extendedGcd(int a, int b) {
    if (b == 0) {
        ExtendedGcd result = {1, 0, a};
        return result;
    } else {
        ExtendedGcd temp = extendedGcd(b, a % b);
        ExtendedGcd result = {temp.y, temp.x - (a / b) * temp.y, temp.d};
        return result;
    }
}
