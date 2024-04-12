#pragma once

typedef struct {
    int x, y, d;
} ExtendedGcd;

typedef struct {
    int x, y;
} DiophantineEq;

int factorial(int n);
int gcd(int a, int b);
ExtendedGcd extendedGcd(int a, int b);
DiophantineEq diophantineEq(int a, int b, int c);
