#include <stdio.h>
#include <float.h>

int main() {
    float macheps_Float = 1.0f * FLT_EPSILON;
    double macheps_Double = 1.0 * DBL_EPSILON;
    long double macheps_LDouble = 1.0L * LDBL_EPSILON;

    printf("Machine Epsilon dla floata: %e\n", macheps_Float);
    printf("Machine Epsilon dla doubla: %e\n", macheps_Double);
    printf("Machine Epsilon dla long doubla: %Le\n", macheps_LDouble);

    printf("Wielkość floata w bitach: %zu\n", sizeof(float) * 8);
    printf("Wielkość doubla w bitach: %zu\n", sizeof(double) * 8);
    printf("Wielkość long doubla w bitach: %zu\n", sizeof(long double) * 8);

    return 0;
}
