#include <stdio.h>
#include "2_f.h"

int main(void)
{
    double a = 2., b = 4.;
    double eps = 0.0000001;
    printf("rozwiazanie: %lf\n", rozwiazanie(a,b,eps));
    return 0;
}
