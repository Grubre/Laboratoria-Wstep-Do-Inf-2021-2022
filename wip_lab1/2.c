#include <stdio.h>
#include <math.h>

int main()
{
    float a,b,c;
    scanf("%f %f %f", &a, &b, &c);

    float delta = (b * b) - (4 * a * c);
    if(delta < 0)
    {
        printf("Nie ma rozwiazania w zbiorze liczb rzeczywistych");
    }
    else if(delta == 0)
    {
        float rozw = -b / (2 * a);
        printf("x = %f",rozw);
    }
    else
    {
        float rozw1 = (-b - sqrt(delta))/(2 * a), rozw2 = (-b + sqrt(delta)) / (2 * a);
        printf("x1 = %f\nx2 = %f",rozw1,rozw2);
    }
    return 0;
}