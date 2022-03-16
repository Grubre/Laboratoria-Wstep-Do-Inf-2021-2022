#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "agents.h"

struct agent newagent(int x, int y)
{
    struct agent a;
    a.x = x;
    a.y = y;
    return a;
}

void north(struct agent *a)
{
    (*a).y++;
}
void south(struct agent *a)
{
    (*a).y--;
}
void east(struct agent *a)
{
    (*a).x++;
}
void west(struct agent *a)
{
    (*a).y--;
}

double distance(struct agent a1, struct agent a2)
{
    int x = abs(a1.x - a2.x);
    int y = abs(a1.y - a2.y);
    return sqrt(x * x + y * y);
}
