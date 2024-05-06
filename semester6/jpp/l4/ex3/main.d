import std.stdio;
import gf;

alias gfdf = GF!1234577;

void main()
{
    auto a = gfdf(5);
    auto b = gfdf(7);

    writeln("a + b = ", a + b);
    writeln("a - b = ", a - b);
}
