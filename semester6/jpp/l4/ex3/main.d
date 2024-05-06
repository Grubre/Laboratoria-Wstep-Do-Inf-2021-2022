import std.stdio;
import gf;
import dh;
import user;

alias gfdf = GF!1234577;

void main()
{
    auto a = gfdf(5);
    auto b = gfdf(7);

    auto dh = DHSetup!gfdf();

    User!gfdf(&dh);

    writeln("a + b = ", a + b);
    writeln("a - b = ", a - b);
}
