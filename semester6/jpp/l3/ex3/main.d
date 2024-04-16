import std.stdio;
import gf;

void main()
{
    auto a = GF!5(3);
    auto b = GF!5(5);
    // print a + b
    writeln(a + b);
    writeln(a - b);
    writeln(a * b);
}
