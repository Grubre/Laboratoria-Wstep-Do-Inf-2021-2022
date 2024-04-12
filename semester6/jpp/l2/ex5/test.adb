with Interfaces.C; use Interfaces.C;
with Ada.Text_IO;  use Ada.Text_IO;

procedure test is
    type ExtendedGcdRet is record
        X : int;
        Y : int;
        D : int;
    end record
    with Convention => C;
    type DiophantineEqRet is record
        X : int;
        Y : int;
    end record
    with Convention => C;

    function factorial (n: int) return int
        with
        Import => True,
        Convention => C;

    function gcd (A, B : int) return int
        with
        Import => True,
        Convention => C;

    function extendedGcd (A, B : int) return ExtendedGcdRet
        with
        Import => True,
        Convention => C;

    function diophantineEq (A, B, C : int) return DiophantineEqRet
        with
        Import => True,
        Convention => C;

begin
    Put_Line("Factorial of 5 is " & int'Image(factorial(5)));
    Put_Line("GCD of 12 and 15 is " & int'Image(gcd(12, 15)));

end test;
