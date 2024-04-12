with Interfaces.C; use Interfaces.C;

package Iterative is
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
        with Export => True,
             Convention => C,
             External_Name => "factorial";

    function gcd (A, B : int) return int
        with Export => True,
             Convention => C,
             External_Name => "gcd";

    function extendedGcd (A, B : int) return ExtendedGcdRet
        with Export => True,
             Convention => C,
             External_Name => "extendedGcd";

    function diophantineEq (A, B, C : int) return DiophantineEqRet
        with Export => True,
             Convention => C,
             External_Name => "diophantineEq";
end Iterative;

