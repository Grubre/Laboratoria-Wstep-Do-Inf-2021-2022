package body recursive is

    function factorial (N : int) return int is
    begin
        if N = 0 then
            return 1;
        else
            return N * factorial (N - 1);
        end if;
    end factorial;

    function gcd (A, B : int) return int is
    begin
        if B = 0 then
            return A;
        else
            return gcd (B, A mod B);
        end if;
    end gcd;

    function extendedGcd (A, B : int) return ExtendedGcdRet is
        G : ExtendedGcdRet;
    begin
        if B = 0 then
            G.X := 1;
            G.Y := 0;
            G.D := A;
            return G;
        else
            G := extendedGcd (B, A mod B);
            return (G.Y, G.X - (A / B) * G.Y, G.D);
        end if;
    end extendedGcd;

    function diophantineEq (A : int; B : int; C : int) return DiophantineEqRet is
        GcdRet : ExtendedGcdRet := extendedGcd (A, B);
    begin
        if C mod GcdRet.D /= 0 then
            raise Constraint_Error;
        end if;
        return (C / GcdRet.D * GcdRet.X, C / GcdRet.D * GcdRet.Y);
    end diophantineEq;


end recursive;
