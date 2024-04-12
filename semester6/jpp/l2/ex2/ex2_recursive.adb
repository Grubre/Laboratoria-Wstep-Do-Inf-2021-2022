with Ada.Text_IO;
with Ada.Command_Line;
use Ada.Text_IO;

procedure ex2_recursive is
    type ExtendedGcdRet is record
        X : Integer;
        Y : Integer;
        D : Integer;
    end record;
    type DiophantineEqRet is record
        X : Integer;
        Y : Integer;
    end record;

    function factorial (N : Integer) return Integer is
    begin
        if N = 0 then
            return 1;
        else
            return N * factorial (N - 1);
        end if;
    end factorial;

    function gcd (A, B : Integer) return Integer is
    begin
        if B = 0 then
            return A;
        else
            return gcd (B, A mod B);
        end if;
    end gcd;

    function extendedGcd (A, B : Integer) return ExtendedGcdRet is
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

    function diophantineEq (A : Integer; B : Integer; C : Integer) return DiophantineEqRet is
        GcdRet : ExtendedGcdRet := extendedGcd (A, B);
    begin
        if C mod GcdRet.D /= 0 then
            raise Constraint_Error;
        end if;
        return (C / GcdRet.D * GcdRet.X, C / GcdRet.D * GcdRet.Y);
    end diophantineEq;


begin
    declare
        G : DiophantineEqRet;
    begin
    -- test factorial
    Put_Line ("0! = " & Integer'Image (factorial (5)));
    Put_Line ("5! = " & Integer'Image (factorial (5)));
    Put_Line ("10! = " & Integer'Image (factorial (5)));
    -- test gcd
    Put_Line ("gcd(1, 9) = " & Integer'Image (gcd (1, 9)));
    Put_Line ("gcd(5, 15) = " & Integer'Image (gcd (5, 15)));
    Put_Line ("gcd(7, 31) = " & Integer'Image (gcd (7, 31)));
    -- test diophantine
    G := diophantineEq (7, 31, 1);
    Put_Line ("7x + 31y = 1, x = " & Integer'Image (G.X) & ", y = " & Integer'Image (G.Y));
    end;
end ex2_recursive;
