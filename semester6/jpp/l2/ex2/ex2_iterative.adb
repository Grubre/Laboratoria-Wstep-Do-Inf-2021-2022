with Ada.Text_IO;
use Ada.Text_IO;

procedure ex2_iterative is
    type ExtendedGcdRet is record
        X : Integer;
        Y : Integer;
        D : Integer;
    end record;
    type DiophantineEqRet is record
        X : Integer;
        Y : Integer;
    end record;

    function factorial(N : Integer) return Integer is
        Result : Integer := 1;
    begin
        for I in 1..N loop
            Result := Result * I;
        end loop;
        return Result;
    end factorial;

    function gcd (A : Integer; B : Integer) return Integer is
        LocalA : Integer := A;
        LocalB : Integer := B;
    begin
        while LocalB /= 0 loop
            declare
                T : Integer := LocalB;
            begin
                LocalB := LocalA mod LocalB;
                LocalA := T;
            end;
        end loop;
        return LocalA;
    end gcd;

    function extendedGcd (A : Integer; B : Integer) return ExtendedGcdRet is
        X0 : Integer := 1;
        Y0 : Integer := 0;
        X1 : Integer := 0;
        Y1 : Integer := 1;
        Temp : Integer := 0;
        LocalA : Integer := A;
        LocalB : Integer := B;
    begin
        while LocalB /= 0 loop
            declare
                Q : Integer := LocalA / LocalB;
                R : Integer := LocalA mod LocalB;
            begin
                LocalA := LocalB;
                LocalB := R;

                Temp := X1;
                X1 := X0 - Q * X1;
                X0 := Temp;

                Temp := Y1;
                Y1 := Y0 - Q * Y1;
                Y0 := Temp;
            end;
        end loop;
        return (X0, Y0, LocalA);
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
end ex2_iterative;
