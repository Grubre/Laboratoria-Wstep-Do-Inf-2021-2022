package body iterative is

function factorial (n: int) return int is
    i: int := 1;
begin
    for j in 1..n loop
        i := i * j;
    end loop;
    return i;
end factorial;

function gcd (A : int; B : int) return int is
    LocalA : int := A;
    LocalB : int := B;
begin
    while LocalB /= 0 loop
        declare
            T : int := LocalB;
        begin
            LocalB := LocalA mod LocalB;
            LocalA := T;
        end;
    end loop;
    return LocalA;
end gcd;

function extendedGcd (A : int; B : int) return ExtendedGcdRet is
    X0 : int := 1;
    Y0 : int := 0;
    X1 : int := 0;
    Y1 : int := 1;
    Temp : int := 0;
    LocalA : int := A;
    LocalB : int := B;
begin
    while LocalB /= 0 loop
        declare
            Q : int := LocalA / LocalB;
            R : int := LocalA mod LocalB;
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

function diophantineEq (A : int; B : int; C : int) return DiophantineEqRet is
    GcdRet : ExtendedGcdRet := extendedGcd (A, B);
begin
    if C mod GcdRet.D /= 0 then
        raise Constraint_Error;
    end if;
    return (C / GcdRet.D * GcdRet.X, C / GcdRet.D * GcdRet.Y);
end diophantineEq;

end iterative;
