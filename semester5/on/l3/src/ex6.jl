# Author: Jakub Ogrodowczyk

include("./solvers.jl")
using .Solvers

function f1(x::Float64)
    return exp(1.0-x) - 1.0
end

function f2(x::Float64)
    return x * exp(-x)
end

function df1(x::Float64)
    return -exp(1.0-x)
end

function df2(x::Float64)
    return exp(-x) * (1.0 - x)
end

delta = 1e-5
epsilon = 1e-5

function bisect(f, a, b, function_name)
    x, y, it, err = Solvers.mbisekcji(f, a, b, delta, epsilon)
    println("Metoda bisekcji dla $function_name(x):")
    println("interval: [$a, $b], x: $x, f(x) = $y, it: $it, err: $err")
end

function newton(f, df, x0, function_name)
    x, y, it, err = Solvers.mstycznych(f, df, x0, delta, epsilon, 500)
    println("Metoda stycznych dla $function_name(x):")
    println("x0: $x0, x: $x, f(x) = $y, it: $it, err: $err")
end

function sieczne(f, x0, x1, function_name)
    # println("----------------------------------")
    x, y, it, err = Solvers.msiecznych(f, x0, x1, delta, epsilon, 500)
    println("Metoda siecznych dla $function_name(x):")
    println("x0: $x0, x1: $x1, x: $x, f(x) = $y, it: $it, err: $err")
end

# bisect(f1, 0.0, 2.0, "f1")
# bisect(f1, 0.0, 1.0, "f1")
# bisect(f1, -2.0, 1.0, "f1")
# bisect(f1, 0.0, 10.0^10, "f1")
# bisect(f1, -11.0^10, 1.0, "f1")
# bisect(f1, 2.0, 3.0, "f1")

# newton(f1, df1, 1.0, "f1")
# newton(f1, df1, 0.0, "f1")
# newton(f1, df1, -5.0, "f1")
# newton(f1, df1, 2.0, "f1")
# newton(f1, df1, 3.0, "f1")
# newton(f1, df1, 4.0, "f1")
# newton(f1, df1, 5.0, "f1")
# newton(f1, df1, 6.0, "f1")
# newton(f1, df1, 7.0, "f1")
# newton(f1, df1, 7.215, "f1")
# newton(f1, df1, 8.0, "f1")
# newton(f1, df1, 100.0, "f1")
# newton(f1, df1, -800.0, "f1")

# sieczne(f1, 0.0, 0.0, "f1")
# sieczne(f1, 0.0, 0.5, "f1")
# sieczne(f1, -0.5, 0.0, "f1")
# sieczne(f1, 10.5, -100.0, "f1")
# sieczne(f1, 3.0, 5.0, "f1")
# sieczne(f1, 500.0, 500.5, "f1")

# bisect(f2, -1.0, 1.0, "f2")
bisect(f2, -5.0, 10.0, "f2")
# bisect(f2, -2.0, 10.0^10, "f2")
# bisect(f2, -11.0^10, 11.0^10, "f2")
# bisect(f2, 2.0, 3.0, "f2")

# newton(f2, df2, 0.0, "f2")
# newton(f2, df2, -5.0, "f2")
# newton(f2, df2, -10.0, "f2")
# newton(f2, df2, -100.0, "f2")
# newton(f2, df2, -1000.0, "f2")
# newton(f2, df2, 0.99, "f2")
# newton(f2, df2, 0.999, "f2")
# newton(f2, df2, 1.0, "f2")
# newton(f2, df2, 2.0, "f2")
# newton(f2, df2, 10.0, "f2")

# sieczne(f2, 0.0, 0.0, "f2")
# sieczne(f2, 0.0, 0.5, "f2")
# sieczne(f2, -1.5, -0.5, "f2")
# sieczne(f2, 1.0, 0.5, "f2")
# sieczne(f2, 0.5, 1.0, "f2")
# sieczne(f2, 0.5, 1.5, "f2")
# sieczne(f2, 0.1, 1.5, "f2")
# sieczne(f2, -1000.0, -999.5, "f2")
# sieczne(f2, 1.0, 1.5, "f2")
# sieczne(f2, 3.0, 5.0, "f2")
# sieczne(f2, 500.0, 500.5, "f2")

# print(f2(-1000.0))
# Wartosc nie zbiega, dokonczyc