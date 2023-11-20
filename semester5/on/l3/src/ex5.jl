# Author: Jakub Ogrodowczyk

include("./solvers.jl")
using .Solvers

function f(x::Float64)
    exp(x) - 3x
end

delta = 1e-4
epsilon = 1e-4

x, y, it, err = Solvers.mbisekcji(f, 0.0, 1.0, delta, epsilon)
println("x: $x, f(x) = $y, it: $it, err: $err")
x, y, it, err = Solvers.mbisekcji(f, 1.0, 2.0, delta, epsilon)
println("x: $x, f(x) = $y, it: $it, err: $err")