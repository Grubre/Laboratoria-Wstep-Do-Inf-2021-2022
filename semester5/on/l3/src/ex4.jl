# Author: Jakub Ogrodowczyk

include("./solvers.jl")
using .Solvers

function f(x::Float64)
    return sin(x) - (x/2)^2
end

function df(x::Float64)
    return cos(x) - x/2
end


delta = 0.5 * 1e-5
epsilon = 0.5 * 1e-5

x, y, it, err = Solvers.mbisekcji(f, 1.5, 2.0, delta, epsilon)

println("Metoda bisekcji:")
println("x: $x, f(x) = $y, it: $it, err: $err")

x, y, it, err = Solvers.mstycznych(f, df, 1.5, delta, epsilon, 100)
println("Metoda stycznych:")
println("x: $x, f(x) = $y, it: $it, err: $err")

x, y, it, err = Solvers.msiecznych(f, 1.5, 2.0, delta, epsilon, 100)
println("Metoda siecznych:")
println("x: $x, f(x) = $y, it: $it, err: $err")