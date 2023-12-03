include("./quotient.jl")
using .Quotients

println(ilorazyRoznicowe([3.,1.,5.,6.], [1., -3., 2., 4.]))
x = [1.,2.,3.]
f = [1., 4., 9.]
fx = ilorazyRoznicowe(x, f)
println(warNewton(x, fx, 4.))