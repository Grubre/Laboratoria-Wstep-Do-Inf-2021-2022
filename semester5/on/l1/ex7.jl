# Jakub Ogrodowczyk

# Args:
# `f::Function`: funkcja której pochodną będziemy liczyć
# `x::Real`: punkt w którym liczymy pochodną
# `h::Real`: wielkość kroku
#
# Return:
# Przybliżona wartość pochodnej funkcji f w punkcie x
function derivative(f::Function, x::Real, h::Real)
    return (f(x + h) - f(x)) / h
end

function f(x)
    return sin(x) + cos(3*x)
end
function f_prawdziwe(x)
    return cos(x) - 3*sin(3*x)
end

# Punkt w którym szukamy pochodnej
x = 1.0
d_prawdziwe = f_prawdziwe(x)

for n = 0:54
    h = 2.0^-n # krok
    d = derivative(f, x, h) # wartość pochodnej
    blad = abs(d_prawdziwe - d) # wartość błędu
    println("h <- 2.0^-$n (=$h)")
    println("_f'($x) = $d")
    println(" f'($x) = $d_prawdziwe")
    println("blad: $blad")
end