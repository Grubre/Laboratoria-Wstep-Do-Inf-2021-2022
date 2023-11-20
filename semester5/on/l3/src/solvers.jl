# Author: Jakub Ogrodowczyk

module Solvers

function mbisekcji(f::Function, a::Float64, b::Float64, delta::Float64, epsilon::Float64)
    if sign(f(a)) == sign(f(b))
        return Nothing, Nothing, Nothing, 1
    end

    it = 1
    
    while true
        c = (a + b) / 2

        f_c = f(c)

        if f_c == 0 || (b - a) / 2 < delta || abs(f_c) < epsilon
            return c, f_c, it, 0
        end

        it += 1
        if (sign(f_c) == sign(f(a)))
            a = c
        else
            b = c
        end
    end
end


function mstycznych(f::Function, pf::Function, x0::Float64, delta::Float64, epsilon::Float64, maxit::Int)
    it = 1
    x = x0
    while it < maxit
        y = f(x)
        yp = pf(x)

        if abs(yp) < epsilon
            return x, y, it, 2
        end
        
        x1 = x - y / yp

        if abs(x1 - x) < delta
            return x1, f(x1), it, 0
        end

        x = x1

        it += 1
    end
    return x, f(x), it, 1
end

function msiecznych(f::Function, x0::Float64, x1::Float64, delta::Float64, epsilon::Float64, maxit::Int)
    it = 1

    while it < maxit
        x2 = x1 - f(x1) * (x1 - x0) / (f(x1) - f(x0))

        if abs(x2 - x1) < delta || abs(f(x1)) < epsilon
            return x2, f(x2), it, 0
        end

        x0 = x1
        x1 = x2

        it += 1
    end

    return x1, f(x1), it, 1
end


end