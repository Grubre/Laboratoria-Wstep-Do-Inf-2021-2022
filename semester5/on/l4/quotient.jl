# Author: Jakub Ogrodowczyk

module Quotients
export ilorazyRoznicowe
export warNewton

function ilorazyRoznicowe(x::Vector{Float64}, f::Vector{Float64})
    n = length(x)

    ret = [f[1]]

    for i in 1:n-1
        for j in 1:n-i
            f[j] = (f[j + 1] - f[j]) / (x[i + j] - x[j])
        end
        push!(ret, f[1])
    end

    return ret
end

function warNewton(x::Vector{Float64}, fx::Vector{Float64}, t::Float64)
    nt = 0

    for i in reverse(1:length(x))
        nt = fx[i] + nt * (t - x[i])
    end

    return nt
end

function naturalna(x::Vector{Float64}, fx::Vector{Float64})
    n = length(x)
    a = zeros(n)
    a[n] = fx[n]

    for i in reverse(1:n)
        for j in i:n-1
            a[j] -= a[j+1] * x[j]
        end
        a[i] += fx[i]
    end

    return a
end

end