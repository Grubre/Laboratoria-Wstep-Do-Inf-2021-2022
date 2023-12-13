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

    for i in reverse(1:n)
        for j in i:n-1
            a[j] -= a[j+1] * x[i]
        end
        a[i] += fx[i]
    end

    return a
end

function rysujNnfx(f, a::Float64, b::Float64, n::Int)
    h = (b - a) / n
    points_f = [a + i * h for i in 0:n]
    values_f = [f(x) for x in points_f]

    fx = ilorazyRoznicowe(points_f, values_f)

    points_num = 100 * n + 1
    delta = (b - a) / (points_num - 1)

    x_values = [a + i * delta for i in 0:points_num-1]
    y_values = [f(x) for x in x_values]
    w_values = [warNewton(points_f, fx, x) for x in x_values]

    plot()
    title!("n = $n")
    plot!(x_values, y_values, label="f(x)")
    plot!(x_values, w_values, label="w(x)")
end

end