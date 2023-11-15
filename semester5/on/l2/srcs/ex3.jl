# Jakub Ogrodowczyk
using LinearAlgebra
using CSV
using DataFrames

function matcond(n::Int, c::Float64)
# Function generates a random square matrix A of size n with
# a given condition number c.
# Inputs:
#	n: size of matrix A, n>1
#	c: condition of matrix A, c>= 1.0
#
# Usage: matcond(10, 100.0)
#
# Pawel Zielinski
        if n < 2
         error("size n should be > 1")
        end
        if c< 1.0
         error("condition number  c of a matrix  should be >= 1.0")
        end
        (U,S,V)=svd(rand(n,n))
        return U*diagm(0 =>[LinRange(1.0,c,n);])*V'
end

function hilb(n::Int)
# Function generates the Hilbert matrix  A of size n,
#  A (i, j) = 1 / (i + j - 1)
# Inputs:
#	n: size of matrix A, n>=1
#
#
# Usage: hilb(10)
#
# Pawel Zielinski
        if n < 1
            error("size n should be >= 1")
        end
        return [1 / (i + j - 1) for i in 1:n, j in 1:n]
end

df = DataFrame(n=[], error_inverse=[], error_gauss=[])

for n in 2:100
    x = ones(n)
    A = hilb(n)
    b = A * x
    
    gauss = A \ b
    inverse = inv(A) * b
    println("$n -> $gauss")

    error_gauss = norm(gauss - x) / norm(x)
    error_inverse = norm(inverse - x) / norm(x)

    push!(df, (n=n, error_inverse=error_inverse, error_gauss=error_gauss))
end

CSV.write("output.csv", df)