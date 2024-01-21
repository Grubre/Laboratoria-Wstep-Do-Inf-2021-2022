# Author: Jakub Ogrodowczyk
module blocksys

export write_x_to_file, calculate_b, sparse_mat_from_file, vec_from_file, gaussian_elimination, gaussian_elimination_pivot, lu!, lu_pivot!, solve_using_lu!, solve_using_lu_pivot!

using SparseArrays

"""
Parses contents of file at "filepath" into SparseMatrixCSC.

in:
    filepath::String - path to file
out:
    sparse_mat::SparseMatrixCSC{Float64, Int64} - sparse matrix of parsed values
"""
function sparse_mat_from_file(filepath)::Tuple{SparseMatrixCSC{Float64,Int64},Int64,Int64}
    cols = []
    rows = []
    vals = []

    f = open(filepath, "r")
    lines = readlines(f)
    close(f)

    n, l = split(lines[1])
    n = parse(Int64, n)
    l = parse(Int64, l)

    for i in 2:length(lines)
        i1, j1, val = split(lines[i])
        push!(rows, parse(Int64, i1))
        push!(cols, parse(Int64, j1))
        push!(vals, parse(Float64, val))
    end

    sparse_mat = sparse(rows, cols, vals)
    return (sparse_mat,n,l)
end

"""
Parses contents of file at `filepath` into Vector{Float64}.

in:
    filepath::String - path to file
out:
    vec::Vector{Float64} - vector of parsed values
"""
function vec_from_file(filepath)::Vector{Float64}
    f = open(filepath, "r")
    lines = readlines(f)
    close(f)

    n = parse(Int64, lines[1])

    vec = zeros(n)

    for i in 2:length(lines)
        val = parse(Float64, lines[i])
        vec[i - 1] = val
    end

    return vec
end

"""
Writes vector `x` to file "x.txt".

in:
    x::Vector{Float64} - vector of values
"""
function write_x_to_file(x::Vector{Float64}, filename::String)
    f = open(filename, "w")
    for i in 1:length(x)
        println(f, x[i])
    end
    close(f)
end

"""
Solves system of linear equations Ax = b using Gaussian elimination.

in:
    A::SparseMatrixCSC{Float64, Int64} - input square sparse matrix
    n::Int64 - size of matrix
    l::Int64 - size of inner matrices A_k, B_k, C_k
    b::Vector{Float64} - vector of values
out:
    x::Vector{Float64} - vector of solutions
"""
function gaussian_elimination(A::SparseMatrixCSC{Float64, Int64}, n::Int64, l::Int64, b::Vector{Float64})
    # Sprowadzanie do postaci trójkątnej górnej
    for k = 1:n-1
        for i = k+1:min(n, k+l-(k%l))
            multiplier = A[i, k] / A[k, k]
            A[i, k] = 0.0

            for j = k+1:min(n, k+l)
                A[i, j] -= multiplier * A[k, j]
            end

            b[i] -= multiplier * b[k]
        end
    end

    # Podstawienie wsteczne
    x = zeros(n)
    x[n] = b[n] / A[n, n]

    for i = n-1:-1:1
        x[i] = (b[i] - sum(A[i, j] * x[j] for j = i+1:min(n, 2*l + i))) / A[i, i]
    end

    return x
end

"""
Solves system of linear equations Ax = b using Gaussian elimination with partial pivoting.

in:
    A::SparseMatrixCSC{Float64, Int64} - input square sparse matrix
    n::Int64 - size of matrix
    l::Int64 - size of inner matrices A_k, B_k, C_k
    b::Vector{Float64} - vector of values
out:
    x::Vector{Float64} - vector of solutions
"""
function gaussian_elimination_pivot(A::SparseMatrixCSC{Float64, Int64}, n::Int64, l::Int64, b::Vector{Float64})
    p = collect(1:n)

    # Sprowadzanie do postaci trójkątnej górnej
    for k = 1:n-1
        pivot_bound = min(n, k + l - (k % l))
        max_idx = findmax([abs(A[p[i], k]) for i in k:pivot_bound])[2] + k - 1
        p[k], p[max_idx] = p[max_idx], p[k]

        for i = k+1:pivot_bound
            factor = A[p[i], k] / A[p[k], k]
            A[p[i], k] = 0.0

            for j = k+1:min(n, 2*l + k)
                A[p[i], j] -= factor * A[p[k], j]
            end

            b[p[i]] -= factor * b[p[k]]
        end
    end

    # Podstawienie wsteczne
    x = zeros(n)
    x[n] = b[p[n]] / A[p[n], n]

    for i = n-1:-1:1
        x[i] = (b[p[i]] - sum(A[p[i], j] * x[j] for j = i+1:min(n, 2*l + i))) / A[p[i], i]
    end

    return x
end

"""
Calculates in place LU decomposition of matrix A.

in:
    A::SparseMatrixCSC{Float64, Int64} - input square sparse matrix, will be overwritten with LU
    n::Int64 - size of matrix
    l::Int64 - size of inner matrices A_k, B_k, C_k
"""
function lu!(A::SparseMatrixCSC{Float64, Int64}, n::Int64, l::Int64)
    for k = 1:n-1
        row_limit = min(n, k + l - (k % l))
        for i = k+1:row_limit
            factor = A[i, k] / A[k, k]
            A[i, k] = factor

            column_limit = min(n, k + l)
            for j = k+1:column_limit
                A[i, j] -= factor * A[k, j]
            end
        end
    end
end

"""
Calculates in place LU decomposition of matrix A with partial pivoting.

in:
    A::SparseMatrixCSC{Float64, Int64} - input square sparse matrix, will be overwritten with LU
    n::Int64 - size of matrix
    l::Int64 - size of inner matrices A_k, B_k, C_k
out:
    p::Vector{Int64} - vector of permutations(swapped rows)
"""
function lu_pivot!(A::SparseMatrixCSC{Float64, Int64}, n::Int64, l::Int64)
    p = collect(1:n)

    for k = 1:n-1
        pivot_bound = min(n, k + l - (k % l))
        _, pivot_idx = findmax(abs.(A[p[k:pivot_bound], k]))
        pivot_row = k + pivot_idx - 1
        p[k], p[pivot_row] = p[pivot_row], p[k]

        for i = k+1:pivot_bound
            multiplier = A[p[i], k] / A[p[k], k]
            A[p[i], k] = multiplier

            for j = k+1:min(n, 2*l + k)
                A[p[i], j] -= multiplier * A[p[k], j]
            end
        end
    end

    return p
end

"""
Solves system of linear equations Ax = b using LU decomposition.

in:
    A::SparseMatrixCSC{Float64, Int64} - input square sparse matrix
    n::Int64 - size of matrix
    l::Int64 - size of inner matrices A_k, B_k, C_k
    b::Vector{Float64} - vector of values
out:
    x::Vector{Float64} - vector of solutions
"""
function solve_using_lu!(A::SparseMatrixCSC{Float64, Int64}, n::Int64, l::Int64, b::Vector{Float64})
    lu!(A, n, l)

    # Lz = b
    for k = 1:n-1
        for i = k+1:min(n, k + l - (k % l))
            b[i] -= A[i, k] * b[k]
        end
    end

    # Ux = z
    x = zeros(Float64, n)
    for i = n:-1:1
        sum_val = 0.0
        for j = i+1:min(n, l + i)
            sum_val += A[i, j] * x[j]
        end
        x[i] = (b[i] - sum_val) / A[i, i]
    end
    return x
end


"""
Solves system of linear equations Ax = b using LU decomposition with partial pivoting.

in:
    A::SparseMatrixCSC{Float64, Int64} - input square sparse matrix
    n::Int64 - size of matrix
    l::Int64 - size of inner matrices A_k, B_k, C_k
    b::Vector{Float64} - vector of values
out:
    x::Vector{Float64} - vector of solutions
"""
function solve_using_lu_pivot!(A::SparseMatrixCSC{Float64, Int64}, n::Int64, l::Int64, b::Vector{Float64})
    p = lu_pivot!(A, n, l)

    # Lz = Pb
    for i = 2:n
        bpi = b[p[i]]
        row_start = max(1, p[i] - ((p[i] - 1) % l) - 1)
        for j = row_start:i-1
            bpi -= A[p[i], j] * b[p[j]]
        end
        b[p[i]] = bpi
    end

    # Ux = z
    x = zeros(Float64, n)
    for i = n:-1:1
        sum = b[p[i]]
        row_end = min(n, 2 * l + i)
        for j = i+1:row_end
            sum -= A[p[i], j] * x[j]
        end
        x[i] = sum / A[p[i], i]
    end
    return x
end

"""
Calculates b = Ax for block matrix A and x = ones(n).

in:
    A::SparseMatrixCSC{Float64, Int64} - input square sparse matrix
    n::Int64 - size of matrix
    l::Int64 - size of inner matrices A_k, B_k, C_k
out:
    b::Vector{Float64} - vector of solutions
"""
function calculate_b(A::SparseMatrixCSC{Float64, Int64}, n::Int64, l::Int64)
    [sum(A[i, max(1, i - ((i - 1) % l) - 1):min(n, l + i)]) for i in 1:n]
end

end