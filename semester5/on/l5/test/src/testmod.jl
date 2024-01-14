include("blocksys.jl")
using .blocksys
using Test

data_path = "../dane/"

list = []
data_folder = readdir(data_path)
for dir in data_folder
    a_file = data_path * dir * "/A.txt"
    b_file = data_path * dir * "/b.txt"

    A, n, l = sparse_mat_from_file(a_file)
    b = vec_from_file(b_file)

    push!(list, (A, n, l, b))
end

@testset "Calculate B" begin
    for (A, n, l, b) in copy(list)
        println("Testing A of size $n")
        b2 = calculate_b(A, n, l)
        @test b2 ≈ b
    end
end

@testset "Gauss elimination" begin
    for (A, n, l, b) in copy(list)
        println("Testing A of size $n")
        x = gaussian_elimination(A, n, l, b)
        x_ones = ones(n)
        @test x ≈ x_ones
        @test A * x ≈ b
    end
end

@testset "Gauss elimination with pivot" begin
    for (A, n, l, b) in copy(list)
        println("Testing A of size $n")
        x = gaussian_elimination_pivot(A, n, l, b)
        x_ones = ones(n)
        @test x ≈ x_ones
        @test A * x ≈ b
    end
end

@testset "LU decomposition" begin
    for (A, n, l, b) in copy(list)
        println("Testing A of size $n")
        x = solve_using_lu!(A, n, l, b)
        x_ones = ones(n)
        @test x ≈ x_ones
        @test A * x ≈ b
    end
end

@testset "LU decomposition with pivot" begin
    for (A, n, l, b) in copy(list)
        println("Testing A of size $n")
        x = solve_using_lu_pivot!(A, n, l, b)
        x_ones = ones(n)
        @test x ≈ x_ones
        @test A * x ≈ b
    end
end