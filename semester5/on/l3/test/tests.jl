#Author: Jakub Ogrodowczyk

using Test
include("../src/solvers.jl")
using .Solvers

@testset "Testy metody bisekcji" begin
    @testset "test rownania kwadratowego" begin
        f(x) = x^2 - 4
        a, b = 1.0, 5.0
        delta, epsilon = 1e-5, 1e-5
        r, v, it, err = Solvers.mbisekcji(f, a, b, delta, epsilon)
        
        @test err == 0
        @test r ≈ sqrt(4) atol=delta
        @test abs(v) < epsilon
        @test it > 0
    end

    @testset "test rownania trzeciego stopnia" begin
        f(x) = x^3 - 1
        a, b = 0.0, 2.0
        delta, epsilon = 1e-5, 1e-5
        r, v, it, err = Solvers.mbisekcji(f, a, b, delta, epsilon)

        @test err == 0
        @test r ≈ 1.0 atol=delta
        @test abs(v) < epsilon
        @test it > 0
    end

    @testset "test tego samego znaku" begin
        f(x) = x - 1
        a, b = 2.0, 4.0
        delta, epsilon = 1e-5, 1e-5
        r, v, it, err = Solvers.mbisekcji(f, a, b, delta, epsilon)
        
        @test err == 1
        @test r === Nothing
        @test v === Nothing
        @test it === Nothing
    end

    @testset "test sinusa" begin
        f(x) = sin(x)
        a, b = 3.0, 4.0
        delta, epsilon = 1e-5, 1e-5
        r, v, it, err = Solvers.mbisekcji(f, a, b, delta, epsilon)

        @test err == 0
        @test r ≈ π atol=delta
        @test abs(v) < epsilon
        @test it > 0
    end
end

@testset "Testy metody stycznych" begin
    @testset "Test funkcji kwadratowej" begin
        f(x) = x^2 - 4
        pf(x) = 2x
        x0 = 1.0
        delta, epsilon = 1e-5, 1e-5
        maxit = 100
        
        x, y, it, err = Solvers.mstycznych(f, pf, x0, delta, epsilon, maxit)
        
        @test err == 0
        @test x ≈ sqrt(4) atol=delta
        @test abs(y) < epsilon
        @test it > 0
    end

    @testset "Test dla funkcji sześciennej" begin
        f(x) = x^3 - 1
        pf(x) = 3x^2
        x0 = 0.5
        delta, epsilon = 1e-5, 1e-5
        maxit = 100
        
        x, y, it, err = Solvers.mstycznych(f, pf, x0, delta, epsilon, maxit)
        
        @test err == 0
        @test x ≈ 1.0 atol=delta
        @test abs(y) < epsilon
        @test it > 0
    end

    @testset "Test zbyt małej ilości iteracji" begin
        f(x) = cos(x)
        pf(x) = -sin(x)
        x0 = 1.0
        delta, epsilon = 1e-5, 1e-5
        maxit = 2
        
        x, y, it, err = Solvers.mstycznych(f, pf, x0, delta, epsilon, maxit)

        @test it == maxit
        @test err == 1
    end
end


@testset "Testy metody siecznych" begin

    @testset "Test dla funkcji kwadratowej" begin
        f(x) = x^2 - 4
        x0, x1 = 1.0, 2.0
        delta, epsilon = 1e-5, 1e-5
        maxit = 100

        x, y, it, err = Solvers.msiecznych(f, x0, x1, delta, epsilon, maxit)

        @test err == 0
        @test x ≈ sqrt(4) atol=delta
        @test abs(y) < epsilon
        @test it < maxit
    end

    @testset "Test dla funkcji sześciennej" begin
        f(x) = x^3 - 1
        x0, x1 = 0.0, 2.0
        delta, epsilon = 1e-5, 1e-5
        maxit = 100

        x, y, it, err = Solvers.msiecznych(f, x0, x1, delta, epsilon, maxit)

        @test err == 0
        @test x ≈ 1.0 atol=delta
        @test abs(y) < epsilon
        @test it < maxit
    end

    @testset "Test zbyt małej ilości iteracji" begin
        f(x) = cos(x) - x
        x0, x1 = 0.0, 1.0
        delta, epsilon = 1e-5, 1e-5
        maxit = 2

        x, y, it, err = Solvers.msiecznych(f, x0, x1, delta, epsilon, maxit)

        @test it == maxit
        @test err == 1
    end

end