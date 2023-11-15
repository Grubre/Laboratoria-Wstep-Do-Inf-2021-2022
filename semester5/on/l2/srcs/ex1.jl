# Jakub Ogrodowczyk
function w_przod(T, x::Vector, y::Vector)
    if length(x) != length(y)
        error("Vectory musza byc tego samego wymiaru.")
    end
    S = 0
    for i in 1:length(x)
        S = S + T(x[i]) * T(y[i])
    end
    return S
end

function w_tyl(T, x::Vector, y::Vector)
    if length(x) != length(y)
        error("Vectory musza byc tego samego wymiaru.")
    end
    S = 0
    for i in length(x):-1:1
        S = S + T(x[i]) * T(y[i])
    end
    return S
end

function od_najw_do_najm(T, x::Vector, y::Vector)
    m = []
    for i in 1:length(x)
        push!(m, T(x[i]) * T(y[i]))
    end
    m = sort(m, rev=true)
    S = 0
    for i in 1:length(m)
        S = S + m[i]
    end
    return S
end

function od_najm_do_najw(T, x::Vector, y::Vector)
    m = []
    for i in 1:length(x)
        push!(m, T(x[i]) * T(y[i]))
    end
    m = sort(m)
    S = 0
    for i in 1:length(m)
        S = S + m[i]
    end
    return S
end

actual32 = Float32(1.00657107000000e-11)
actual64 = Float64(1.00657107000000e-11)

x1 = [2.718281828, -3.141592654, 1.414213562, 0.5772156649, 0.3010299957]
y1 = [1486.2497, 878366.9879, -22.37492, 4773714.647, 0.000185049]
x2 = [2.718281828, -3.141592654, 1.414213562, 0.577215664, 0.301029995]
y2 = [1486.2497, 878366.9879, -22.37492, 4773714.647, 0.000185049]

function myprint(f::Function, x::Vector, y::Vector)
    f32 = f(Float32, x,y)
    f64 = f(Float64, x,y)
    println("           Float32: ", f32)
    println("           blad32:  ", abs(actual32 - f32))
    println("           Float64: ", f64)
    println("           Blad64:  ", abs(actual64 - f64))
end

println("blad32: ", actual32)
println("blad64: ", actual64)

printstyled("w przod:\n"; color=:red)
println("   poprzednie wartosci:")
myprint(w_przod, x1, y1)
println("   nowe wartosci:")
myprint(w_przod, x2, y2)
printstyled("w tyl:\n"; color=:red)
println("   poprzednie wartosci:")
myprint(w_tyl, x1, y1)
println("   nowe wartosci:")
myprint(w_tyl, x2, y2)
printstyled("od najw do najm:\n"; color=:red)
println("   poprzednie wartosci:")
myprint(od_najw_do_najm, x1, y1)
println("   nowe wartosci:")
myprint(od_najw_do_najm, x2, y2)
printstyled("od najm do najw:\n"; color=:red)
println("   poprzednie wartosci:")
myprint(od_najm_do_najw, x1, y1)
println("   nowe wartosci:")
myprint(od_najm_do_najw, x2, y2)


function print_csv(function_name::String, f::Function)
    wynik = f(Float64, x2,y2)
    wynik_x1 = f(Float64, x1,y1)
    wynik_diff = abs(wynik - wynik_x1)
    blad = abs(actual64 - wynik)
    blad_x1 = abs(actual64 - wynik_x1)
    blad_diff = abs(blad - blad_x1)
    println("\$x_2y\$ & $function_name & $wynik & $blad \\\\")
    println("\\hline  % Add space between rows")
    println("\\multicolumn{2}{|c|}{\\textbf{Różnica bezwzględna}} & $wynik_diff & $blad_diff \\\\")
    println("\\hline  % Add space between rows")
end

print_csv("w przód", w_przod)
print_csv("w tył", w_tyl)
print_csv("od największego do najmniejszego", od_najw_do_najm)
print_csv("od najmniejszego do największego", od_najm_do_najw)