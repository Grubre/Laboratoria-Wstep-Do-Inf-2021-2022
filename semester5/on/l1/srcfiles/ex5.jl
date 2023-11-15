# Jakub Ogrodowczyk

# Wektory na których operujemy
x = [2.718281828, -3.141592654, 1.414213562, 0.5772156649, 0.3010299957]
y = [1486.2497, 878366.9879, -22.37492, 4773714.647, 0.000185049]

# Args:
# `T::Type`: typ liczby zmiennoprzecinkowej dla jakiego szukamy macheps
# `x::Vector`: lewa strona iloczynu skalarnego
# `y::Vector`: prawa strona iloczynu skalarnego
#
# Return:
# `S::T`: iloczyn skalarny dodając od przodu
function w_przod(T, x::Vector, y::Vector)
    if length(x) != length(y)
        error("Vectory musza byc tego samego wymiaru.")
    end
    S = T(0.0) # Wynik iloczynu skalarnego
    for i in 1:length(x)
        S = S + T(x[i]) * T(y[i])
    end
    return S
end

# Args:
# `T::Type`: typ liczby zmiennoprzecinkowej dla jakiego szukamy macheps
# `x::Vector`: lewa strona iloczynu skalarnego
# `y::Vector`: prawa strona iloczynu skalarnego
#
# Return:
# `S::T`: iloczyn skalarny dodając od tyłu
function w_tyl(T, x::Vector, y::Vector)
    if length(x) != length(y)
        error("Vectory musza byc tego samego wymiaru.")
    end
    S = T(0.0) # Wynik iloczynu skalarnego
    for i in length(x):-1:1
        S = S + T(x[i]) * T(y[i])
    end
    return S
end

# Args:
# `T::Type`: typ liczby zmiennoprzecinkowej dla jakiego szukamy macheps
# `x::Vector`: lewa strona iloczynu skalarnego
# `y::Vector`: prawa strona iloczynu skalarnego
#
# Return:
# `S::T`: iloczyn skalarny dodając od największego do najmniejszego
function od_najw_do_najm(T, x::Vector, y::Vector)
    m = [] # Wektor do którego dodajemy iloczyny kolejnych składowych
    for i in 1:length(x)
        push!(m, T(x[i]) * T(y[i]))
    end
    m = sort(m, rev=true)
    S = T(0.0) # Wynik iloczynu skalarnego
    for i in 1:length(m)
        S = S + m[i]
    end
    return S
end

# Args:
# `T::Type`: typ liczby zmiennoprzecinkowej dla jakiego szukamy macheps
# `x::Vector`: lewa strona iloczynu skalarnego
# `y::Vector`: prawa strona iloczynu skalarnego
#
# Return:
# `S::T`: iloczyn skalarny dodając od najmniejszego do największego
function od_najm_do_najw(T, x::Vector, y::Vector)
    m = [] # Wektor do którego dodajemy iloczyny kolejnych składowych
    for i in 1:length(x)
        push!(m, T(x[i]) * T(y[i]))
    end
    m = sort(m)
    S = T(0.0) # Wynik iloczynu skalarnego
    for i in 1:length(m)
        S = S + m[i]
    end
    return S
end


actual32 = Float32(1.00657107000000e-11)
actual64 = Float64(1.00657107000000e-11)
println("blad32: ", actual32)
println("blad64: ", actual64)
println("w przod:")
f32 = w_przod(Float32, x,y)
f64 = w_przod(Float64, x,y)
println("Float32: ", f32)
println("Float64: ", f64)
println("blad32:  ", abs(actual32 - f32))
println("Blad64:  ", abs(actual64 - f64))
println("w tyl:")
f32 = w_tyl(Float32, x,y)
f64 = w_tyl(Float64, x,y)
println("Float32: ", f32)
println("Float64: ", f64)
println("blad32:  ", abs(actual32 - f32))
println("Blad64:  ", abs(actual64 - f64))
println("od_najw_do_najm:")
f32 = od_najw_do_najm(Float32, x,y)
f64 = od_najw_do_najm(Float64, x,y)
println("Float32: ", f32)
println("Float64: ", f64)
println("blad32:  ", abs(actual32 - f32))
println("Blad64:  ", abs(actual64 - f64))
println("od_najm_do_najw:")
f32 = od_najm_do_najw(Float32, x,y)
f64 = od_najm_do_najw(Float64, x,y)
println("Float32: ", f32)
println("Float64: ", f64)
println("blad32:  ", abs(actual32 - f32))
println("Blad64:  ", abs(actual64 - f64))