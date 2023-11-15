# Jakub Ogrodowczyk

# Args:
# `T::Type`: typ liczby zmiennoprzecinkowej dla jakiego szukamy macheps
#
# Return:
# `macheps_T::T`: Macheps dla arytmetyki typu T
function macheps(T::Type)
    macheps_T = T(1.0) # zmienna w ktorej przechowujemy obliczony macheps

    while T(1.0) + (macheps_T / T(2.0)) > T(1.0)
        macheps_T = macheps_T / T(2.0)
    end

    return macheps_T
end

# Args:
# `T::Type`: typ liczby zmiennoprzecinkowej dla jakiego szukamy eta
#
# Return:
# `eta_T::T`: Macheps dla arytmetyki typu T
function eta(T)
    eta_T = T(1.0)

    while T(0.0) + (eta_T / T(2.0)) > T(0.0)
        eta_T = eta_T / T(2.0)

    end
    return eta_T
end

# Args:
# `T::Type`: typ liczby zmiennoprzecinkowej dla jakiego szukamy max
#
# Return:
# `max_T::T`: Macheps dla arytmetyki typu T
function max_val(T)
    max_T = T(1/2) # Zmienna w której przechowujemy nasz max

    # Najpierw wypełniamy mantyse 1
    while T(1/2) - (max_T / T(2.0)) < T(1/2)
        max_T = max_T / T(2.0)
    end
    max_T = T(1/2) - max_T

    # A potem wypełniamy wykładnik poza najmłodszym bitem
    while !isinf(max_T * T(2.0))
        max_T = max_T * T(2.0)
    end

    return max_T
end

# Pomocnicza funkcja do wypisywania wynikow
function print_macheps(T)
    println(T)
    println("my function -> ", macheps(T))
    println("julia eps ---> ", eps(T))
end
# Pomocnicza funkcja do wypisywania wynikow
function print_eta(T)
    println(T)
    println("my function -> ", eta(T))
    println("julia eta ---> ", nextfloat(T(0.0)))
end
# Pomocnicza funkcja do wypisywania wynikow
function print_max(T)
    println(T)
    println("my function -> ", max_val(T))
    println("julia max ---> ", floatmax(T))
end

print_macheps(Float16)
print_macheps(Float32)
print_macheps(Float64)
print_eta(Float16)
print_eta(Float32)
print_eta(Float64)
print_max(Float16)
print_max(Float32)
print_max(Float64)