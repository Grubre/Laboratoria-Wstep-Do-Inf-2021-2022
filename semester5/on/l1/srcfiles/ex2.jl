# Jakub Ogrodowczyk

# Args:
# `T::Type`: typ liczby zmiennoprzecinkowej dla jakiego szukamy macheps
#
# Return:
# `T`: Macheps dla arytmetyki typu T
function macheps(T)
    three = T(3.0)
    four = T(4.0)
    one = T(1.0)
    return three * (four / three - one) - one
end

# Pomocnicza funkcja do wypisywania wynikow
function print_macheps(T)
    println(T)
    println("my function -> ", macheps(T))
    println("julia eps ---> ", eps(T))
end

print_macheps(Float16)
print_macheps(Float32)
print_macheps(Float64)