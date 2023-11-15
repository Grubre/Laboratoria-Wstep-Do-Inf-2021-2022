# Jakub Ogrodowczyk

function f(x)
    return sqrt(x * x + 1) - 1
end

function g(x)
    return (x * x) / (sqrt(x * x + 1) + 1)
end

for i in 1:50
    x = 8.0^-i
    f_x = f(x)
    g_x = g(x)
    println("x <- 8^-$i:")
    println("f(x) = $f_x")
    println("g(x) = $g_x")
end