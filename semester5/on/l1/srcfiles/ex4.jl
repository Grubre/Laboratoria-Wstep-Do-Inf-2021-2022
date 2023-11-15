# Jakub Ogrodowczyk

i = 1.0     # Inkrementowana wartość
while i <= 2.0
    if i * (1/i) != 1
        break
    end
    i = nextfloat(i)
end
println("i = $i")