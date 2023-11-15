# Jakub Ogrodowczyk

delta = Float64(2e-52)  # Macheps
i = Float64(1.0)    # Inkrementowana wartość
k = 1   # Współczynnik przez który mnożymy deltę
while i < 2.0
    i += delta
    if i != 1.0 + k * delta
        println("mismatch")
    end
    k += 1
end 