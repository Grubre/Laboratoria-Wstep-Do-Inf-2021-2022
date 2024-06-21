fun binomial n k =
    if k = 0 orelse k = n then 1
    else if k > n then 0
    else binomial (n - 1) k + binomial (n - 1) (k - 1)

val result = binomial 5 3
val () = print (Int.toString result ^ "\n")
