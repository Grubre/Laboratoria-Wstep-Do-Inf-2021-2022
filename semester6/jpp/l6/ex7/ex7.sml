fun prime_factors n =
    let
        fun factorize n d =
            if n < 2 then
                []
            else if n < d * d then
                [n]
            else if n mod d = 0 then
                d :: factorize (n div d) d
            else
                factorize n (d + 1)
    in
        factorize n 2
    end

fun totient2 n =
    let
      fun f [a] = a - 1
          | f (a1::a2::a) =
              if a1 = a2 then
                  a1 * f (a2::a)
              else
                  (a1 - 1) * f (a2::a)
    in
    if n <= 1 then 0 else f (prime_factors n)
    end

val _ = print (Int.toString (totient2 9) ^ "\n")
