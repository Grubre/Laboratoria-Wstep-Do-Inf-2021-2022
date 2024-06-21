fun gcd (a, 0) = a
  | gcd (a, b) = gcd (b, a mod b)

fun totient n =
    let
        fun is_coprime x = (gcd (n, x) = 1)
        fun count_coprimes 0 count = count
          | count_coprimes x count = count_coprimes (x - 1) (if is_coprime x then count + 1 else count)
    in
        count_coprimes n 0
    end

val _ = print (Int.toString (totient 9) ^ "\n")
