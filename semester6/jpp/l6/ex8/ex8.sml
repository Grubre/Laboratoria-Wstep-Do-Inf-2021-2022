fun prime_factors n =
    let
        fun factor n d =
            if n < 2 then
                []
            else if n < d * d then
                [n]
            else if n mod d = 0 then
                d :: factor (n div d) d
            else
                factor n (d + 1)
    in
        factor n 2
    end

fun primes n =
    let
        fun is_prime n = (prime_factors n = [n])
        fun collect_primes 0 acc = acc
          | collect_primes x acc = 
                if is_prime x then collect_primes (x - 1) (x :: acc)
                else collect_primes (x - 1) acc
    in
        collect_primes n []
    end

fun listToString [] = "[]"
  | listToString [x] = "[" ^ Int.toString x ^ "]"
  | listToString (x::xs) = "[" ^ Int.toString x ^ List.foldr (fn (y, acc) => ", " ^ Int.toString y ^ acc) "]" xs

val _ = print (listToString (primes 100) ^ "\n")
