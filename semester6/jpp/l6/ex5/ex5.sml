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


fun listToString [] = "[]"
  | listToString [x] = "[" ^ Int.toString x ^ "]"
  | listToString (x::xs) = "[" ^ Int.toString x ^ List.foldr (fn (y, acc) => ", " ^ Int.toString y ^ acc) "]" xs

val _ = print (listToString (prime_factors 27) ^ "\n")
