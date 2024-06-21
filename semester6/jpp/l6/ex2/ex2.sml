fun pascalTriangle n =
    let
        fun nextRow [] = [1]
          | nextRow row = 
              let
                val shiftedRight = 0 :: row
                val shiftedLeft = row @ [0]
              in
                ListPair.map op+ (shiftedRight, shiftedLeft)
              end

        fun iterate f x 0 = x
          | iterate f x n = iterate f (f x) (n - 1)

    in
        iterate nextRow [1] n
    end

fun binomial2 n k =
    let
        val row = pascalTriangle n
    in
        List.nth(row, k)
    end

val _ = print (Int.toString (binomial2 5 3) ^ "\n")
