fun merge_sort [] = []
  | merge_sort [x] = [x]
  | merge_sort nums =
    let
        fun merge_lists ([], l) = l
          | merge_lists (l, []) = l
          | merge_lists (a::rest_a, b::rest_b) =
              if a <= b then a :: merge_lists (rest_a, b::rest_b)
              else b :: merge_lists (a::rest_a, rest_b)

        val half = (List.length nums) div 2
        val left = List.take (nums, half)
        val right = List.drop (nums, half)
    in
        merge_lists (merge_sort left, merge_sort right)
    end

fun listToString [] = "[]"
  | listToString [x] = "[" ^ Int.toString x ^ "]"
  | listToString (x::xs) = "[" ^ Int.toString x ^ List.foldr (fn (y, acc) => ", " ^ Int.toString y ^ acc) "]" xs

val _ = 
    let
        val list = [5, 3, 8, 1, 2, 9, 4, 7, 6]
    in
        print ("Przed: " ^ listToString list ^ "\n");
        print ("Po: " ^ listToString (merge_sort list) ^ "\n\n")
    end

