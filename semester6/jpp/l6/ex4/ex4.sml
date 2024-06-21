fun de a b =
    if b = 0 then
        (1, 0, a)
    else
        let
            val q = a div b
            val r = a mod b
            val (s, t, g) = de b r
        in
            (t, s - q * t, g)
        end

val _ = let
  val (x, y, z) = de 5 3
        in
          print (Int.toString x ^ " " ^ Int.toString y ^ " " ^ Int.toString z ^ "\n")
        end
