de :: Integer -> Integer -> (Integer, Integer, Integer)
de a b
    | b == 0    = (1, 0, a)
    | otherwise = (t, s - q * t, g)
  where
    (q, r) = a `divMod` b
    (s, t, g) = de b r

main :: IO ()
main = print $ de 56 15
