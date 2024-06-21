totient :: Integer -> Integer
totient n = fromIntegral (length [x | x <- [1..n], gcd n x == 1])

main :: IO ()
main = print $ totient 9
