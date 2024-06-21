prime_factors :: Integer -> [Integer]
prime_factors n = factorize n 2
  where
    factorize n d
        | n < 2           = []
        | n < d * d       = [n]
        | n `mod` d == 0  = d : factorize (n `div` d) d
        | otherwise       = factorize n (d + 1)

totient2 :: Integer -> Integer
totient2 n
    | n <= 1    = 0
    | otherwise = f (prime_factors n)
  where
    f [a] = a - 1
    f (a1 : a2 : a)
        | a1 == a2  = a1 * f (a2 : a)
        | otherwise = (a1 - 1) * f (a2 : a)

main :: IO ()
main = print $ totient2 9
