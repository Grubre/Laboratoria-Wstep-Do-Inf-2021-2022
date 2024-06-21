prime_factors :: Integer -> [Integer]
prime_factors n = factor n 2
  where
    factor n d
        | n < 2           = []
        | n < d * d       = [n]
        | n `mod` d == 0  = d : factor (n `div` d) d
        | otherwise       = factor n (d + 1)

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
