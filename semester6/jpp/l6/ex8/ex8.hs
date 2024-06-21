prime_factors :: Integer -> [Integer]
prime_factors n = factor n 2
  where
    factor n d
        | n < 2           = []
        | n < d * d       = [n]
        | n `mod` d == 0  = d : factor (n `div` d) d
        | otherwise       = factor n (d + 1)


primes :: Integer -> [Integer]
primes n = [x | x <- [1..n], prime_factors x == [x]]

main :: IO ()
main = print $ primes 100
