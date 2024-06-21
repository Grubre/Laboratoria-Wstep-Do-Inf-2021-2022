prime_factors :: Integer -> [Integer]
prime_factors n = factorize n 2
  where
    factorize n d
        | n < 2           = []
        | n < d * d       = [n]
        | n `mod` d == 0  = d : factorize (n `div` d) d
        | otherwise       = factorize n (d + 1)


primes :: Integer -> [Integer]
primes n = [x | x <- [1..n], prime_factors x == [x]]

main :: IO ()
main = print $ primes 100
