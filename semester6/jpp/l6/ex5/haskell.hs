prime_factors :: Integer -> [Integer]
prime_factors n = factorize n 2
  where
    factorize n d
        | n < 2           = []
        | n < d * d       = [n]
        | n `mod` d == 0  = d : factorize (n `div` d) d
        | otherwise       = factorize n (d + 1)

main :: IO ()
main = print $ prime_factors 17
