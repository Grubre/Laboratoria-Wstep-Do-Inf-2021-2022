binomial :: Integer -> Integer -> Integer
binomial n k
    | k == 0 = 1
    | k == n = 1
    | k > n = 0
    | otherwise = binomial (n - 1) k + binomial (n - 1) (k - 1)

main :: IO ()
main = print $ binomial 5 3
