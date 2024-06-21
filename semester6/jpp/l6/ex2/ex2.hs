binomial2 :: Integer -> Integer -> Integer
binomial2 n k = pascal !! fromIntegral n !! fromIntegral k
  where
    pascal = iterate nextRow [1]
    nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])

main :: IO ()
main = print $ binomial2 5 2
