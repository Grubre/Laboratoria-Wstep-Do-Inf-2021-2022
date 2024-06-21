mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort nums = merge_lists (mergeSort left) (mergeSort right)
  where
    merge_lists [] l = l
    merge_lists l [] = l
    merge_lists (a:rest_a) (b:rest_b)
        | a <= b        = a : merge_lists rest_a (b:rest_b)
        | otherwise     = b : merge_lists (a:rest_a) rest_b
    (left, right) = splitAt (length nums `div` 2) nums

main :: IO ()
main = print $ mergeSort [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
