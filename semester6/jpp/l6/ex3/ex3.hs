merge_sort :: Ord a => [a] -> [a]
merge_sort [] = []
merge_sort [x] = [x]
merge_sort nums = merge_lists (merge_sort left) (merge_sort right)
  where
    merge_lists [] l = l
    merge_lists l [] = l
    merge_lists (a:rest_a) (b:rest_b)
        | a <= b        = a : merge_lists rest_a (b:rest_b)
        | otherwise     = b : merge_lists (a:rest_a) rest_b
    (left, right) = splitAt (length nums `div` 2) nums

main :: IO ()
main = print $ merge_sort [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
