{-
    [Leetcode] 46. Permutations
    https://leetcode.com/problems/permutations/
-}

import Data.List

permute :: (Eq a) => [a] -> Int -> [[a]]
permute _ 0 = [[]]
permute xs k =
    let attachSelected = (\x -> map (x:) $ permute (delete x xs) (k-1))
    in concat $ map attachSelected xs

solution :: (Eq a) => [a] -> [[a]]
solution xs = 
    let n = length xs
    in permute xs n

{-
    If the length of xs is n, then 'permute xs k' produces P(n, k) permutations on xs. (k-permutations of n)
    I assumed n >= k >= 0 for permute function.
-}