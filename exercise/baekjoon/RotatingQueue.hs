{-
    [Baekjoon Online Judge] 1021. Rotating Queue
    https://www.acmicpc.net/problem/1021
-}

import Data.Function
import Data.List

type Queue = [Int]

solution :: Int -> [Int] -> Int
solution n seq = fst $ foldl' proceed (0, [1 .. n]) seq
  where
    proceed (sum, q) e = (sum + minDist, poppedQ)
      where
        (minDist, poppedQ) = findMinDistAndPop e q

findMinDistAndPop :: Int -> Queue -> (Int, Queue)
findMinDistAndPop e q = (minDist, (tail right) ++ left)
  where
    (left, right) = span (/= e) q
    minDist       = (min `on` length) left right

main = interact
    $ show 
    . uncurry solution 
    . (\(n:_:rest) -> (n,rest)) 
    . map read 
    . words

