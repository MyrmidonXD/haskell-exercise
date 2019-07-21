{-
    [Baekjoon Online Judge] 9020. Goldbach Conjecture
    https://www.acmicpc.net/problem/9020

    Note that this code uses the module 'Prime', in the ../general/Prime.hs.
    To run this, you should give '-i<Dir>' option (search path) to GHC/GHCi
-}

import Control.Monad
import qualified Prime as P

getPartition :: Int -> (Int, Int)
getPartition n = getPartition' n smallerPrimes largerPrimes
  where
    halfN = div n 2
    targetPrimes = takeWhile (< n) P.primes

    smallerPrimes = reverse $ filter (<= halfN) targetPrimes
    largerPrimes  = filter (>= halfN) targetPrimes

    getPartition' :: Int -> [Int] -> [Int] -> (Int, Int)
    getPartition' n s@(sp:sps) l@(lp:lps)
        | sp + lp < n = getPartition' n s lps
        | sp + lp > n = getPartition' n sps l
        | otherwise   = (sp, lp)

main = do
    line <- getLine
    let inputCount = read line
    replicateM_ inputCount solution
  where
    solution = do
        caseLine <- getLine
        let n = read caseLine
        let (p1, p2) = getPartition n
        putStrLn $ (show p1) ++ " " ++ (show p2)
