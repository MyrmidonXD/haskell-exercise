{-
    [Baekjoon Online Judge] 1011. Fly me to the Alpha Centauri
    https://www.acmicpc.net/problem/1011
-}

import Control.Monad

getMinWarp :: Int -> Int -> Int
getMinWarp x y
    | diff <= (k - 1) * k = (2 * k) - 2
    | otherwise           = (2 * k) - 1
  where
    diff = y - x
    k = ceiling $ sqrt $ fromIntegral diff

main = do
    inputCount <- fmap read getLine
    replicateM_ inputCount solution
  where
    solution = do
        (input1, _:input2) <- fmap (span (/= ' ')) getLine
        let [x, y] = map read [input1, input2]
        putStrLn $ show $ getMinWarp x y
