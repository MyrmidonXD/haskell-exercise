{-
    Sieve of Eratosthenes Implementation using Wheel
    Based on the paper of Melissa E. O'Neill, "The Genuine Sieve Of Eratosthenes"
    (https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf)
-}

module Prime ( primes )
    where

import Data.Maybe
import qualified Heap as H

newtype KeyValuePair a b = KeyValuePair (a, b)
    deriving (Show)

instance (Eq a) => Eq (KeyValuePair a b) where
    KeyValuePair (k1, _) == KeyValuePair (k2, _) = k1 == k2

instance (Ord a) => Ord (KeyValuePair a b) where
    KeyValuePair (k1, _) `compare` KeyValuePair (k2, _) = k1 `compare` k2

getMinKey :: (Ord a) => H.LeftistHeap (KeyValuePair a b) -> Maybe a
getMinKey H.Empty = Nothing
getMinKey heap    = do
    KeyValuePair (key, _) <- H.peek heap
    return key

sieve :: [Int] -> [Int]
sieve []     = []
sieve (x:xs) = x : sieve' xs (insertPrime x xs H.Empty)
  where
    insertPrime :: Int -> [Int] -> H.LeftistHeap (KeyValuePair Int [Int]) -> H.LeftistHeap (KeyValuePair Int [Int])
    insertPrime p xs composites = H.insert (KeyValuePair (p * p, map (* p) xs)) composites

    sieve' :: [Int] -> H.LeftistHeap (KeyValuePair Int [Int]) -> [Int]
    sieve' []     composites = []
    sieve' (x:xs) composites
        | nextComposite == Nothing = []
        | (Just x) < nextComposite = x : sieve' xs (insertPrime x xs composites)
        | otherwise                = sieve' xs (updateComposites composites)
          where
            nextComposite = getMinKey composites

            -- Returns H.Empty when H.deleteMin or H.peek fails
            updateComposites :: H.LeftistHeap (KeyValuePair Int [Int]) -> H.LeftistHeap (KeyValuePair Int [Int])
            updateComposites composites
                | minComposite == Nothing  = H.Empty
                | minComposite <= (Just x) = case proceedMinComposite composites of
                    Just updated -> updateComposites updated
                    Nothing      -> H.Empty
                | otherwise                = composites
                  where
                    minComposite = getMinKey composites

                    proceedMinComposite :: H.LeftistHeap (KeyValuePair Int [Int]) -> Maybe (H.LeftistHeap (KeyValuePair Int [Int]))
                    proceedMinComposite composites = do
                        (KeyValuePair (_, next:nexts), otherComposites) <- H.deleteMin composites
                        return $ H.insert (KeyValuePair (next, nexts)) otherComposites

wheel2357 :: [Int]
wheel2357 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:
    2:4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel2357

spinWheel :: [Int] -> Int -> [Int]
spinWheel (x:xs) n = n : spinWheel xs (n + x)

primes :: [Int]
primes = 2 : 3 : 5 : 7 : sieve (spinWheel wheel2357 11)
