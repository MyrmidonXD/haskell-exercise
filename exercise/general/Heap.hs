{-
    Leftist Heap (https://en.wikipedia.org/wiki/Leftist_tree)
    Implementation as Min-Heap
-}

import Data.Maybe

class Heap h where
    insert :: (Ord a) => a -> h a -> h a
    deleteMin :: (Ord a) => h a -> Maybe (a, h a)
    peek :: h a -> Maybe a

data LeftistHeap a 
    = Empty
    | Node a Int (LeftistHeap a) (LeftistHeap a)
    deriving (Show)

mergeLeftistHeap :: (Ord a) => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
mergeLeftistHeap h1 Empty = h1
mergeLeftistHeap Empty h2 = h2
mergeLeftistHeap h1@(Node v1 _ _ _) h2@(Node v2 _ _ _)
    | v1 <= v2  = mergeInner h1 h2
    | otherwise = mergeInner h2 h1
  where
    mergeInner :: (Ord a) => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
    mergeInner (Node v _ l r) hLarge
        | sl < sr   = Node v (1 + sl) mergedRight l
        | otherwise = Node v (1 + sr) l mergedRight
      where
        getS :: LeftistHeap a -> Int
        getS Empty          = 0
        getS (Node _ s _ _) = s

        mergedRight = mergeLeftistHeap r hLarge

        sl = getS l
        sr = getS mergedRight

instance Heap LeftistHeap where
    insert x h = mergeLeftistHeap (Node x 0 Empty Empty) h

    deleteMin Empty          = Nothing
    deleteMin (Node x _ l r) = Just (x, mergeLeftistHeap l r)

    peek Empty          = Nothing
    peek (Node x _ _ _) = Just x

-- Example codes below
x -: f = f x    -- Defines custom 'pipe' operator

exampleHeap = Empty -: insert 3 -: insert 5 -: insert 1 -: insert 2     -- Inserts [3, 5, 1, 2] sequentially

{-
    Now exampleHeap looks like this:
                                      1
                                    /   \
                                   3     2
                                 /
                                5
    ... and you can see this using 'show exampleHeap'

    > show exampleHeap
    "Node 1 1 (Node 3 1 (Node 5 0 Empty Empty) Empty) (Node 2 0 Empty Empty)"
-}

-- Aux function for chaining deleteMin using monad bind (>>=)
deleteMinChainable (_, h) = deleteMin h

first  = return ((), exampleHeap) >>= deleteMinChainable
-- first:  Just (1, Node 2 1 (Node 3 1 (Node 5 0 Empty Empty) Empty) Empty)

second = return ((), exampleHeap) >>= deleteMinChainable >>= deleteMinChainable
-- second: Just (2, Node 3 1 (Node 5 0 Empty Empty) Empty)

third  = return ((), exampleHeap) >>= deleteMinChainable >>= deleteMinChainable >>= deleteMinChainable
-- third:  Just (3, Node 5 0 Empty Empty)

fourth = return ((), exampleHeap) >>= deleteMinChainable >>= deleteMinChainable >>= deleteMinChainable >>= deleteMinChainable
-- fourth: Just (5, Empty)

fifth  = return ((), exampleHeap) >>= deleteMinChainable >>= deleteMinChainable >>= deleteMinChainable >>= deleteMinChainable >>= deleteMinChainable
-- fifth:  Nothing

{-
 - Heap Sort Example using Leftist Heap
 -}

unsortedList = [3, 7, 19, 2, 5, 8, 4, 7, 1, 9, 12]

heapSort :: (Ord a) => [a] -> [a]
heapSort l = 
    let heap = foldr insert Empty l
        deleteMinAcc _ (lPrev, hPrev) = case deleteMin hPrev of
            Just (x, h) -> (x:lPrev, hPrev)
            Nothing     -> (lPrev, hPrev)
    in reverse $ fst $ foldr deleteMinAcc ([], heap) l

sortedList = heapSort unsortedList
-- sortedList: [1,2,3,4,5,7,7,8,9,12,19]
