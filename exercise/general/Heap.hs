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
mergeLeftistHeap t1 Empty = t1
mergeLeftistHeap Empty t2 = t2
mergeLeftistHeap t1@(Node v1 _ _ _) t2@(Node v2 _ _ _)
    | v1 <= v2  = mergeInner t1 t2
    | otherwise = mergeInner t2 t1
  where
    mergeInner :: (Ord a) => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
    mergeInner (Node v _ l r) tLarge
        | sl <= sr  = Node v (1 + sl) mergedRight l
        | otherwise = Node v (1 + sr) l mergedRight
      where
        getS :: LeftistHeap a -> Int
        getS Empty          = 0
        getS (Node _ s _ _) = s

        mergedRight = mergeLeftistHeap r tLarge

        sl = getS l
        sr = getS mergedRight

instance Heap LeftistHeap where
    insert x t = mergeLeftistHeap (Node x 0 Empty Empty) t

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
