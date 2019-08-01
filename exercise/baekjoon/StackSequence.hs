{-
    [Baekjoon Online Judge] 1874. Stack Sequence
    https://www.acmicpc.net/problem/1874
-}

import Data.Maybe
import Data.List
import Data.Char

type Stack = [Int]

peek :: Stack -> Int
peek = head

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> Stack
push = (:)

data SolveState = SolveState 
    { numbers :: [Int]
    , inputSeq :: [Int]
    , stack :: Stack
    } deriving (Show)

pushNumber :: SolveState -> SolveState
pushNumber s = s
    { numbers = tail $ numbers s
    , stack = push (head $ numbers s) (stack s)
    }

popNumber :: SolveState ->  SolveState
popNumber s = 
    let (_, newStack) = pop $ stack s
    in s {stack = newStack, inputSeq = tail $ inputSeq s}

isPop :: SolveState -> Bool
isPop (SolveState _ (x:restSeq) (t:st)) = x == t
isPop _                                 = False

isPush :: SolveState -> Bool
isPush (SolveState (n:nextNum) (x:restSeq) (t:st)) = n <= x && t < x
isPush (SolveState (n:nextNum) (x:restSeq) [])     = n <= x
isPush _                                           = False

isDone :: SolveState -> Bool
isDone (SolveState [] [] []) = True
isDone _                     = False

solution :: Int -> [Int] -> [String]
solution n targetSeq = case solve initialSolveState [] of
    Nothing        -> ["NO"]
    Just resultSeq -> reverse resultSeq
  where
    initialSolveState = SolveState 
        { numbers  = [1 .. n]
        , inputSeq = targetSeq
        , stack    = []
        }
    solve :: SolveState -> [String] -> Maybe [String]
    solve s resultSeq
        | isDone s  = Just resultSeq
        | isPush s  = solve (pushNumber s) ("+":resultSeq)
        | isPop  s  = solve (popNumber s) ("-":resultSeq)
        | otherwise = Nothing

main = interact $ unlines 
                . uncurry solution
                . fromMaybe (0, [])
                . uncons
                . map read
                . lines
 
