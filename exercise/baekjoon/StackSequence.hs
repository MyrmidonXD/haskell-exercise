import Data.Maybe

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

popNumber :: SolveState -> (Int, SolveState)
popNumber s = 
    let (x, newStack) = pop $ stack s
    in (x, s {stack = newStack})

isPop :: SolveState -> Bool
isPop (SolveState _ x:restSeq t:st) = x == t
isPop _                             = False

isPush :: SolveState -> Bool
isPush (SolveState n:nextNum x:restSeq t:st) = n <= x && t < x
isPush (SolveState n:nextNum x:restSeq [])   = n <= x
isPush _                                     = False

isDone :: SolveState -> Bool
isDone (SolveState [] [] []) = True
isDone _                     = False

{-
isValid :: SolveState -> Bool
isValid (SolveState _ [] _)                   = True
isValid (SolveState [] _ [])                  = False
isValid (SolveState [] seq st)                = seq == st
isValid (SolveState n:nextNum x:restSeq [])   = n <= x
isValid (SolveState n:nextNum x:restSeq t:st)
    | x == t = True
    | x > t && n <= x = True
    | otherwise = False


solution :: Int -> [Int] -> [String]
solution n targetSeq = case solve initialSolveState [] of
    Nothing        = ["NO"]
    Just resultSeq = resultSeq
  where
    initialSolveState = SolveState 
        { numbers  = [1 .. n]
        , inputSeq = targetSeq
        , stack    = []
        }
    solve :: SolveState -> [String] -> Maybe [String]
    solve (SolveState {inputSeq = []}) result       = Just result
    solve (SolveState {numbers = [], stack = []}) _ = Nothing
    solve (SolveState {inputSeq = (i:is)}) _        = Nothing -- WIP
    solve _ _ = Nothing -- Temporary base case
-}
