-- Int : typical 32 or 64 bit integer, bounded.
a = 3 :: Int

-- Integer : arbitrary precision type integer, like BigInt in JavaScript or BigInteger in Java.
b = 4 :: Integer

-- Float : 32-bit IEEE 754 real floating point number.
t = 3.14 :: Float

-- Double : 64-bit IEEE 754 real floating point number.
p = 3.141592653589793238462 :: Double

{-
    ":: Type" is Type Signature, which tells the type of a variable.
        - It is not always necessary, because usually GHC can infer proper types for values.
-}

-- Addition
3 + 4

-- Subtraction
7 - 2

-- Multiplication
3.14 * 7

-- Division
7 / 2.4

-- Integer division, remainder and modulo
quot 7 3   -- 2 (integer division quotient, rounds toward zero when negative)
rem 7 3    -- 1 (integer division remainder)

div 7 3    -- 2 (integer division quotient, rounds toward negative infinity when negative)
mod 7 3    -- 1 (modular arithmatic)

7 `mod` 3  -- 1 (also modular arithmatic)

{-
    There is no modulo operator ("%") predefined in Haskell.
    mod is a function that calculates modulo.

    Note that if a function is binary, (i.e. takes 2 arguments) then you can make it 'infix'
    by wrapping a function name with ` (backtick).

    If both 2 arguments are positive, then using quot and rem is preferable,
    because they are faster than div and mod.
-}

-- Negative Number

-3
5 * (-3)

{-
    'Unary -' is the only unary operater in Haskell, and it is a syntatic sugar for function 'negate'.
    Therefore (- 3) also means a negative integer -3. (not like (+ 3))
    It is a bit quirky, so we should use it with parentheses in the arithmatic expressions.
    (Writing '5 * -3' gives you a compile error!)
-}

-- Comparison
1 > 3    -- False
2 <= 4   -- True

compare 3 4    -- LT

{-
    compare is a function that takes two arguments which are the same type that can be ordered,
    and outputs the Ordering type, which consists of LT, GT and EQ.

    (i.e. compare :: (Ord a) => a -> a -> Ordering)
-}

-- Equality
1 == 2    -- False
3 /= 4    -- True

{-
    In Haskell, /= is the not equal operator.
-}
