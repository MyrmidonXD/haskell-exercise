-- Prints "Hello World!"
main = putStrLn "Hello World!"

{-
    This is multi-line comment: {- ... -}

    How to compile: ghc -o HelloWorld HelloWorld.hs
    How to directly run a code: runhaskell HelloWorld.hs
    How to load this code into GHCi:
    ```
        $ ghci
        Prelude> :cd <file_dir>
        Prelude> :l HelloWorld
    ```

    (FYI: :l is just an abbreviation of :load)
-}