module Main where

import           System.IO                                ( hFlush
                                                          , stdout
                                                          )

import           Lib                                      ( initState
                                                          , initScript
                                                          , runEval
                                                          )

main :: IO ()
main = do
  putStrLn "Welcome to lazyfuck! Each line will be evaluated as a script!"
  go
 where
  go :: IO ()
  go = do
    putStr "lf)) "
    hFlush stdout
    script <- getLine
    runEval (initScript script, initState)
    putStrLn ""
    go
