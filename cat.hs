module Main where

import System.Environment
import System.IO

cat :: Bool -> String -> IO ()
cat False file = readFile file >>= putStr
cat True _ = getContents >>= putStr

main :: IO ()
main = do
    argv <- getArgs
    if null argv -- stdin
        then cat True []
        else mapM_ (cat False) argv
