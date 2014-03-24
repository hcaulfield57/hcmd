module Main where

import Control.Monad
import System.Environment
import System.Exit
import System.IO

echo :: Bool -> [String] -> IO ()
echo newLine args = do
    echo' args (length args)
    when newLine (putStrLn "")
    where echo' (x:_) 1 = putStr x
          echo' (x:xs) i = putStr (x++" ") 
            >> echo' xs (i-1)

main :: IO ()
main = do
    argv <- getArgs
    case argv of
        ("-n":args) -> echo False args
        (('-':_):args) -> usage
        _ -> echo True argv

usage :: IO ()
usage = hPutStrLn stderr "usage: echo [-n] ..."
    >> exitWith (ExitFailure 1)
