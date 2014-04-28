module Main where

import System.Environment

main :: IO ()
main = do
    argv <- getArgs
    mapM_ (putStrLn . basename) argv

basename :: String -> String
basename str = slash str str
    where slash [] rest = rest
          slash (x:xs) rest = 
            if x == '/'
            then slash xs xs
            else slash xs rest
