module Main where

import Control.Monad
import System.Directory
import System.Environment
import System.Exit
import System.IO

mv :: FilePath -> FilePath -> IO ()
mv f1 f2 = do
    isDirf1 <- doesDirectoryExist f1
    isDirf2 <- doesDirectoryExist f2
    if isDirf1 then usage
        else if isDirf2 then renameFile f1 (f2 ++ "/" ++ f1)
            else renameFile f1 f2

main :: IO ()
main = do
    argv' <- getArgs
    let argv = slash argv'
    case length argv of
        0 -> usage
        1 -> usage
        2 -> mv (head argv) (head . tail $ argv)
        _ -> mapM_ (`mv` last argv) (init argv)
    where slash [] = []
          slash xs = map slash' xs
          slash' [] = []
          slash' (x:xs) = if x == '/' then slash' xs else x : slash' xs

usage :: IO ()
usage = hPutStrLn stderr "usage:\tmv file1 file2\n\tmv file ... directory"
    >> exitWith (ExitFailure 1)
