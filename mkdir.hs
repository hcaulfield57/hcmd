module Main where

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO

import HCmd.Chmod

data Flag = Parents | Mode String
    deriving Eq

options :: [OptDescr Flag]
options = [ Option "p" [] (NoArg Parents) [],
            Option "m" [] (ReqArg Mode "MODE") [] ]

main :: IO ()
main = do
    argv <- getArgs
    let (flags,dirs,_) = getOpt Permute options argv
    mkdir flags dirs

mkdir :: [Flag] -> [String] -> IO ()
mkdir _ [] = return ()
mkdir flags (x:xs) = do
    case any (== Parents) flags of
        True -> createDirectoryIfMissing True x
        False -> createDirectoryIfMissing False x
    let mode = getMode flags
    case null mode of
        True -> mkdir flags xs
        False -> chmod x mode >> mkdir flags xs
    
getMode :: [Flag] -> String
getMode [] = []
getMode ((Mode m):ms) = m
getMode (_:ms) = getMode ms
