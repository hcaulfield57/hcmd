{- BUGS: No error handling of any kind
 - Breaks if user supplies trailing / -}

module Main where

import Control.Monad
import Control.Monad.Reader
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.Printf

data Flag = Verbose | Recurse
    deriving Eq

flags :: [OptDescr Flag]
flags = [Option "v" [] (NoArg Verbose) [],
         Option "rR" [] (NoArg Recurse) []]

copy :: [String] -> String-> ReaderT [Flag] IO ()
copy [] _ = return ()
copy s@(src:srcs) dst = do
    isSrcFile <- lift $ doesFileExist src
    isDstFile <- lift $ doesFileExist dst
    isSrcDir <- lift $ doesDirectoryExist src
    isDstDir <- lift $ doesDirectoryExist dst
    when isSrcFile (cp src dst)
    when (isSrcDir && (not isDstFile))
        (if (length (dst:s)) /= 2 then lift (usage [])
            else cpdir src dst)
    when (isSrcFile && isDstDir) (cp src dst)
    copy srcs dst

cp :: String -> String -> ReaderT [Flag] IO ()
cp src dst = do
    opts <- ask
    isDstDir <- lift $ doesDirectoryExist dst
    let dst' = if isDstDir then dst ++ "/" ++ src
               else dst
    lift $ copyFile src dst'
    lift $ when (Verbose `elem` opts) 
        (verbose src dst')

cpdir :: String -> String -> ReaderT [Flag] IO ()
cpdir src dst = do
    opts <- ask
    isDstDir <- lift $ doesDirectoryExist dst
    dotdot <- lift $ getDirectoryContents src
    let dst' = if isDstDir then dst ++ "/" ++ src
               else dst
        basename = filter (`notElem` [".",".."]) dotdot
        contents = map ((dst'++"/")++) basename
    lift $ when (Recurse `notElem` opts) (usage [])
    lift $ createDirectoryIfMissing False dst'
    copy contents dst'

verbose :: String -> String -> IO ()
verbose src dst = printf "cp: %s\t%s\n" src dst

main :: IO ()
main = do
    argv <- getArgs
    case getOpt RequireOrder flags argv of
        (opts,args,[]) -> runReaderT 
            (copy (init args) (last args)) opts
        (_,_,err) -> usage $ concat err

usage :: String -> IO ()
usage err = do 
    unless (null err) (hPutStrLn stderr err)
    hPutStr stderr $ "usage: cp [-rv] file1 file2\n" ++
                     "usage: cp [-rv] file ... directory\n" ++
                     "usage: cp [-rv] dir1 dir2\n"
    exitWith (ExitFailure 1)
