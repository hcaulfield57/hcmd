module Main where

import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.Printf

data Flag = Verbose | Recurse
    deriving Eq

type CopyMonad = ReaderT [Flag] (ErrorT IOException IO) ()

flags :: [OptDescr Flag]
flags = [Option "v" [] (NoArg Verbose) [],
         Option "rR" [] (NoArg Recurse) []]

copy :: [String] -> String -> CopyMonad 
copy [] _ = return ()
copy src@(s:ss) dst = do
    isSrcFile <- liftIO $ doesFileExist s
    isDstFile <- liftIO $ doesFileExist dst
    isSrcDir <- liftIO $ doesDirectoryExist s
    isDstDir <- liftIO $ doesDirectoryExist dst
    when (not isDstFile && not isDstDir) -- file or dir to dne
        (if isSrcFile then cp s dst
            else if isDstFile then cpdir s dst
                else liftIO $ usage [])
    when (isSrcFile && isDstDir) (cp s dst)
    when (isSrcDir && isDstFile) (liftIO $ usage []) -- dir to file
    when (isSrcDir && isDstDir) (cpdir s dst) -- dir to dir
    when (isSrcFile && isDstFile) (cp s dst) -- file to file
    copy ss dst

cpdir :: String -> String -> CopyMonad
cpdir src dst = do
    opts <- ask
    dotdot <- liftIO $ getDirectoryContents src
    when (Recurse `notElem` opts) 
        (liftIO $ usage (src ++ " is directory!"))
    let dst' = if last dst == '/'
               then dst
               else dst ++ "/"
        src' = if last src == '/'
               then src
               else src ++ "/"
        dotdot' = filter (`notElem` [".",".."]) dotdot
        contents = map (src'++) dotdot'
    liftIO $ createDirectoryIfMissing False dst'
    copy contents dst'

cp :: String -> String -> CopyMonad
cp src dst = do
    opts <- ask
    let dst' = if last dst == '/'
               then dst ++ src
               else dst ++ "/" ++ src
    liftIO $ copyFile src dst'
    when (Verbose `elem` opts) (liftIO $ verbose src dst')

verbose :: String -> String -> IO ()
verbose = printf "cp: %s\t%s\n"

main :: IO ()
main = do
    argv <- getArgs
    case getOpt RequireOrder flags argv of
        (opts,args,[]) -> do
            res <- runErrorT $ runReaderT 
                (copy (init args) (last args)) opts
            case res of
                (Right _) -> return ()
                (Left err) -> print err
        (_,_,err) -> usage $ concat err

usage :: String -> IO ()
usage errstr = do
    unless (null errstr) (hPutStrLn stderr errstr)
    hPutStr stderr $ "usage: cp [-rv] file1 file2\n" ++
                     "usage: cp [-rv] file ... directory\n" ++
                     "usage: cp [-rv] dir1 dir2\n"
    exitWith (ExitFailure 1)
