module Chmod where

import Control.Exception
import System.Exit
import System.IO
import System.Posix.Files

chmod :: FilePath -> String -> IO ()
chmod file mode
    | length mode /= 3 = usage "-m supports no more than three decimals"
    | otherwise = do
        let user = mode !! 0
            group = mode !! 1
            other = mode !! 2
            usermode = case user of
                '7' -> ownerModes
                '6' -> unionFileModes ownerReadMode ownerWriteMode
                '5' -> unionFileModes ownerReadMode ownerExecuteMode
                '4' -> ownerReadMode
                '3' -> unionFileModes ownerWriteMode ownerExecuteMode
                '2' -> ownerWriteMode
                '1' -> ownerExecuteMode
            groupmode = case group of
                '7' -> groupModes
                '6' -> unionFileModes groupReadMode groupWriteMode
                '5' -> unionFileModes groupReadMode groupExecuteMode
                '4' -> groupReadMode
                '3' -> unionFileModes groupWriteMode groupExecuteMode
                '2' -> groupWriteMode
                '1' -> groupExecuteMode
            othermode = case other of
                '7' -> otherModes
                '6' -> unionFileModes otherReadMode otherWriteMode
                '5' -> unionFileModes otherReadMode otherExecuteMode
                '4' -> otherReadMode
                '3' -> unionFileModes otherWriteMode otherExecuteMode
                '2' -> otherWriteMode
                '1' -> otherExecuteMode
            filemode = unionFileModes usermode (unionFileModes 
                groupmode othermode)
        ret <- try (setFileMode file filemode) :: IO (Either IOError ())
        case ret of
            Right _ -> return ()
            Left e -> hPutStrLn stderr (show e)
                >> exitWith (ExitFailure 1)

usage :: String -> IO ()
usage s = do
    hPutStrLn stderr s
    hPutStrLn stderr "usage: mkdir [-p] [-m mode] dirname ..."
    exitWith (ExitFailure 1)
