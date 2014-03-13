-- rmdir(1)

import Control.Exception
import System.Directory
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = getArgs >>= mapM_ rmdir

rmdir :: String -> IO ()
rmdir x = catch (removeDirectory x) usage 

usage :: SomeException -> IO ()
usage e = hPutStrLn stderr ("rmdir: " ++ show e)
    >> exitWith (ExitFailure 1)
