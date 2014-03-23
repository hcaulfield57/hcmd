import Control.Monad
import Control.Monad.Reader
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO

data Flag = Force | Recurse
    deriving Eq

flags :: [OptDescr Flag]
flags = [Option "f" [] (NoArg Force) [],
         Option "r" [] (NoArg Recurse) []]

remove :: [String] -> ReaderT [Flag] IO ()
remove (x:xs) = do
    isFile <- lift $ doesFileExist x
    isDir <- lift $ doesDirectoryExist x
    if isFile 
        then rm x >> remove xs
        else if isDir
             then rmdir x >> remove xs
             else force x >> remove xs
remove [] = return ()

rm :: String -> ReaderT [Flag] IO ()
rm file = lift $ removeFile file

rmdir :: String -> ReaderT [Flag] IO ()
rmdir dir = do
    opts <- ask
    dotdot <- lift $ getDirectoryContents dir
    let recurse = Recurse `elem` opts
        base = filter (`notElem` [".",".."]) dotdot
        contents = map ((dir++"/")++) base
    if recurse then remove contents >> lift (removeDirectory dir)
        else lift $ warn (dir ++ " is directory!")

force :: String -> ReaderT [Flag] IO ()
force path = do
    opts <- ask
    let force' = Force `elem` opts
    unless force' 
        $ lift $ warn (path ++ " does not exist!")

main :: IO ()
main = do
    argv <- getArgs
    case getOpt RequireOrder flags argv of
        (opts,args,_) -> runReaderT (remove args) opts

warn :: String -> IO ()
warn = hPutStrLn stderr
