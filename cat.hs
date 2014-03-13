import Control.Monad
import System.IO
import System.Environment

cat :: Handle -> IO ()
cat h = hGetContents h >>= putStr

main :: IO ()
main = do
    argv <- getArgs
    case null argv of
        True -> cat stdin
        False -> mapM_ ((openFile' ReadMode) >=> cat) argv
    where openFile' = flip openFile
