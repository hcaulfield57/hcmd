import System.Environment
import System.IO

cat :: Bool -> String -> IO ()
cat True _ = do
    buf <- hGetContents stdin
    hPutStr stdout buf
cat False file = do
    han <- openFile file ReadMode
    buf <- hGetContents han
    hPutStr stdout buf

main :: IO ()
main = do
    argv <- getArgs
    case length argv of
        0 -> cat True ""
        _ -> mapM_ (cat False) argv
