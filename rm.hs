{- Copyright (C) 2013 Grant Mather <hcaulfield57@gmail.com>
 -
 - Permission to use, copy, modify, and/or distribute this software for any
 - purpose with or without fee is hereby granted, provided that the above 
 - copyright notice and this permission notice appear in all copies.
 -
 - THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 - WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 - MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 - ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 - WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 - ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 - OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 -}

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO
import System.IO.Error
import System.Posix.Files

data Flag = Recurse | Force
    deriving Eq

options :: [OptDescr Flag]
options = [ Option "Rr" [] (NoArg Recurse) [],
            Option "f" [] (NoArg Force) [] ]

main :: IO ()
main = do
    argv <- getArgs
    let (flags,_,_) = getOpt RequireOrder options argv
        recurse = any (== Recurse) flags
        force = any (== Force) flags
    case recurse of
        True -> rmdir force (tail argv) 
        False -> case force of          -- find out if options provided
            True -> rm force (tail argv)
            False -> rm force argv

rm :: Bool -> [String] -> IO ()
rm _ [] = return ()
rm force (x:xs) = do
    ret <- try (removeFile x)
    case ret of
        Right _ -> rm force xs
        Left e -> case force of
            True -> rm force xs
            False -> warn (show e)
                >> rm force xs

rmdir :: Bool -> [String] -> IO ()
rmdir _ [] = return ()
rmdir force (x:xs) = do
    isFile <- doesFileExist x
    isDir <- doesDirectoryExist x
    case isFile of
        True -> rm force [x]
            >> rmdir force xs
        False -> case isDir of
            True -> do
                contents <- getDirectoryContents x >>= return . 
                    filter (`notElem` [".",".."])
                rmdir force (map ((x ++ "/") ++) contents) -- does not handle dir/ properly
                removeDirectory x --should be empty now
            False -> case force of
                True -> rmdir force xs
                False -> warn (x ++ " does not exist!")
                    >> rmdir force xs

warn :: String -> IO ()
warn w = hPutStrLn stderr w
