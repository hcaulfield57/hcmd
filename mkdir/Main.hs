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

module Main where

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO

import Chmod

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
