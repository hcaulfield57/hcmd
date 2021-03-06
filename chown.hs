module Main where

import Control.Monad.Trans (liftIO)
import System.Environment
import System.Exit
import System.Posix.Files
import System.Posix.Types
import System.Posix.User
import Text.Parsec
import Text.ParserCombinators.Parsec (Parser)

type ID = (UserID,GroupID)

main :: IO ()
main = do
    (arg:argv) <- getArgs
    parsed <- runParserT parseChown () "" arg
    let (uid,gid) = case parsed of
            (Right ids) -> ids
            (Left err) -> ((-1),(-1))  -- should never be reached
    mapM_ (chown uid gid) argv

chown :: UserID -> GroupID -> FilePath -> IO ()
chown uid gid fp = setOwnerAndGroup fp uid gid

parseChown :: ParsecT String () IO ID
parseChown =
    try group <|>
    try ownerAndGroup <|>
    try owner <?>
    "Invalid Argument: "

owner :: ParsecT String () IO ID
owner = do
    user <- many letter
    uid <- liftIO $ return . userID =<< getUserEntryForName user
    gid <- liftIO $ return . userGroupID =<< getUserEntryForName user
    return (uid,gid)

ownerAndGroup :: ParsecT String () IO ID
ownerAndGroup = do
    user <- many letter
    char ':'
    group <- many letter
    uid <- liftIO $ return . userID =<< getUserEntryForName user
    gid <- liftIO $ return . groupID =<< getGroupEntryForName group
    return (uid,gid)

group :: ParsecT String () IO ID
group = do
    char ':'
    group <- many letter
    gid <- liftIO $ return . groupID =<< getGroupEntryForName group
    return ((-1),gid)
