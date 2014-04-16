module HCmd.Chmod (chmod) where

import qualified Control.Monad.State as S
import Control.Monad.Reader
import Data.Functor.Identity (Identity(..))
import System.Posix.Files
import System.Posix.Types
import Text.Parsec.Prim (Parsec, putState)
import Text.ParserCombinators.Parsec

data User = AllUser | User | Group | Other
    deriving Eq
data Action = Equals | Subtract | Add | Null
    deriving Eq
data Mode = AllMode | Read | Write | Execute
    deriving Eq

type Symbolic = ([User],Action,[Mode])
type SymbolicInfo = (Symbolic,FileMode)
type SymbolicChMonad = Parsec String SymbolicInfo FileMode

chmod :: String -> FileMode
chmod (s:u:g:o:[]) =
    let system = case s of
          '0' -> nullFileMode
          '1' -> nullFileMode -- TODO - Sticky Bit
          '2' -> setGroupIDMode
          '3' -> nullFileMode -- TODO - Sticky Bit
          '4' -> setUserIDMode
          '6' -> unionFileModes setGroupIDMode setUserIDMode
          '7' -> unionFileModes setGroupIDMode setUserIDMode
            -- TODO - Sticky Bit
        user = case u of
          '0' -> nullFileMode
          '1' -> ownerExecuteMode
          '2' -> ownerWriteMode
          '3' -> unionFileModes ownerWriteMode ownerExecuteMode
          '4' -> ownerReadMode
          '5' -> unionFileModes ownerReadMode ownerExecuteMode
          '6' -> unionFileModes ownerReadMode ownerWriteMode
          '7' -> ownerModes
        group = case g of
          '0' -> nullFileMode
          '1' -> groupExecuteMode
          '2' -> groupWriteMode
          '3' -> unionFileModes groupWriteMode groupExecuteMode
          '4' -> groupReadMode
          '5' -> unionFileModes groupReadMode groupExecuteMode
          '6' -> unionFileModes groupReadMode groupWriteMode
          '7' -> groupModes
        other = case o of
          '0' -> nullFileMode
          '1' -> otherExecuteMode
          '2' -> otherWriteMode
          '3' -> unionFileModes otherWriteMode otherExecuteMode
          '4' -> otherReadMode
          '5' -> unionFileModes otherReadMode otherExecuteMode
          '6' -> unionFileModes otherReadMode otherExecuteMode
          '7' -> otherModes
    in unionFileModes system . unionFileModes user 
        $ unionFileModes group other

symbolicChmod :: String -> FileMode -> FileMode
symbolicChmod modeStr initMode = 
    case runParser chMonad (([],Null,[]),initMode) "" modeStr of
        (Left _) -> initMode
        (Right mode) -> mode

chMonad :: SymbolicChMonad
chMonad = do 
    many userParse 
    actionParse 
    modeParse
    ((user,action,mode),curMode) <- getState
    return $ case action of
        Equals -> 
            S.execState 
                (runReaderT (eqMode user mode) (user,mode)) nullFileMode

eqMode :: [User] -> [Mode] -> ReaderT ([User],[Mode]) (S.State FileMode) ()
eqMode _ [] = return ()
eqMode [] (m:ms) = do
    (user,_) <- ask
    eqMode user ms
eqMode (u:us) (m:ms) = do
    newMode <- S.get
    case u of
        AllUser -> case m of
            AllMode -> S.put (unionFileModes (chmod "0777") newMode)
                >> eqMode us (m:ms)
            Read -> S.put (unionFileModes (chmod "0444") newMode)
                >> eqMode us (m:ms)
            Write -> S.put (unionFileModes (chmod "0222") newMode)
                >> eqMode us (m:ms)
            Execute -> S.put (unionFileModes (chmod "0111") newMode)
                >> eqMode us (m:ms)
        User -> case m of
            AllMode -> S.put (unionFileModes (chmod "0700") newMode)
                >> eqMode us (m:ms)
            Read -> S.put (unionFileModes (chmod "0400") newMode)
                >> eqMode us (m:ms)
            Write -> S.put (unionFileModes (chmod "0200") newMode)
                >> eqMode us (m:ms)
            Execute -> S.put (unionFileModes (chmod "0100") newMode)
                >> eqMode us (m:ms)
        Group -> case m of
            AllMode -> S.put (unionFileModes (chmod "0070") newMode)
                >> eqMode us (m:ms)
            Read -> S.put (unionFileModes (chmod "0040") newMode)
                >> eqMode us (m:ms)
            Write -> S.put (unionFileModes (chmod "0020") newMode)
                >> eqMode us (m:ms)
            Execute -> S.put (unionFileModes (chmod "0010") newMode)
                >> eqMode us (m:ms)
        Other -> case m of 
            AllMode -> S.put (unionFileModes (chmod "0007") newMode)
                >> eqMode us (m:ms)
            Read -> S.put (unionFileModes (chmod "0004") newMode)
                >> eqMode us (m:ms)
            Write -> S.put (unionFileModes (chmod "0002") newMode)
                >> eqMode us (m:ms)
            Execute -> S.put (unionFileModes (chmod "0001") newMode)
                >> eqMode us (m:ms)

userParse :: SymbolicChMonad
userParse = do
    ((user,action,mode),curMode) <- getState
    ch <- oneOf "augo"
    case ch of
        'a' -> putState ((AllUser:user,action,mode),curMode)
            >> return curMode
        'u' -> putState ((User:user,action,mode),curMode)
            >> return curMode
        'g' -> putState ((Group:user,action,mode),curMode)
            >> return curMode
        'o' -> putState ((Other:user,action,mode),curMode)
            >> return curMode
        
actionParse :: SymbolicChMonad
actionParse = do
    ((user,action,mode),curMode) <- getState
    ch <- oneOf "-+="
    case ch of
        '-' -> putState ((user,Subtract,mode),curMode)
            >> return curMode
        '+' -> putState ((user,Add,mode),curMode)
            >> return curMode
        '=' -> putState ((user,Equals,mode),curMode)
            >> return curMode

modeParse :: SymbolicChMonad
modeParse = do
    ((user,action,mode),curMode) <- getState
    ch <- oneOf "arwx"
    case ch of
        'a' -> putState ((user,action,AllMode:mode),curMode)
            >> return curMode
        'r' -> putState ((user,action,Read:mode),curMode)
            >> return curMode
        'w' -> putState ((user,action,Write:mode),curMode)
            >> return curMode
        'x' -> putState ((user,action,Execute:mode),curMode)
            >> return curMode
