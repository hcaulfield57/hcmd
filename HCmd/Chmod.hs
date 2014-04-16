module HCmd.Chmod where

import System.Posix.Files
import System.Posix.Types

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
