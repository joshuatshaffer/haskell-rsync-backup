module Main (main) where

import Data.Time.Clock.POSIX

import Control.Monad (filterM)
import System.Directory
import System.Posix.Files
import System.FilePath.Posix

import Decisions


getExistingVersions :: FilePath -> IO [POSIXTime]
getExistingVersions dir = do
  files <- listDirectory dir
  dirs <- filterM (fmap isDirectory . getFileStatus . (dir </>)) files
  return $ map (fromInteger . read . takeFileName) dirs

askUser :: [Action] -> IO ()
askUser _ = return ()

doActions :: [Action] -> IO ()
doActions _ = return ()

main :: IO ()
main = do
  versions <- getExistingVersions "test"
  print versions
  time <- getPOSIXTime
  let actions = pickActions time versions
  print actions
  askUser actions
  doActions actions
