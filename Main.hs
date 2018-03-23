
module Main (main) where

import VersionInfo
import Actions

import Data.Time.Clock.POSIX
import Control.Monad (filterM)
import System.Directory
import System.Posix.Files
import System.FilePath.Posix


getExistingVersions :: FilePath -> IO [POSIXTime]
getExistingVersions dir = do
  files <- listDirectory dir
  dirs <- filterM (fmap isDirectory . getFileStatus . (dir </>)) files
  return $ map (fromInteger . read . takeFileName) dirs

main :: IO ()
main = do
  versions <- getExistingVersions "test"
  print versions
  time <- getPOSIXTime
  let statuses = statusOfVersions time versions
  mapM_ print statuses
  let actions = chooseActions statuses
  mapM_ print actions
