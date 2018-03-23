
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Calendar
import Data.List (mapAccumL, sort)
import Control.Monad (filterM)
import System.Directory
import System.Posix.Files
import System.FilePath.Posix

data VersionCat = VCYear | VCMonth | VCDay | VCLatest | VCOther deriving (Show, Eq)
data Action = KeepVersion POSIXTime
            | DeleteStaleMonth POSIXTime
            | DeleteStaleDay POSIXTime
            | DeleteStaleOther POSIXTime
            | LinkFromLatest POSIXTime
            deriving (Show, Eq)

unpackTime :: POSIXTime -> (Integer,Int,Int,POSIXTime)
unpackTime s = (y,m,d,s)
  where (y,m,d) = toGregorian . utctDay $ posixSecondsToUTCTime s

-- Catagorize the versions.
-- Does not work with verions made before the year 0 (Gregorian).
-- Considering that computers had not been invented, it is safe
-- to assume that there will not be any versions from that time.
catagorize :: [POSIXTime] -> [(POSIXTime,VersionCat)]
catagorize = snd . mapAccumL foo (0,0,0) . sort . map unpackTime
  where
    foo :: (Integer,Int,Int) -> (Integer,Int,Int,POSIXTime) -> ((Integer,Int,Int), (POSIXTime,VersionCat))
    foo (y',m',d') (y,m,d,s) = ((y,m,d), (s,c))
      where c | y > y'    = VCYear
              | m > m'    = VCMonth
              | d > d'    = VCDay
              | otherwise = VCOther

getExistingVersions :: FilePath -> IO [POSIXTime]
getExistingVersions dir = do
  files <- listDirectory dir
  dirs <- filterM (fmap isDirectory . getFileStatus . (dir </>)) files
  return $ map (fromInteger . read . takeFileName) dirs

secondsInADay,secondsInAYear,secondsInAMonth,secondsInAWeek :: POSIXTime
secondsInADay = 60 * 60 * 24
secondsInAYear = secondsInADay * 365
secondsInAMonth = secondsInADay * 31
secondsInAWeek = secondsInADay * 7

pickActions :: POSIXTime -> [POSIXTime] -> [Action]
pickActions now = map foo . catagorize
  where
    foo :: (POSIXTime,VersionCat) -> Action
    foo (t,VCLatest) = LinkFromLatest t
    foo (t,VCYear) = KeepVersion t
    foo (t,VCMonth) = if now - t > secondsInAYear then DeleteStaleMonth t else KeepVersion t
    foo (t,VCDay) = if now - t > secondsInAMonth then DeleteStaleDay t else KeepVersion t
    foo (t,VCOther) = if now - t > secondsInAWeek then DeleteStaleOther t else KeepVersion t

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
