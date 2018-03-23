
module Decisions (VersionCatagory, VersionStatus, catagorizeVersions, statusOfVersions) where

import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Calendar
import Data.List (mapAccumL, sort)

data VersionCatagory = VC_Latest
                     | VC_Year
                     | VC_Month
                     | VC_Day
                     | VC_Other
                     deriving (Show, Eq)

data VersionStatus = VS_Normal
                   | VS_Stale
                   | VS_Latest
                   deriving (Show, Eq)


unpackTime :: POSIXTime -> (Integer,Int,Int,POSIXTime)
unpackTime s = (y,m,d,s)
  where (y,m,d) = toGregorian . utctDay $ posixSecondsToUTCTime s

bar :: (Integer,Int,Int,POSIXTime) -> (Integer,Int,Int,POSIXTime) -> (POSIXTime,VersionCatagory)
bar (y',m',d',_) (y,m,d,s) = (s,c)
  where c | y > y'    = VC_Year
          | m > m'    = VC_Month
          | d > d'    = VC_Day
          | otherwise = VC_Other

-- Catagorize the versions.
catagorizeVersions :: [POSIXTime] -> [(POSIXTime,VersionCatagory)]
catagorizeVersions versions = oldest:middle ++ [latest]
  where
    versions' = sort versions
    versions'' = map unpackTime versions'
    oldest = (head versions', VC_Year)
    latest = (last versions', VC_Latest)
    middle = init $ zipWith bar (versions'') (tail versions'')

secondsInADay,secondsInAYear,secondsInAMonth,secondsInAWeek :: POSIXTime
secondsInADay = 60 * 60 * 24
secondsInAYear  = secondsInADay * 365
secondsInAMonth = secondsInADay * 31
secondsInAWeek  = secondsInADay * 7

statusOfVersions :: POSIXTime -> [POSIXTime] -> [(POSIXTime,VersionCatagory,VersionStatus)]
statusOfVersions now = map (\(t,c) -> (t, c, foo t c)) . catagorizeVersions
  where
    foo :: POSIXTime -> VersionCatagory -> VersionStatus
    foo t VC_Latest = VS_Latest
    foo t VC_Year   = VS_Normal
    foo t VC_Month  = if now - t > secondsInAYear  then VS_Stale else VS_Normal
    foo t VC_Day    = if now - t > secondsInAMonth then VS_Stale else VS_Normal
    foo t VC_Other  = if now - t > secondsInAWeek  then VS_Stale else VS_Normal
