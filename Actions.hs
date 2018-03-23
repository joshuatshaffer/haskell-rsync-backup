
module Actions (Action, chooseActions, preformAction) where

import VersionInfo (VersionCatagory,VersionStatus)

import Data.Time.Clock.POSIX (POSIXTime)


data Action = CreateSyncDest POSIXTime
            | CreateSyncDest'
            | DeleteVersion POSIXTime
            | DeleteDownloads POSIXTime
            | Sync
            deriving (Show, Eq)


preformAction :: Action -> IO ()
preformAction = undefined

chooseActions :: [(POSIXTime,VersionCatagory,VersionStatus)] -> [Action]
chooseActions = undefined
