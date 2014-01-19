module Util.Time where

import           Data.Time
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

epochNow :: IO Double
epochNow = getCurrentTime >>= utcToEpoch where
  utcToEpoch :: UTCTime -> IO Double
  utcToEpoch = return . fromIntegral . round . utcTimeToPOSIXSeconds
