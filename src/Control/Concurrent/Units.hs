{-# LANGUAGE TypeFamilies  #-}
module Control.Concurrent.Units
  ( module ControlConcurrent
  , threadDelay
  , timeout
  , Second
  , milli
  , micro
  , Minute(..)
  , Hour(..)
  ) where

import Control.Concurrent as ControlConcurrent hiding (threadDelay)
import qualified Control.Concurrent as Conc
import qualified System.Timeout     as Timeout
import Data.Metrology
import Data.Metrology.SI

data Minute = Minute
instance Unit Minute where
  type BaseUnit Minute = Second
  conversionRatio _ = 60
instance Show Minute where
  show _ = "min"

data Hour = Hour
instance Unit Hour where
  type BaseUnit Hour = Second
  conversionRatio _ = 60 * 60
instance Show Hour where
  show _ = "hour"


threadDelay :: Time -> IO ()
threadDelay t = Conc.threadDelay ( t #! micro Second)

timeout :: Time -> IO a -> IO (Maybe a)
timeout t = Timeout.timeout (t #! micro Second)
