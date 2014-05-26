{-# LANGUAGE TypeFamilies  #-}
module Control.Concurrent.Units
  ( threadDelay
  , timeout
  , Second
  , milli
  , micro
  , Minute(..)
  , Hour(..)
  ) where

import qualified Control.Concurrent.Thread.Delay as Conc
import qualified Control.Concurrent.Timeout     as Timeout
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
threadDelay t = Conc.delay ( round $ t ## micro Second)

timeout :: Time -> IO a -> IO (Maybe a)
timeout t = Timeout.timeout (round $ t ## micro Second)
