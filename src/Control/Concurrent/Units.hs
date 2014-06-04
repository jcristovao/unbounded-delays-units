{-# LANGUAGE TypeFamilies  #-}
module Control.Concurrent.Units
  ( threadDelay
  , timeout
  , Second
  , milli
  , micro
  , Minute(..)
  , Hour(..)
  , Day(..)
  ) where

import qualified Control.Concurrent.Thread.Delay as Conc
import qualified Control.Concurrent.Timeout     as Timeout
import Data.Metrology
import Data.Metrology.SI


-- | Like @Control.Concurrent.'threadDelay'@, but with a delay specified as a
-- proper time unit.
--
-- For example:
--
-- > threadDelay (5 %% Second |+| 40 %% milli Second)
--
-- or
--
-- > threadDelay (5.040 %% Second)
--
-- There is no guarantee that the thread will be rescheduled promptly when the
-- delay has expired, but the thread will never continue to run earlier than
-- specified.
--

threadDelay :: Time -> IO ()
threadDelay t = Conc.delay ( round $ t ## micro Second)

-- | Like @System.Timeout.'System.Timeout.timeout'@, but with a delay specified as a
-- proper time unit.
--
-- For example:
--
-- > timeout (4.5 %% Hour) (reallyLongIOAction)
--
-- Wrap an 'IO' computation to time out and return 'Nothing' in case no result is
-- available within @n@ seconds. In case a result is
-- available before the timeout expires, 'Just' @a@ is returned. A negative timeout
-- interval means \"wait indefinitely\".
--
-- The design of this combinator was guided by the objective that @timeout n f@
-- should behave exactly the same as @f@ as long as @f@ doesn't time out. This
-- means that @f@ has the same 'myThreadId' it would have without the timeout
-- wrapper. Any exceptions @f@ might throw cancel the timeout and propagate further
-- up. It also possible for @f@ to receive exceptions thrown to it by another
-- thread.
--
-- A tricky implementation detail is the question of how to abort an 'IO'
-- computation. This combinator relies on asynchronous exceptions internally.  The
-- technique works very well for computations executing inside of the Haskell
-- runtime system, but it doesn't work at all for non-Haskell code. Foreign
-- function calls, for example, cannot be timed out with this combinator simply
-- because an arbitrary C function cannot receive asynchronous exceptions. When
-- @timeout@ is used to wrap an FFI call that blocks, no timeout event can be
-- delivered until the FFI call returns, which pretty much negates the purpose of
-- the combinator. In practice, however, this limitation is less severe than it may
-- sound. Standard I\/O functions like 'System.IO.hGetBuf', 'System.IO.hPutBuf',
-- Network.Socket.accept, or 'System.IO.hWaitForInput' appear to be blocking, but
-- they really don't because the runtime system uses scheduling mechanisms like
-- @select(2)@ to perform asynchronous I\/O, so it is possible to interrupt
-- standard socket I\/O or file I\/O using this combinator.
--
timeout :: Time -> IO a -> IO (Maybe a)
timeout t = Timeout.timeout (round $ t ## micro Second)
