{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Hadron.Logger
    ( module Hadron.Logger
    , module Katip.Core
    ) where

-------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Data.IORef
import           Katip.Core
import           Katip.Scribes.Handle
import           System.IO
import           System.IO.Unsafe
-------------------------------------------------------------------------------


runLog :: MonadIO m => KatipT m b -> m b
runLog m = liftIO (readIORef _ioLogger) >>= flip runKatipT m



-------------------------------------------------------------------------------
_ioLogger :: IORef LogEnv
_ioLogger = unsafePerformIO $ do
  le <- initLogEnv "hadron" "-"
  hSetBuffering stdout LineBuffering
  s <- mkHandleScribe ColorIfTerminal stdout InfoS V3
  newIORef $ registerScribe "stdout" s le
{-# NOINLINE _ioLogger #-}


