-- |
-- Maintainer: hi@haolian.org
-- Portability: GHC
--
-- This module dumbly wraps up blocking Unagi channels into conduit
-- sources and sinks.

module Data.Conduit.Chan.Unagi where

import qualified Data.Conduit.List as Conduit

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Conduit
import           Data.Conduit.Chan.Unagi.Types

-- | Data arriving on the out-channel is passed to the source
sourceChan :: (MonadIO m, Readable chan, ReadableConstraint chan a) => chan a -> Source m a
sourceChan chan =
  forever $ liftIO (readChan chan) >>= yield

-- | Data arriving on the sink is passed to the in-channel
sinkChan :: (MonadIO m, Writeable chan, WriteableConstraint chan a) => chan a -> Sink a m ()
sinkChan chan =
  Conduit.mapM_ (liftIO . writeChan chan)
