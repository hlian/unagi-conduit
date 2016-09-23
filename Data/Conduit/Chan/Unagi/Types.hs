-- |
-- Maintainer: hi@haolian.org
-- Portability: GHC
--
-- This module dumbly rounds up Unagi channels into a "Readable" and a
-- "Writeable" typeclass, to be used by "Data.Conduit.Chan.Unagi".

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TypeFamilies    #-}

module Data.Conduit.Chan.Unagi.Types where

import qualified Control.Concurrent.Chan.Unagi as U
import qualified Control.Concurrent.Chan.Unagi.Bounded as UB
import qualified Control.Concurrent.Chan.Unagi.Unboxed as UX
import           GHC.Exts

-- | A typeclass for readable, blocking Unagi channels
class Readable chan where
  type ReadableConstraint chan a :: Constraint
  readChan :: ReadableConstraint chan a => chan a -> IO a

-- | A typeclass for writeable, blocking Unagi channels
class Writeable chan where
  type WriteableConstraint chan a :: Constraint
  writeChan :: WriteableConstraint chan a => chan a -> a -> IO ()

-- | "Control.Concurrent.Chan.Unagi.OutChan" (unbounded queue)
instance Readable U.OutChan where
  type ReadableConstraint U.OutChan a = ()
  readChan = U.readChan

-- | "Control.Concurrent.Chan.Unagi.Bounded.OutChan" (bounded queue)
instance Readable UB.OutChan where
  type ReadableConstraint UB.OutChan a = ()
  readChan = UB.readChan

-- | "Control.Concurrent.Chan.Unagi.Unboxed.OutChan" (unbounded queue for unboxed types)
instance Readable UX.OutChan where
  type ReadableConstraint UX.OutChan a = UX.UnagiPrim a
  readChan = UX.readChan

-- | "Control.Concurrent.Chan.Unagi.InChan" (unbounded queue)
instance Writeable U.InChan where
  type WriteableConstraint U.InChan a = ()
  writeChan = U.writeChan

-- | "Control.Concurrent.Chan.Unagi.Bounded.InChan" (bounded queue)
instance Writeable UB.InChan where
  type WriteableConstraint UB.InChan a = ()
  writeChan = UB.writeChan

-- | "Control.Concurrent.Chan.Unagi.Unboxed.InChan" (unbounded queue for unboxed types)
instance Writeable UX.InChan where
  type WriteableConstraint UX.InChan a = UX.UnagiPrim a
  writeChan = UX.writeChan
