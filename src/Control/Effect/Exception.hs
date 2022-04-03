{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
-- | Operations from "Control.Exception" lifted into effectful contexts using 'Control.Effect.Lift.Lift'.
--
-- @since 1.0.0.0
module Control.Effect.Exception
( -- * Lifted "Control.Exception" operations
  throwIO
, ioError
, throwTo
, catch
, catches
, U.Handler(..)
, catchJust
, handle
, handleJust
, try
, tryJust
, evaluate
, mask
, mask_
, uninterruptibleMask
, uninterruptibleMask_
, getMaskingState
, interruptible
, allowInterrupt
, bracket
, bracket_
, bracketOnError
, finally
, onException
, module Control.Exception
  -- * Lift effect
, Lift(..)
, sendM
, liftWith
  -- * Re-exports
, Algebra
, Has
, run
) where

import           Control.Concurrent (ThreadId)
import qualified Control.Effect.Exception.UnliftIO as U
import           Control.Effect.Lift
import           Control.Exception hiding
  ( Handler
  , allowInterrupt
  , bracket
  , bracketOnError
  , bracket_
  , catch
  , catchJust
  , catches
  , evaluate
  , finally
  , getMaskingState
  , handle
  , handleJust
  , interruptible
  , ioError
  , mask
  , mask_
  , onException
  , throwIO
  , throwTo
  , try
  , tryJust
  , uninterruptibleMask
  , uninterruptibleMask_
  )
import qualified Control.Exception as Exc
import           Prelude hiding (ioError)

-- | See @"Unlift.Exception".throwIO@.
--
-- @since 1.0.0.0
throwIO :: (Exc.Exception e, Has (Lift IO) sig m) => e -> m a
throwIO = U.throwIO @IO

-- | See @"Control.Exception".'Exc.ioError'@.
--
-- @since 1.0.0.0
ioError :: Has (Lift IO) sig m => IOError -> m a
ioError = U.ioError @IO

-- | See @"Control.Exception".'Exc.throwTo'@.
--
-- @since 1.0.0.0
throwTo :: (Exc.Exception e, Has (Lift IO) sig m) => ThreadId -> e -> m ()
throwTo = U.throwTo @IO

-- | See @"Control.Exception".catch@.
--
-- @since 1.0.0.0
catch :: (Exc.Exception e, Has (Lift IO) sig m) => m a -> (e -> m a) -> m a
catch = U.catch @IO

-- | See @"Control.Exception".catches@.
--
-- @since 1.0.0.0
catches :: Has (Lift IO) sig m => m a -> [U.Handler m a] -> m a
catches = U.catches @IO

-- | See @"Control.Exception".catchJust@.
--
-- @since 1.0.0.0
catchJust
  :: (Exc.Exception e, Has (Lift IO) sig m)
  => (e -> Maybe b)
  -> m a
  -> (b -> m a)
  -> m a
catchJust = U.catchJust @IO

-- | See @"Control.Exception".'Exc.handle'@.
--
-- @since 1.0.0.0
handle :: (Exc.Exception e, Has (Lift IO) sig m) => (e -> m a) -> m a -> m a
handle = U.handle @IO

-- | See @"Control.Exception".'Exc.handleJust'@.
--
-- @since 1.0.0.0
handleJust
  :: (Exc.Exception e, Has (Lift IO) sig m)
  => (e -> Maybe b)
  -> (b -> m a)
  -> m a
  -> m a
handleJust = U.handleJust @IO

-- | See @"Control.Exception".'Exc.try'@.
--
-- @since 1.0.0.0
try :: (Exc.Exception e, Has (Lift IO) sig m) => m a -> m (Either e a)
try = U.try @IO

-- | See @"Control.Exception".'Exc.tryJust'@.
--
-- @since 1.0.0.0
tryJust :: (Exc.Exception e, Has (Lift IO) sig m) => (e -> Maybe b) -> m a -> m (Either b a)
tryJust = U.tryJust @IO

-- | See @"Control.Exception".evaluate@.
--
-- @since 1.0.0.0
evaluate :: Has (Lift IO) sig m => a -> m a
evaluate = U.evaluate @IO

-- | See @"Control.Exception".mask@.
--
-- @since 1.0.0.0
mask :: Has (Lift IO) sig m => ((forall a . m a -> m a) -> m b) -> m b
mask = U.mask @IO

-- | See @"Control.Exception".'Exc.mask_'@.
--
-- @since 1.0.0.0
mask_ :: Has (Lift IO) sig m => m a -> m a
mask_ = U.mask_ @IO

-- | See @"Control.Exception".uninterruptibleMask@.
--
-- @since 1.0.0.0
uninterruptibleMask :: Has (Lift IO) sig m => ((forall a . m a -> m a) -> m b) -> m b
uninterruptibleMask = U.uninterruptibleMask @IO

-- | See @"Control.Exception".'Exc.uninterruptibleMask_'@.
--
-- @since 1.0.0.0
uninterruptibleMask_ :: Has (Lift IO) sig m => m a -> m a
uninterruptibleMask_ = U.uninterruptibleMask_ @IO

-- | See @"Control.Exception".'Exc.getMaskingState'@.
--
-- @since 1.0.0.0
getMaskingState :: Has (Lift IO) sig m => m Exc.MaskingState
getMaskingState = U.getMaskingState @IO

-- | See @"Control.Exception".'Exc.interruptible'@.
--
-- @since 1.0.0.0
interruptible :: Has (Lift IO) sig m => m a -> m a
interruptible = U.interruptible @IO

-- | See @"Control.Exception".'Exc.allowInterrupt'@.
--
-- @since 1.0.0.0
allowInterrupt :: Has (Lift IO) sig m => m ()
allowInterrupt = U.allowInterrupt @IO

-- | See @"Control.Exception".'Exc.bracket'@.
--
-- @since 1.0.0.0
bracket
  :: Has (Lift IO) sig m
  => m a
  -> (a -> m b)
  -> (a -> m c)
  -> m c
bracket = U.bracket @IO

-- | See @"Control.Exception".'Exc.bracket_'@.
--
-- @since 1.0.0.0
bracket_
  :: Has (Lift IO) sig m
  => m a
  -> m b
  -> m c
  -> m c
bracket_ = U.bracket_ @IO

-- | See @"Control.Exception".'Exc.bracketOnError'@.
--
-- @since 1.0.0.0
bracketOnError
  :: Has (Lift IO) sig m
  => m a
  -> (a -> m b)
  -> (a -> m c)
  -> m c
bracketOnError = U.bracketOnError @IO

-- | See @"Control.Exception".'Exc.finally'@.
--
-- @since 1.0.0.0
finally
  :: Has (Lift IO) sig m
  => m a
  -> m b
  -> m a
finally = U.finally @IO

-- | See @"Control.Exception".'Exc.onException'@.
--
-- @since 1.0.0.0
onException :: Has (Lift IO) sig m => m a -> m b -> m a
onException = U.onException @IO
