{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
-- | Operations from "Control.Exception" and "UnliftIO.Exception" lifted into effectful contexts using 'Control.Effect.Lift.Lift'.
--
-- @since 1.1.2.0
module Control.Effect.Exception.UnliftIO
( -- * Lifted "Control.Exception" operations
  throwIO
, ioError
, throwTo
, catch
, catches
, Handler(..)
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
import qualified Control.Effect.Exception.Internal as Exc
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
import qualified Control.Exception as EUnsafe
import           Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO, withRunInIO)
import           Prelude hiding (ioError)

-- | See @"Unlift.Exception".throwIO@.
--
-- @since 1.1.2.0
throwIO
  :: forall n e sig m a
   . (MonadUnliftIO n, Exc.Exception e, Has (Lift n) sig m)
  => e
  -> m a
throwIO = sendM @n . Exc.throwIO

-- | See @"Control.Exception".'Exc.ioError'@.
--
-- @since 1.1.2.0
ioError
  :: forall n sig m a
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => IOError
  -> m a
ioError = sendM @n . liftIO . EUnsafe.ioError

-- | See @"Control.Exception".'Exc.throwTo'@.
--
-- @since 1.1.2.0
throwTo
  :: forall n e sig m a
   . (MonadUnliftIO n, Exc.Exception e, Has (Lift n) sig m)
  => ThreadId
  -> e
  -> m ()
throwTo thread = sendM @n . liftIO . EUnsafe.throwTo thread

-- | See @"UnliftIO.Exception".catch@.
--
-- @since 1.1.2.0
catch
  :: forall n e sig m a
   . (MonadUnliftIO n, Exc.Exception e, Has (Lift n) sig m)
  => m a
  -> (e -> m a)
  -> m a
catch m h = liftWith @n $
  \run ctx -> run (m <$ ctx) `Exc.catch` (run . (<$ ctx) . h)

-- | See @"UnliftIO.Exception".catches@.
--
-- @since 1.1.2.0
catches
  :: forall n sig m a
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m a
  -> [Handler m a]
  -> m a
catches m hs = liftWith @n $
  \ run ctx -> Exc.catches
    (run (m <$ ctx))
    (map (\ (Handler h) -> Exc.Handler (run . (<$ ctx) . h)) hs)

-- | See @"Control.Exception".'Exc.Handler'@.
--
-- @since <version>
data Handler m a
  = forall e . Exc.Exception e => Handler (e -> m a)

deriving instance Functor m => Functor (Handler m)

-- | See @"UnliftIO.Exception".catchJust@.
--
-- @since 1.1.2.0
catchJust
  :: forall n e sig m a b
   . (MonadUnliftIO n, Exc.Exception e, Has (Lift n) sig m)
  => (e -> Maybe b)
  -> m a
  -> (b -> m a)
  -> m a
catchJust p m h = liftWith @n $
  \ run ctx -> Exc.catchJust p (run (m <$ ctx)) (run . (<$ ctx) . h)

-- | See @"Control.Exception".'Exc.handle'@.
--
-- @since 1.1.2.0
handle
  :: forall n e sig m a
   . (MonadUnliftIO n, Exc.Exception e, Has (Lift n) sig m)
  => (e -> m a)
  -> m a
  -> m a
handle = flip $ catch @n

-- | See @"Control.Exception".'Exc.handleJust'@.
--
-- @since 1.1.2.0
handleJust
  :: forall n e sig m a b
   . (MonadUnliftIO n, Exc.Exception e, Has (Lift n) sig m)
  => (e -> Maybe b)
  -> (b -> m a)
  -> m a
  -> m a
handleJust p = flip (catchJust @n p)

-- | See @"Control.Exception".'Exc.try'@.
--
-- @since 1.1.2.0
try
  :: forall n e sig m a b
   . (MonadUnliftIO n, Exc.Exception e, Has (Lift n) sig m)
  => m a
  -> m (Either e a)
try m = catch @n (Right <$> m) (pure . Left)

-- | See @"Control.Exception".'Exc.tryJust'@.
--
-- @since 1.1.2.0
tryJust
  :: forall n e sig m a b
   . (MonadUnliftIO n, Exc.Exception e, Has (Lift n) sig m)
  => (e -> Maybe b)
  -> m a
  -> m (Either b a)
tryJust p m = catchJust @n p (Right <$> m) (pure . Left)

-- | See @"UnliftIO.Exception".evaluate@.
--
-- @since 1.1.2.0
evaluate :: forall n sig m a. (MonadUnliftIO n, Has (Lift n) sig m) => a -> m a
evaluate = sendM @n . Exc.evaluate

-- | See @"UnliftIO.Exception".mask@.
--
-- @since 1.1.2.0
mask
  :: forall n sig m a b
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => ((forall a . m a -> m a) -> m b)
  -> m b
mask with = liftWith @n $ \ run ctx -> Exc.mask $ \ restore ->
  run (with (\ m -> liftWith $ \ run' ctx' -> restore (run' (m <$ ctx'))) <$ ctx)

-- | See @"Control.Exception".'Exc.mask_'@.
--
-- @since 1.1.2.0
mask_
  :: forall n sig m a
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m a
  -> m a
mask_ m = mask @n (const m)

-- | See @"UnliftIO.Exception".uninterruptibleMask@.
--
-- @since 1.1.2.0
uninterruptibleMask
  :: forall n sig m a b
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => ((forall a . m a -> m a) -> m b)
  -> m b
uninterruptibleMask with = liftWith @n $
  \ run ctx -> Exc.uninterruptibleMask $ \ restore ->
    run (with (\ m -> liftWith $
      \ run' ctx' -> restore (run' (m <$ ctx'))) <$ ctx)

-- | See @"Control.Exception".'Exc.uninterruptibleMask_'@.
--
-- @since 1.1.2.0
uninterruptibleMask_
  :: forall n sig m a
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m a
  -> m a
uninterruptibleMask_ m = uninterruptibleMask @n (const m)

-- | See @"Control.Exception".'Exc.getMaskingState'@.
--
-- @since 1.1.2.0
getMaskingState
  :: forall n sig m
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m EUnsafe.MaskingState
getMaskingState = sendM @n (liftIO EUnsafe.getMaskingState)

-- | See @"Control.Exception".'Exc.interruptible'@.
--
-- @since 1.1.2.0
interruptible
  :: forall n sig m a
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m a
  -> m a
interruptible m = liftWith @n $ \ run ctx -> withRunInIO $ \runInIO ->
  EUnsafe.interruptible (runInIO $ run (m <$ ctx))

-- | See @"Control.Exception".'Exc.allowInterrupt'@.
--
-- @since 1.1.2.0
allowInterrupt
  :: forall n sig m a
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m ()
allowInterrupt = sendM @n (liftIO EUnsafe.allowInterrupt)

-- | See @"Control.Exception".'Exc.bracket'@.
--
-- @since 1.1.2.0
bracket
  :: forall n sig m a b c
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m a
  -> (a -> m b)
  -> (a -> m c)
  -> m c
bracket acquire release m = mask @n $ \ restore -> do
  a <- acquire
  r <- onException @n (restore $ m a) (release a)
  r <$ release a

-- | See @"Control.Exception".'Exc.bracket_'@.
--
-- @since 1.1.2.0
bracket_
  :: forall n sig m a b c
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m a
  -> m b
  -> m c
  -> m c
bracket_ before after thing = bracket @n before (const after) (const thing)

-- | See @"Control.Exception".'Exc.bracketOnError'@.
--
-- @since 1.1.2.0
bracketOnError
  :: forall n sig m a b c
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m a
  -> (a -> m b)
  -> (a -> m c)
  -> m c
bracketOnError acquire release m = mask @n $ \ restore -> do
  a <- acquire
  onException @n (restore $ m a) (release a)

-- | See @"Control.Exception".'Exc.finally'@.
--
-- @since 1.1.2.0
finally
  :: forall n sig m a b
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m a
  -> m b
  -> m a
finally m sequel = mask @n $
  \ restore -> onException @n (restore m) sequel <* sequel

-- | See @"Control.Exception".'Exc.onException'@.
--
-- @since 1.1.2.0
onException
  :: forall n sig m a b
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m a
  -> m b
  -> m a
onException io what = catch @n io $
  \e -> what >> throwIO @n @Exc.SomeException e
