{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
-- | Operations from "UnliftIO.Exception" lifted into effectful contexts using 'Control.Effect.Lift.Lift'.
--
-- @since 1.1.2.1
module Control.Effect.UnliftIO.Exception
( -- * Lifted "UnliftIO.Exception" operations
  throwIO
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
import           Control.Effect.Lift
import           Control.Exception hiding
  ( Handler
  , bracket
  , bracketOnError
  , bracket_
  , catch
  , catchJust
  , catches
  , evaluate
  , finally
  , handle
  , handleJust
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
import           Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO, withRunInIO)
import           Prelude hiding (ioError)
import qualified UnliftIO.Exception as Exc

-- | See @"UnliftIO.Exception".'Exc.throwIO'@.
--
-- @since 1.1.2.1
throwIO
  :: forall n e sig m a
   . (MonadUnliftIO n, Exc.Exception e, Has (Lift n) sig m)
  => e
  -> m a
throwIO = sendM @n . liftIO . Exc.throwIO

-- | See @"UnliftIO.Exception".'Exc.throwTo'@.
--
-- @since 1.1.2.1
throwTo
  :: forall n e sig m a
   . (MonadUnliftIO n, Exc.Exception e, Has (Lift n) sig m)
  => ThreadId
  -> e
  -> m ()
throwTo thread = sendM @n . liftIO . Exc.throwTo thread

-- | See @"UnliftIO.Exception".'Exc.catch'@.
--
-- @since 1.1.2.1
catch
  :: forall n e sig m a
   . (MonadUnliftIO n, Exc.Exception e, Has (Lift n) sig m)
  => m a
  -> (e -> m a)
  -> m a
catch m h = liftWith @n $
  \hdl ctx -> hdl (m <$ ctx) `Exc.catch` (hdl . (<$ ctx) . h)

-- | See @"UnliftIO.Exception".'Exc.catches'@.
--
-- @since 1.1.2.1
catches
  :: forall n sig m a
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m a
  -> [Exc.Handler m a]
  -> m a
catches m hs = liftWith @n $
  \ hdl ctx -> Exc.catches
    (hdl (m <$ ctx))
    (map (\ (Exc.Handler h) -> Exc.Handler (hdl . (<$ ctx) . h)) hs)

-- | See @"UnliftIO.Exception".'Exc.Handler'@.
--
-- @since 1.1.2.1
data Handler m a
  = forall e . Exc.Exception e => Handler (e -> m a)

deriving instance Functor m => Functor (Handler m)

-- | See @"UnliftIO.Exception".'Exc.catchJust'@.
--
-- @since 1.1.2.1
catchJust
  :: forall n e sig m a b
   . (MonadUnliftIO n, Exc.Exception e, Has (Lift n) sig m)
  => (e -> Maybe b)
  -> m a
  -> (b -> m a)
  -> m a
catchJust p m h = liftWith @n $
  \ hdl ctx -> Exc.catchJust p (hdl (m <$ ctx)) (hdl . (<$ ctx) . h)

-- | See @"UnliftIO.Exception".'Exc.handle'@.
--
-- @since 1.1.2.1
handle
  :: forall n e sig m a
   . (MonadUnliftIO n, Exc.Exception e, Has (Lift n) sig m)
  => (e -> m a)
  -> m a
  -> m a
handle = flip $ catch @n

-- | See @"UnliftIO.Exception".'Exc.handleJust'@.
--
-- @since 1.1.2.1
handleJust
  :: forall n e sig m a b
   . (MonadUnliftIO n, Exc.Exception e, Has (Lift n) sig m)
  => (e -> Maybe b)
  -> (b -> m a)
  -> m a
  -> m a
handleJust p = flip (catchJust @n p)

-- | See @"UnliftIO.Exception".'Exc.try'@.
--
-- @since 1.1.2.1
try
  :: forall n e sig m a b
   . (MonadUnliftIO n, Exc.Exception e, Has (Lift n) sig m)
  => m a
  -> m (Either e a)
try m = catch @n (Right <$> m) (pure . Left)

-- | See @"UnliftIO.Exception".'Exc.tryJust'@.
--
-- @since 1.1.2.1
tryJust
  :: forall n e sig m a b
   . (MonadUnliftIO n, Exc.Exception e, Has (Lift n) sig m)
  => (e -> Maybe b)
  -> m a
  -> m (Either b a)
tryJust p m = catchJust @n p (Right <$> m) (pure . Left)

-- | See @"UnliftIO.Exception".evaluate@.
--
-- @since 1.1.2.1
evaluate :: forall n sig m a. (MonadUnliftIO n, Has (Lift n) sig m) => a -> m a
evaluate = sendM @n . Exc.evaluate

-- | See @"UnliftIO.Exception".mask@.
--
-- @since 1.1.2.1
mask
  :: forall n sig m a b
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => ((forall a. m a -> m a) -> m b)
  -> m b
mask with = liftWith @n $ \ hdl ctx -> Exc.mask $ \ restore ->
  hdl (with (\ m -> liftWith $ \ hdl' ctx' -> restore (hdl' (m <$ ctx'))) <$ ctx)

-- | See @"UnliftIO.Exception".'Exc.mask_'@.
--
-- @since 1.1.2.1
mask_
  :: forall n sig m a
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m a
  -> m a
mask_ m = mask @n (const m)

-- | See @"UnliftIO.Exception".uninterruptibleMask@.
--
-- @since 1.1.2.1
uninterruptibleMask
  :: forall n sig m a b
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => ((forall a. m a -> m a) -> m b)
  -> m b
uninterruptibleMask with = liftWith @n $
  \ hdl ctx -> Exc.uninterruptibleMask $ \ restore ->
    hdl (with (\ m -> liftWith $
      \ hdl' ctx' -> restore (hdl' (m <$ ctx'))) <$ ctx)

-- | See @"UnliftIO.Exception".'Exc.uninterruptibleMask_'@.
--
-- @since 1.1.2.1
uninterruptibleMask_
  :: forall n sig m a
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m a
  -> m a
uninterruptibleMask_ m = uninterruptibleMask @n (const m)

-- | See @"UnliftIO.Exception".'Exc.bracket'@.
--
-- @since 1.1.2.1
bracket
  :: forall n sig m a b c
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m a
  -> (a -> m b)
  -> (a -> m c)
  -> m c
bracket acquire release m = liftWith @n $ \ hdl ctx ->
  Exc.bracket (hdl (acquire <$ ctx)) (hdl . (release <$>)) (hdl . (m <$>))

-- | See @"UnliftIO.Exception".'Exc.bracket_'@.
--
-- @since 1.1.2.1
bracket_
  :: forall n sig m a b c
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m a
  -> m b
  -> m c
  -> m c
bracket_ before after thing = bracket @n before (const after) (const thing)

-- | See @"UnliftIO.Exception".'Exc.bracketOnError'@.
--
-- @since 1.1.2.1
bracketOnError
  :: forall n sig m a b c
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m a
  -> (a -> m b)
  -> (a -> m c)
  -> m c
bracketOnError acquire release m = liftWith @n $ \ hdl ctx ->
  Exc.bracketOnError
    (hdl (acquire <$ ctx))
    (hdl . (release <$>))
    (hdl . (m <$>))

-- | See @"UnliftIO.Exception".'Exc.finally'@.
--
-- @since 1.1.2.1
finally
  :: forall n sig m a b
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m a
  -> m b
  -> m a
finally m sequel = liftWith @n $ \ hdl ctx ->
  Exc.finally (hdl (m <$ ctx)) (hdl (sequel <$ ctx))

-- | See @"UnliftIO.Exception".'Exc.onException'@.
--
-- @since 1.1.2.1
onException
  :: forall n sig m a b
   . (MonadUnliftIO n, Has (Lift n) sig m)
  => m a
  -> m b
  -> m a
onException io what = liftWith @n $ \ hdl ctx ->
  Exc.onException (hdl (io <$ ctx)) (hdl (what <$ ctx))
