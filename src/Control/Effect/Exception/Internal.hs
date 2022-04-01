{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- | Operations from "UnliftIO.Exception" lifted into effectful contexts using 'Control.Effect.Lift.Lift'.
--
-- These methods are shamelessly copied from the @unliftio@ module in an effort to keep dependencies small.
-- @unliftio-core@ is assumed fair game though considering it's included in core @fused-effects@ package.
--
-- @since <version>
module Control.Effect.Exception.Internal
( -- * Throwing
  throwIO
  -- * Catching (with recovery)
, catch
, catchJust
, Handler (..)
, catches
  -- * Evaluation
, evaluate
  -- * Masking
, mask
, uninterruptibleMask
  -- * Reexports
, Exception (..)
, Typeable
, SomeException (..)
, SomeAsyncException (..)
, IOException
) where

import           Control.Exception (Exception(..), IOException, SomeAsyncException(..), SomeException(..))
import qualified Control.Exception as EUnsafe
import           Control.Monad.IO.Unlift
import           Data.Typeable (Typeable, cast)

-- | Catch a synchronous (but not asynchronous) exception and recover from it.
--
-- This is parameterized on the exception type. To catch all synchronous exceptions,
-- use 'catchAny'.
catch
  :: (MonadUnliftIO m, Exception e)
  => m a -- ^ action
  -> (e -> m a) -- ^ handler
  -> m a
catch f g = withRunInIO $ \run -> run f `EUnsafe.catch` \e ->
  if isSyncException e
    then run (g e)
    -- intentionally rethrowing an async exception synchronously,
    -- since we want to preserve async behavior
    else EUnsafe.throwIO e

-- | 'catchJust' is like 'catch' but it takes an extra argument which
-- is an exception predicate, a function which selects which type of
-- exceptions we're interested in.
catchJust :: (MonadUnliftIO m, Exception e) => (e -> Maybe b) -> m a -> (b -> m a) -> m a
catchJust f a b = a `catch` \e -> maybe (liftIO (throwIO e)) b $ f e

-- | A helper data type for usage with 'catches' and similar functions.
data Handler m a = forall e . Exception e => Handler (e -> m a)

-- | Internal.
catchesHandler :: MonadIO m => [Handler m a] -> SomeException -> m a
catchesHandler handlers e = foldr tryHandler (liftIO (EUnsafe.throwIO e)) handlers
    where tryHandler (Handler handler) res
              = case fromException e of
                Just e' -> handler e'
                Nothing -> res

-- | Similar to 'catch', but provides multiple different handler functions.
--
-- For more information on motivation, see @base@'s 'EUnsafe.catches'. Note that,
-- unlike that function, this function will not catch asynchronous exceptions.
catches :: MonadUnliftIO m => m a -> [Handler m a] -> m a
catches io handlers = io `catch` catchesHandler handlers

-- | Lifted version of 'EUnsafe.evaluate'.
evaluate :: MonadIO m => a -> m a
evaluate = liftIO . EUnsafe.evaluate

-- | Synchronously throw the given exception.
--
-- Note that, if you provide an exception value which is of an asynchronous
-- type, it will be wrapped up in 'SyncExceptionWrapper'. See 'toSyncException'.
throwIO :: (MonadIO m, Exception e) => e -> m a
throwIO = liftIO . EUnsafe.throwIO . toSyncException

-- | Wrap up an asynchronous exception to be treated as a synchronous
-- exception.
--
-- This is intended to be created via 'toSyncException'.
data SyncExceptionWrapper = forall e. Exception e => SyncExceptionWrapper e
    deriving Typeable

instance Show SyncExceptionWrapper where
    show (SyncExceptionWrapper e) = show e

instance Exception SyncExceptionWrapper where
#if MIN_VERSION_base(4,8,0)
    displayException (SyncExceptionWrapper e) = displayException e
#endif

-- | Convert an exception into a synchronous exception.
--
-- For synchronous exceptions, this is the same as 'toException'.
-- For asynchronous exceptions, this will wrap up the exception with
-- 'SyncExceptionWrapper'.
toSyncException :: Exception e => e -> SomeException
toSyncException e =
    case fromException se of
        Just (SomeAsyncException _) -> toException (SyncExceptionWrapper e)
        Nothing                     -> se
  where
    se = toException e

-- | Check if the given exception is synchronous.
isSyncException :: Exception e => e -> Bool
isSyncException e =
    case fromException (toException e) of
        Just (SomeAsyncException _) -> False
        Nothing                     -> True

-- | Unlifted version of 'EUnsafe.mask'.
mask :: MonadUnliftIO m => ((forall a. m a -> m a) -> m b) -> m b
mask f = withRunInIO $ \run -> EUnsafe.mask $ \unmask ->
  run $ f $ liftIO . unmask . run

-- | Unlifted version of 'EUnsafe.uninterruptibleMask'.
uninterruptibleMask :: MonadUnliftIO m => ((forall a. m a -> m a) -> m b) -> m b
uninterruptibleMask f = withRunInIO $ \run -> EUnsafe.uninterruptibleMask $ \unmask ->
  run $ f $ liftIO . unmask . run
