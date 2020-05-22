module App
  ( App
  , Metric(report)
  , runAppM
  ) where

import Types (Cfg(Cfg, count), PassFail(Pass,Fail), TaskId)

import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.IO.Unlift (MonadUnliftIO, askUnliftIO, withRunInIO)
import Control.Monad.IO.Unlift

import qualified Data.Map.Strict as Map

import GHC.Conc
  (atomically,  readTVar,  writeTVar)

newtype App m a = AppM { unAppM :: ReaderT Cfg m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Cfg)

instance MonadUnliftIO m => MonadUnliftIO (App m) where
  askUnliftIO = AppM $ ReaderT $ \r ->
                withUnliftIO $ \u ->
                return (UnliftIO (unliftIO u . flip runReaderT r . unAppM))
  withRunInIO inner =
    AppM $ ReaderT $ \r ->
    withRunInIO $ \run ->
    inner (run . flip runReaderT r . unAppM)

runAppM :: App IO a -> Cfg -> IO a
runAppM app cfg = runReaderT (unAppM app) cfg

class (Monad m, MonadReader Cfg m) => Metric m where
  report :: TaskId -> PassFail -> m ()

instance (Monad m, MonadIO m) =>  Metric (App m) where
  report task passfail = do
    countVar <- asks count
    liftIO $ atomically $ readTVar countVar
      >>= writeTVar countVar . Map.update (\x -> Just $ Map.insertWith (+) passfail 1 x) task
    pure ()

