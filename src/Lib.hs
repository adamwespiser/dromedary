module Lib
    ( loadTest
    , Cfg(maxTime, count)
    , makeConfig
    , App
    , Metric(report)
    , PassFail(Pass,Fail)
    , runJobs
    ) where

import Types (Count, CountStruct, Cfg(Cfg, count, maxTime), PassFail(Pass,Fail), ppCountStruct, TaskId)
import App (App, Metric(report))

import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (MonadReader, asks, ask)
import Control.Concurrent (threadDelay)
import UnliftIO.Async (withAsync, wait)
import Control.Monad.IO.Unlift (MonadUnliftIO, askUnliftIO, withRunInIO)
import Control.Monad.IO.Unlift

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock
  (diffUTCTime, getCurrentTime, picosecondsToDiffTime, secondsToNominalDiffTime, UTCTime)
import GHC.Conc
  (atomically, newTVar, readTVar, readTVarIO, writeTVar, TVar)

------------------------------------------------------------
-- Controller
newTaskMap :: Map.Map PassFail Integer
newTaskMap = Map.empty

makeConfig :: [TaskId] -> IO Cfg
makeConfig ids = do
 let counter = Map.fromList $ zip ids $ repeat newTaskMap
 shared <- atomically $ newTVar counter
 return $ Cfg 1 shared

-- | loadTest: take a monadIO action and run it for the duration
loadTest :: (MonadReader Cfg m, Metric m, MonadIO m) => m () -> m ()
loadTest action = do
  start <- liftIO $ getCurrentTime
  loop start action
  where
    loop :: (Metric m, MonadIO m, MonadReader Cfg m) => UTCTime -> m () -> m ()
    loop start action = do
      maxtime <- asks maxTime
      now <- liftIO $ getCurrentTime
      case diffUTCTime now start > (secondsToNominalDiffTime $ fromInteger $ maxtime) of
        False -> action  >> loop start action
        True -> pure ()


runJobs :: (MonadUnliftIO  m, MonadReader Cfg m, Metric m, MonadIO m) => m () -> m ()
runJobs task = do
 cfg <- ask
 withAsync (loadTest task) $ \thread -> do
   wait thread
   countVar <- asks count
   unwrapCnt <- liftIO $ readTVarIO $ countVar
   liftIO $ ppCountStruct $ unwrapCnt
   pure ()

