{-# Language DerivingStrategies, DerivingVia, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, StandaloneDeriving, UndecidableInstances, OverloadedStrings  #-}

module Lib
    ( loadTest
    , Cfg(maxTime, count)
    , makeConfig
    , App
    , Metric(report)
    , PassFail(Pass,Fail)
    , runAppM
    , runJobs
    ) where

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
-- Types
type TaskId = Integer
type Count = TVar CountStruct
type CountStruct = Map.Map TaskId (Map.Map PassFail Integer)

data PassFail = Pass | Fail
  deriving (Show, Eq, Ord, Enum)

data Cfg = Cfg {
  maxTime  :: Integer
, count :: Count
}


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
  liftIO $ print start
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
   liftIO $ putStrLn $ show $ unwrapCnt
   pure ()



getStats :: (MonadReader Cfg m, Metric m, MonadIO m) => m T.Text
getStats = do
  countVar <- asks count
  unwrapCnt <- liftIO $ readTVarIO $ countVar

  let ks = Map.keys unwrapCnt
  pure $ Map.foldr' (\a b -> expToText a <> b) "" unwrapCnt
  -- pure $ T.pack $ show ks
  where
    expToText :: Map.Map PassFail Integer -> T.Text
    expToText map =
      let passNum = Map.findWithDefault 0 Pass map
          failNum = Map.findWithDefault 0 Fail map
      in T.concat $ fmap T.pack $ [ "Pass: " ++ show passNum ++ "Fail: " ++ show failNum]

------------------------------------------------------------
-- Classes

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


