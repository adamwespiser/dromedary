module Main where

import Lib
  (loadTest, makeConfig, runJobs)
import App (runAppM, Metric(report))
import Types (PassFail(Pass, Fail))

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Types.Status (Status(statusCode))


mkRequestToStatusCode :: (MonadIO m) => Request -> m Int
mkRequestToStatusCode req = httpBS req >>= pure . statusCode . getResponseStatus

reportStatusCode :: (Metric m, MonadIO m) => Integer -> Int -> m ()
reportStatusCode exp statusCode =
  if statusCode == 500 then report exp Fail else report exp Pass

solidRequestTask :: (Metric m, MonadIO m) => m ()
solidRequestTask = mkRequestToStatusCode "http://localhost:3000/solid" >>= reportStatusCode 1

flakyRequestTask :: (Metric m, MonadIO m) => m ()
flakyRequestTask = mkRequestToStatusCode "http://localhost:3000/flaky" >>= reportStatusCode 2

task1 :: (Metric m, MonadIO m) => m ()
task1 = do
  liftIO $ threadDelay 100000
  report 1 Pass
  report 1 Fail

main :: IO ()
main = do
  putStrLn "start"
  cfg <- makeConfig [1,2]
  runAppM (runJobs $ loadTest (solidRequestTask >> flakyRequestTask)) cfg
  print "done"

