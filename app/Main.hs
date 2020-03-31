module Main where

import Lib
  (loadTest, makeConfig, Metric(report), PassFail(Pass, Fail), runAppM, runJobs)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader

task1 :: (Metric m, MonadIO m) => m ()
task1 = do
  liftIO $ threadDelay 100000
  report 1 Pass
  report 1 Fail

main :: IO ()
main = do
  putStrLn "start"
  cfg <- makeConfig [1]
  runAppM (runJobs $ loadTest task1) cfg
  print "done"
