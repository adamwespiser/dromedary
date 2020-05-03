{-# LANGUAGE DataKinds, DeriveGeneric, LambdaCase, TypeOperators #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Servant
import Network.Wai.Handler.Warp
  (defaultSettings, runSettings, setPort)
import System.Random

main :: IO ()
main = do
  let port = 3000
      settings = defaultSettings & setPort port
  runSettings settings =<< mkApp

type TestApi =
  "solid" :> Capture "id" Integer :> Get '[JSON] Integer :<|>
  "flaky" :> Capture "id" Integer :> Get '[JSON] Integer

api :: Proxy TestApi
api = Proxy

mkApp :: IO Application
mkApp = return $ serve api server

server :: Server TestApi
server = solid :<|> flaky
  where
    solid :: Integer -> Handler Integer
    solid i = return i

    flaky :: Integer -> Handler Integer
    flaky i = do
      gen <- liftIO $ getStdRandom (randomR (0,1))
      if ((gen :: Integer) == 1) then return 1 else throwError err500

