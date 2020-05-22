{-# LANGUAGE DataKinds, DeriveGeneric, LambdaCase, TypeOperators #-}

{-| Description: A simple server for mock load testing.
This is a simple server for to "test" load testing.
It support two GET request:
  /solid - which always returns 'NoContent'
  /flaky - which returns (NoContent,500 err) w/ 50% probability
-}

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
  putStrLn "starting server..."
  runSettings settings =<< mkApp

type TestApi =
  "solid" :>  Get '[JSON] NoContent :<|>
  "flaky" :>  Get '[JSON] NoContent

api :: Proxy TestApi
api = Proxy

mkApp :: IO Application
mkApp = return $ serve api server

server :: Server TestApi
server = solid :<|> flaky
  where
    solid :: Handler NoContent
    solid = pure NoContent

    flaky :: Handler NoContent
    flaky = do
      gen <- liftIO $ getStdRandom (randomR (0,1))
      if ((gen :: Integer) == 1) then return NoContent else throwError err500

