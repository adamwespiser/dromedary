-- | Description : Basic types for keeping track of our IO actions
{-# LANGUAGE NoImplicitPrelude #-}

module Types
  ( TaskId
  , Count
  , CountStruct
  , PassFail(Pass, Fail)
  , ppCountStruct
  , Cfg(Cfg, maxTime, count)
  ) where

import Prelude hiding ((<$>))

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Conc (TVar)
import Text.PrettyPrint.ANSI.Leijen

type TaskId = Integer
type Count = TVar CountStruct
type CountStruct = Map.Map TaskId (Map.Map PassFail Integer)

ppCountStruct :: CountStruct -> IO ()
ppCountStruct map = putDoc $ align (Map.foldlWithKey ppFold (text "Results") map <> linebreak)
  where
    ppFold :: Doc -> TaskId -> Map.Map PassFail Integer -> Doc
    ppFold doc taskId result = doc
     <$> text "Experiment" <+> integer taskId
     <$> indent 4 (ppResult result)
    ppResult :: Map.Map PassFail Integer -> Doc
    ppResult map =
      let pass = fromMaybe 0 $ Map.lookup Pass map
          fail = fromMaybe 0 $ Map.lookup Fail map
      in text "Pass:" <+> integer pass <$> text "Fail:" <+> integer fail

data PassFail = Pass | Fail
  deriving (Show, Eq, Ord, Enum)

data Cfg = Cfg {
  maxTime  :: Integer
, count :: Count
}
