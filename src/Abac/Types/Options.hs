{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
module Abac.Types.Options where

import Data.Default (Default(..))
import qualified Abac.Options as Opt
import Abac.Types.ParserTypes
import Prelude hiding (Word)


--Option types

data Options =
  Options { adwordNo      :: Int
          , connectiveNo  :: Int
          , indexicalNo   :: Int
          , abstractNo    :: Int
          , prepositionNo :: Int
          , multiprepNo   :: Int
          , paralongNo    :: Int
          , sentlongNo    :: Int
          , sentshortNo   :: Int
          , verbweakNo    :: Int
          , verbpassNo    :: Int
          , countallNo    :: Int
          , countspecNo   :: Int
          } deriving (Show)

instance Default Options where
  def = defaultOptions

defaultOptions :: Options
defaultOptions =
  Options { adwordNo      = 12
          , connectiveNo  = 8
          , indexicalNo   = 2
          , abstractNo    = 10
          , prepositionNo = 12
          , multiprepNo   = 2
          , paralongNo    = 50
          , sentlongNo    = 35
          , sentshortNo   = 12
          , verbweakNo    = 12
          , verbpassNo    = 2
          , countallNo    = 0
          , countspecNo   = 0
          }


--keywords

data Keywords
  = Keywords
  { adwordKW      :: [Word]
  , connectiveKW  :: [Word]
  , indexicalKW   :: [Word]
  , abstractKW    :: [Word]
  , prepositionKW :: [Word]
  , verbweakKW    :: [Word]
  , pastpartKW    :: [Word]
  , auxiliarKW    :: [Word]
  , stopwordKW    :: [Word]
  , multiprepKW   :: [Word]
  , verbpassKW    :: [Word]
  , ngramKW       :: [Word]
  , tagKW         :: [Word]
  } deriving (Show)


instance Default Keywords where
  def = defaultKeywords

defaultKeywords :: Keywords
defaultKeywords = Keywords
  { adwordKW      = Opt.adwordKW
  , connectiveKW  = Opt.connectiveKW
  , indexicalKW   = Opt.indexicalKW
  , abstractKW    = Opt.abstractKW
  , prepositionKW = Opt.prepositionKW
  , verbweakKW    = Opt.verbweakKW
  , pastpartKW    = Opt.pastpartKW
  , auxiliarKW    = Opt.auxiliarKW
  , stopwordKW    = Opt.stopwordKW
  , multiprepKW   = []
  , verbpassKW    = []
  , ngramKW       = []
  , tagKW         = []
  }


