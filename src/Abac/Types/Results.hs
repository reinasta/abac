{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
module Abac.Types.Results where

import qualified Data.Map.Strict as M
import Abac.Types.ParserTypes
import Prelude hiding (Word)

type Map = M.Map

-- auxiliary types

type ListResult = FinalResult
type CountResult = FinalResult
type PercentResult = FinalResult
type TimeResult = FinalResult

data FinalResult = ListResult ParaPart
                 | CountResult Int
                 | PercentResult Double
                 | TimeResult Int
                 deriving (Show,Eq)

data Res =
  Res { resElems :: ListResult -- a
      , resCount :: CountResult -- Int
      , resPercent :: PercentResult -- Double
      } deriving (Show)


-- first parameters of expression and position filters
data OptVal = TextParam Text
            | NumParam Int
            | TagParam Text
            deriving (Show,Eq)

type TextParam = OptVal
type NumParam = OptVal
type TagParam = OptVal

-- types for the tree-output (inlines, paragraph, section) of functions hosted in position filters
data InterimTree = InterimInlines Inlines
                 | InterimParagraph Paragraph
                 | InterimSection Section
                 | InterimDocument Document
                 deriving (Show,Eq)

type InterimInlines = InterimTree
type InterimParagraph = InterimTree
type InterimSection = InterimTree

-- alternative result types for a generalized command-to-results function
-------------------------------------------------------------------------

type InterimInline' = FinalResult'
type InterimParaPart' = FinalResult'
type InterimBlock' = FinalResult'
type InterimSection' = FinalResult'
type InterimDocument' = FinalResult'
--type InterimCount' = FinalResult'
--type InterimPercent' = FinalResult'
--type InterimTime' = FinalResult'
type CountResult' = FinalResult'
type PercentResult' = FinalResult'
type TimeResult' = FinalResult'


data FinalResult' = InterimInline' Inline
                  | InterimParaPart' ParaPart
                  | InterimBlock' Block
                  | InterimSection' Section
                  | InterimDocument' Document
                  | CountResult' Int
                  | PercentResult' Double
                  | TimeResult' Int
                  | NullResult'
                  deriving (Show,Eq)

data Res' =
  Res' { resInline   :: InterimInline'    -- Inline
       , resPart     :: InterimParaPart'  -- ParaPart
       , resBlock    :: InterimBlock'     -- Block
       , resSection  :: InterimSection'   -- Section
       , resDocument :: InterimDocument'  -- Document
       , resCount'   :: CountResult'      -- Int
       , resPercent' :: PercentResult'    -- Double
       , resTime     :: TimeResult'       -- Int
       } deriving (Show)

defRes :: Res'
defRes =
  Res' { resInline = InterimInline' Null
       , resPart = InterimParaPart' NullPart
       , resBlock = InterimBlock' (Para [])
       , resSection = InterimSection' (SecBlocks [])
       , resDocument = InterimDocument' (Doc [])
       , resCount' = CountResult' 0
       , resPercent' = PercentResult' 0
       , resTime = TimeResult' 0
       }

-- return the deepest tree which has a non-default value
nondefTreeRes :: Res' -> FinalResult'
nondefTreeRes res | resDocument res /= resDocument defRes = resDocument res
                  | resSection res /= resSection defRes   = resSection res
                  | resBlock res /= resBlock defRes       = resBlock res
                  | resPart res /= resPart defRes         = resPart res
                  | resInline res /= resInline defRes     = resInline res
                  | otherwise                             = NullResult'

hasNonDefaultTree :: Res' -> Bool
hasNonDefaultTree res = nondefTreeRes res /= NullResult'

