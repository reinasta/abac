{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}
module Abac.PrettyResults where

import Data.Char (toUpper,toLower)
import Text.Printf
import Text.PrettyPrint
import qualified Data.Text as T
import Prelude hiding (showList,Word)

import Abac.Types.ParserTypes
import Abac.Types
import Abac.Traverse (getPosition)

showRes :: FinalResult -> Doc
showRes (ListResult prt) = showPart prt
showRes (CountResult n) = int n <+> text "words"
showRes (TimeResult n) = int n <+> text "minutes"
showRes (PercentResult n) = double (twoPrecisionPoints n) <> "%"

twoPrecisionPoints :: Double -> Double
twoPrecisionPoints dbl = read $ printf "%.2f" dbl

showPart :: ParaPart -> Doc
showPart el =
  let putTogether = hsep . punctuate comma
  in case el of
       (Inlines wrds)     -> putTogether $ showWords wrds
       (Sentence wrds)    -> putTogether $ showWords wrds
       (Expressions exps) -> vcat $ fmap showExpression exps
       _                  -> empty

showExpression :: Expression -> Doc
showExpression expr@(Expression wrds) =
  let pos = text $ posToStr $ fst $ getPosition expr
  in  hsep $ pos : fmap showWordSansPos wrds

showExpressionTitle :: Expression -> Doc
showExpressionTitle expr@(Expression wrds) =
  let pos = text $ posToStr $ fst $ getPosition expr
  in  hsep $ pos : fmap showWordSansPosTitle wrds

showWords :: [Inline] -> [Doc]
showWords inls = fmap showWord inls

showWord :: Inline -> Doc
showWord inl = text $ wordToStr inl

showWordSansPos :: Inline -> Doc
showWordSansPos inl = text $ wordToStrSansPos inl

showWordSansPosTitle :: Inline -> Doc
showWordSansPosTitle inl = text $ wordToStrSansPosTitle inl

wordToStrSansPosTitle :: Inline -> String
wordToStrSansPosTitle inl = toTitle $ wordToStrSansPos inl
  where toTitle [] = []
        toTitle (c:str) = toUpper c : fmap toLower str

wordToStrSansPos :: Inline -> String
wordToStrSansPos (Word _ _ txt) = T.unpack txt
wordToStrSansPos (Citation _ _ _ txt) = T.unpack txt
wordToStrSansPos (InlineTech _ _ txt) = T.unpack txt
wordToStrSansPos (InlineComment inls) = concatMap ((++ " ") . wordToStrSansPos) inls
wordToStrSansPos (Punct _ txt) = T.unpack txt
wordToStrSansPos (Number _ _ n) = show n
wordToStrSansPos (Link _ urlid) = T.unpack urlid
wordToStrSansPos (Image _ pthid) = T.unpack pthid
wordToStrSansPos (Email _ _ nom dom) = T.unpack nom ++ "@" ++ T.unpack dom
wordToStrSansPos _ = ""

wordToStr :: Inline -> String
wordToStr (Word _ pos txt) = posToStr pos ++ " " ++ T.unpack txt
wordToStr (Citation _ _ pos txt) = posToStr pos ++ " " ++ T.unpack txt
wordToStr (InlineTech _ pos txt) = posToStr pos ++ " " ++ T.unpack txt
wordToStr (InlineComment inls) = concatMap ((++ " ") . wordToStr) inls
wordToStr (Punct _ txt) = T.unpack txt
wordToStr (Number _ pos n) = posToStr pos ++ " " ++ show n
wordToStr (Link _ urlid) = T.unpack urlid
wordToStr (Image _ pthid) = T.unpack pthid
wordToStr (Email _ pos nom dom) = posToStr pos ++ " " ++ T.unpack nom ++ "@" ++ T.unpack dom
wordToStr _ = ""

posToStr :: Position -> String
posToStr (l,c) = show l ++ ":" ++ show c
