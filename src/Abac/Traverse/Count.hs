{-# LANGUAGE OverloadedStrings #-}
module Abac.Traverse.Count where

import qualified Data.Map.Strict as M
import qualified Data.Text as T (Text, splitOn, length, isSuffixOf,
                                 empty, append, intercalate,
                                 replicate, unwords, pack, unpack,
                                 count, concat, cons, snoc,)
import Prelude hiding (Word)




import Abac.Types.ParserTypes
--import Abac.Internal

--import Abac.Traverse.Searchable

import Abac.Traverse.Internal

--versions of countElemOf

countElemsOfInInlines :: [Word] -> [Inline] -> Int
countElemsOfInInlines ws inls = sum [1 | w <- ws, any (== w) (removeInlineWrapers inls)]

-- countElemsOfInInlines ws (inl:inls) =
--   case inl of
--     (Link inls' _)   -> countElemsOfInInlines ws inls' + countElemsOfInInlines ws inls
--     (Image inls' _)  -> countElemsOfInInlines ws inls' + countElemsOfInInlines ws inls
--     _                -> sum [1 | w <- ws, w == inl] + countElemsOfInInlines ws inls


countElemsOfInExpr :: [Word] -> Expression -> Int
countElemsOfInExpr ws (Expression inls) = countElemsOfInInlines ws inls

countElemsOfInSent :: [Word] -> Sentence -> Int
countElemsOfInSent ws (Sentence inls) = countElemsOfInInlines ws inls
countElemsOfInSent _ _ = 0

countElemsOfInSents :: [Word] -> [Sentence] -> Int
countElemsOfInSents ws snts = sum $ countElemsOfInSent ws <$> snts

-- countElemsOfInExample :: [Word] -> Example -> Int
-- countElemsOfInExample ws (MainEx _ _ _ snts) = sum $ countElemsOfInSent ws <$> snts
-- countElemsOfInExample ws (SubEx _ _ _ snts) = sum $ countElemsOfInSent ws <$> snts
--
-- countElemsOfInExamples :: [Word] -> [Example] -> Int
-- countElemsOfInExamples ws exs = sum $ countElemsOfInExample ws <$> exs

countElemsOfInParaPart :: [Word] -> ParaPart -> Int
countElemsOfInParaPart ws (ParaFtn _ snts)  = countElemsOfInSents ws snts
countElemsOfInParaPart ws (Caption parprts) = sum $ countElemsOfInParaPart ws <$> parprts
countElemsOfInParaPart ws snt@(Sentence _) = countElemsOfInSent ws snt
countElemsOfInParaPart _ _ = 0

countElemsOfInPara :: [Word] -> Paragraph -> Int
countElemsOfInPara ws (Para prts) = sum $ countElemsOfInParaPart ws <$> prts
countElemsOfInPara _ _ = 0

countElemsOfInBlock :: [Word] -> Block -> Int
countElemsOfInBlock ws par@(Para _) = countElemsOfInPara ws par
countElemsOfInBlock ws (Footnote _ blcs) = sum $ countElemsOfInBlock ws <$> blcs
countElemsOfInBlock ws (BlockEx exs) = sum $ countElemsOfInParaPart ws <$> unwrapExamples exs
countElemsOfInBlock ws (BlockQuotes blcs) = sum $ countElemsOfInBlock ws <$> blcs
countElemsOfInBlock ws (BlockQuote par) = countElemsOfInPara ws par
countElemsOfInBlock ws (Table prts) = sum $ countElemsOfInParaPart ws <$> prts
countElemsOfInBlock _ (BlockTech _ _ _) = 0
countElemsOfInBlock _ (LinkRef _ _ _) = 0
countElemsOfInBlock _ (ImageRef _ _ _) = 0
countElemsOfInBlock _ (BlockComment _) = 0

countElemsOfInBlocks :: [Word] -> [Block] -> Int
countElemsOfInBlocks ws blcs = sum $ countElemsOfInBlock ws <$> blcs

countElemsOfInSection :: [Word] -> Section -> Int
countElemsOfInSection ws (Section _ _ (Title inls) bdy subs) =
  countElemsOfInInlines ws inls +
  countElemsOfInBlocks ws (secblcs . secbdy $ bdy) +
  countElemsOfInSections ws subs
countElemsOfInSection ws (SecBlocks blcs) = countElemsOfInBlocks ws blcs
countElemsOfInSection ws (Meta mp) = titleCount ws mp + abstractCount ws mp + tagCount ws mp

--meta fields

titleCount :: [Word] -> M.Map MetaKey MetaValue -> Int
titleCount ws mp = procTitle mp (countElemsOfInInlines ws) 0

abstractCount :: [Word] -> M.Map MetaKey MetaValue -> Int
abstractCount ws mp = procAbstract mp (countElemsOfInBlocks ws) 0

tagCount :: [Word] -> M.Map MetaKey MetaValue -> Int
tagCount ws mp = procTags mp (countElemsOfInInlines ws) 0

countElemsOfInSections :: [Word] -> [Section] -> Int
countElemsOfInSections ws secs = sum $ countElemsOfInSection ws <$> secs

countElemsOfInDocument :: [Word] -> Document -> Int
countElemsOfInDocument ws (Doc secs) = countElemsOfInSections ws secs

--formerly quantifyWordsIn... functions

countWordsInInlines :: [Inline] -> Int
countWordsInInlines = length . wordlikeInInlines . removeInlineWrapers

countWordsInSent :: Sentence -> Int
countWordsInSent (Sentence inls) = countWordsInInlines inls
countWordsInSent _ = 0

countWordsInSents :: [Sentence] -> Int
countWordsInSents snts = sum $ countWordsInSent <$> snts

-- countWordsInExample :: Example -> Int
-- countWordsInExample (MainEx _ _ _ snts) = countWordsInSents snts
-- countWordsInExample (SubEx _ _ _ snts) = countWordsInSents snts
--
-- countWordsInExamples :: [Example] -> Int
-- countWordsInExamples exs = sum $ countWordsInExample <$> exs

countWordsInParaParts :: ParaPart -> Int
countWordsInParaParts snt@(Sentence _) = countWordsInSent snt
countWordsInParaParts (ParaFtn _ snts) = countWordsInSents snts
countWordsInParaParts (Inlines inls) = countWordsInInlines inls
countWordsInParaParts (Caption prts) = sum $ countWordsInParaParts <$> prts
countWordsInParaParts _ = 0



countWordsInPar :: Paragraph -> Int
countWordsInPar (Para prts) = sum $ countWordsInParaParts <$> prts
countWordsInPar _ = 0

countWordsInBlock :: Block -> Int
countWordsInBlock par@(Para _) = countWordsInPar par
countWordsInBlock (Footnote _ blcs) = countWordsInBlocks blcs
countWordsInBlock (BlockEx exs) = sum $ countWordsInParaParts <$> unwrapExamples exs
countWordsInBlock (LinkRef _ _ _) = 1
countWordsInBlock (ImageRef _ _ _) = 1
countWordsInBlock (BlockQuote par) = countWordsInPar par
countWordsInBlock (BlockComment _) = 0
countWordsInBlock (Table parprts) = sum $ countWordsInParaParts <$> parprts
countWordsInBlock (BlockQuotes blcs) = countWordsInBlocks blcs
countWordsInBlock (BlockTech _ _ txt) = ceiling $ mathChars txt / 7
  where
    mathChars :: Text -> Double
    mathChars t = fromIntegral $ T.length t - numToDiscount t
    numToDiscount t = sum $ howManyChars <$> latexMathMacros <*> [t]
    howManyChars = (\x y -> T.length x * T.count x y) :: Text -> Text -> Int
    latexMathMacros = ["mbox", "left", "right", "equation","begin",
                       "end", "big", "mathrm", "textit", "textrm",
                       "textbf", "text", "mathtt", "mathfrak",
                       "mathcal", "cases"] :: [Text]


--NB: for tech blocks we count the number of characters and divided by 7 (the average
--length of an English word (circa 5) slightly ajusted to account for the verbosity
--of latex math strings). I also disregard well-known latex macro markers, which do
--not produce any visible character in the rendred document.**
--In constrast to block tech strings, each inline tech string is considered a word in
--and by itself (regardless of how many characters it has; see the `wordlike` function).
--
-- **There are many latex commands, e.g. "\gamma", "\begin{matrix}", "\times" etc which
--have a couple of characters but are rendred to just one character in the final ouptput.
--On our counting method, these will be counted as one word each, although they are just
--one fifth of an average word' length. Dividing by 7 (instead of 5) corrects this word
--inflation due to the latex math string verbosity, but it also makes the number of words
--in code blocks drop. Perhaps it's not worth obsessing over this now.


countWordsInBlocks :: [Block] -> Int
countWordsInBlocks blcs = sum $ countWordsInBlock <$> blcs

countWordsInSection :: Section -> Int
countWordsInSection (Section _ _ (Title inls) bdy secs) =
  countWordsInInlines inls +
  countWordsInBlocks (secblcs . secbdy $ bdy) +
  countWordsInSections secs
countWordsInSection (SecBlocks blcs) = countWordsInBlocks blcs
countWordsInSection (Meta mp) =
  countWordsInAuthor mp + countWordsInTitle mp + countWordsInTags mp + countWordsInAbstract mp

--meta fields

countWordsInAuthor :: M.Map MetaKey MetaValue -> Int
countWordsInAuthor mp = procAuthor mp (countWordsInInlines . concat) 0

countWordsInTitle :: M.Map MetaKey MetaValue -> Int
countWordsInTitle mp = procTitle mp countWordsInInlines 0

countWordsInTags :: M.Map MetaKey MetaValue -> Int
countWordsInTags mp = procTags mp countWordsInInlines 0

countWordsInAbstract :: M.Map MetaKey MetaValue -> Int
countWordsInAbstract mp = procAbstract mp countWordsInBlocks 0

countWordsInSections :: [Section] -> Int
countWordsInSections secs = sum $ countWordsInSection <$> secs

countWordsInDocument :: Document -> Int
countWordsInDocument (Doc secs) = countWordsInSections secs

