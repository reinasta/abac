{-# LANGUAGE OverloadedStrings #-}
module Abac.Traverse.Textify where


import qualified Data.Map.Strict as M
import qualified Data.Text as T (Text, splitOn, length, isSuffixOf,
                                 empty, append, intercalate,
                                 replicate, unwords, pack, unpack,
                                 count, concat, cons, snoc,)

import Abac.Types.ParserTypes
import Abac.Traverse.Examples
import Abac.Traverse.Predicates


--decode document into text

lineend, parend, spc :: Text
lineend = T.pack "\n"
parend = T.pack "\n\n"
spc = T.pack " "

unwrapInline :: Inline -> Text
unwrapInline (Word _ _ txt) = txt
unwrapInline (Citation _ _ _ txt) = txt
unwrapInline (Email _ _ nom dom) = T.concat [nom, "@", dom]
unwrapInline (InlineTech _ _ txt) = txt
unwrapInline (Number _ _  dbl) = T.pack $ show dbl
unwrapInline (Punct _ txt) = txt
unwrapInline (Link inls _) = T.unwords $ fmap unwrapInline inls
unwrapInline (Image inls _) = T.unwords $ fmap unwrapInline inls
unwrapInline Space = T.pack " "
unwrapInline Newline = lineend
unwrapInline ParEnd = parend
unwrapInline _ = T.empty

unwrapParaPart :: ParaPart -> Text
unwrapParaPart (Sentence inls) = T.unwords $ fmap unwrapInline inls
unwrapParaPart (ParaFtn _ snts) =
  let footnote = T.intercalate spc $ fmap unwrapParaPart snts
      leftMarker = "^[" :: Text
      rightMarker = "]" :: Text
  in  leftMarker `T.append` footnote `T.append` rightMarker
unwrapParaPart (Inlines inls) = T.unwords $ fmap unwrapInline inls
unwrapParaPart (Caption prts) = T.intercalate spc $ fmap unwrapParaPart prts
unwrapParaPart _ = T.empty


-- unwrapExample :: Example -> Text
-- unwrapExample (MainEx _ _ _ prts) = T.concat (fmap unwrapParaPart prts) `T.append` lineend
-- unwrapExample (SubEx _ _ _ prts) = T.concat (fmap unwrapParaPart prts) `T.append` lineend


unwrapBlock :: Block -> Text
unwrapBlock (Para prts) = T.concat $ fmap unwrapParaPart prts
unwrapBlock (Footnote _ blcs) = T.intercalate parend $ fmap unwrapBlock blcs
unwrapBlock (BlockEx exs) = T.concat $ unwrapParaPart <$> unwrapExamples exs
unwrapBlock (BlockQuotes blcs) = T.intercalate parend $ fmap unwrapBlock blcs
unwrapBlock (BlockTech Math _ txt) = '$' `T.cons` txt `T.snoc` '$'
unwrapBlock (BlockTech Code _ txt) = '`' `T.cons` txt `T.snoc` '`'
unwrapBlock (LinkRef _ _ url) = url
unwrapBlock (ImageRef _ _ path) = path
unwrapBlock (Table prts) = T.intercalate spc $ fmap unwrapParaPart prts
unwrapBlock _ = T.empty

-- assignExMarkers :: [[Example]] -> Text
-- assignExMarkers = assignMarkers "(@)" ['a'..'z']
--
-- assignBulletMarkers :: [[Example]] -> Text
-- assignBulletMarkers = assignMarkers "+" (repeat '-')
--
-- assignMarkers :: String -> String -> [[Example]] -> Text
-- assignMarkers mrk submrks [] = T.empty
-- assignMarkers mrk submrks (exs:exss) =
--   let exmarker = T.pack mrk
--       dotspace = ". " :: Text
--       prefixSubmarker :: Char -> Text -> Text
--       prefixSubmarker s ts = s `T.cons` (dotspace `T.append` ts)
--       main = exmarker `T.append` unwrapExample (head exs) `T.append` lineend
--       subexs = zipWith prefixSubmarker submrks (unwrapExample <$> tail exs)
--   in  T.concat (main : subexs) `T.append` assignMarkers mrk (tail submrks) exss

unwrapSection :: Section -> Text
unwrapSection (SecBlocks blcs) = T.intercalate parend $ fmap unwrapBlock blcs
unwrapSection (Section _ lvl ttl bdy subs) =
  let hash = "#" :: Text
      unwrapTitle (Title inls) = T.unwords $ fmap unwrapInline inls
      title = T.replicate lvl hash `T.append` spc `T.append` unwrapTitle ttl `T.append` parend
      body = unwrapSection bdy
      blocks = T.intercalate parend $ fmap unwrapSection subs
  in  title `T.append` body `T.append` blocks
unwrapSection (Meta mp) = T.concat $ addYamlDelimiters $ yamlEntries mp
  where
    addYamlDelimiters :: [Text] -> [Text]
    addYamlDelimiters txts = [delim] ++ txts ++ [delim] where delim = ("---\n" :: Text)

    yamlEntries :: M.Map MetaKey MetaValue -> [Text]
    yamlEntries m = M.elems $ M.mapWithKey makeYamlEntry m
    makeYamlEntry var val = varToString var `T.append` valToString val `T.append` lineend

    varToString :: MetaKey -> Text
    varToString v =
      case v of
        AuthorKey    -> "author: " :: Text
        TitleKey     -> "title: " :: Text
        DateKey      -> "date: " :: Text
        AbstractKey  -> "abstract: " :: Text
        TagKey       -> "tags: " :: Text
        OtherKey txt -> txt :: Text

    valToString :: MetaValue -> Text
    valToString v =
      case v of
        MetaSeq inlss    -> (T.unwords . fmap T.unwords) $ (fmap . fmap) unwrapInline inlss
        MetaNum ints     -> T.concat $ fmap (T.pack . show) ints
        MetaInlines inls -> T.unwords $ fmap unwrapInline inls
        MetaBool bool    -> T.pack $ show bool
        MetaBlocks blcs  -> T.intercalate parend $ fmap unwrapBlock blcs

unwrapDocument :: Document -> Text
unwrapDocument (Doc secs) = T.intercalate spc $ fmap unwrapSection secs

--NB: by unwraping the inlines and then wraping them in Word, we lose
--the distinction between different types of inlines (words, citations
--numbers etc), but for our use cases (counting intersections or string
--rendering) that's ok.

unwrapInlines :: [Inline] -> [Text] -- used for displaying words rather than all inlines
unwrapInlines [] = []
unwrapInlines inls = fmap unwrapInline $ filter wordlike inls



--  case inl of
--   (Word _ _ txt)       -> txt : unwrapInlines inls
--   (Citation _ _ txt)   -> txt : unwrapInlines inls
--   (InlineTech _ _ txt) -> txt : unwrapInlines inls
--   (Number _  dbl)      -> (T.pack $ show dbl) : unwrapInlines inls
--   _                    -> unwrapInlines inls



unwrapExpr :: Expression -> [Inline]
unwrapExpr (Expression inls) = inls

wrapInExpr :: [Inline] -> Expression
wrapInExpr inls = Expression inls

textifyInlines :: [Inline] -> Text
textifyInlines [] = T.pack "nothing found"
textifyInlines inls = T.unwords $ unwrapInlines inls

textifyExpr :: Expression -> Text
textifyExpr exps = T.unwords $ unwrapInlines $ unwrapExpr exps

textifySent :: Sentence -> Text
textifySent (Sentence inls) = textifyInlines inls
textifySent _ = T.empty


