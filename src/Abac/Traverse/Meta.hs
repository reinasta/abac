{-# LANGUAGE OverloadedStrings #-}
module Abac.Traverse.Meta where


import qualified Data.Text as T (intercalate,pack,concat,append)
import qualified Data.Map.Strict as M

import Abac.Types.ParserTypes
import Abac.Internal

import Abac.Traverse.Searchable

-- estimated reading time using an average of 200 words per minute
readingTime :: Searchable a => a -> Int
readingTime el = let wpm = 200 in countWordsIn el `div` wpm


-- to be used by the command-line tool to display the Meta section
metaToText :: Section -> [Text]
metaToText mt =
  let Expressions exprs = repackageMeta mt
      exprsToText [] = []
      exprsToText (e:es) =
        let Expression inls = e in inlinesToTxt inls : exprsToText es
  in  exprsToText exprs


repackageMeta :: Section -> ParaPart
repackageMeta (Meta mp) =
  let ttl = let MetaInlines inls = M.findWithDefault (MetaInlines []) TitleKey mp
            in  Expression $ filter wordlike inls
      ath = M.findWithDefault (MetaInlines []) AuthorKey mp
      abstr = let MetaBlocks blcs = M.findWithDefault (MetaBlocks []) AbstractKey mp
             in  [Expression $ concatMap (filter wordlike . gatherInlines) blcs]
      ttlAndAth =
        case ath of
          MetaInlines inls -> ttl : [Expression $ filter wordlike inls]
          MetaSeq inlss    -> ttl : fmap (Expression . filter wordlike) inlss
          _                -> []
  in  Expressions $ filter (/= Expression []) (ttlAndAth ++ abstr)
repackageMeta _ = Inlines []


-- toc

--toc :: Document -> Text
--toc doc = T.intercalate "\n" $ sectionEntry <$> getAllSections doc
--
--
--sectionEntry :: Section -> Text
--sectionEntry sct =
--  let no = (T.pack $ showSecNo $ secno sct) `T.append` " "
--      ttl = let Title inls = secttl sct in inlinesToTxt inls `T.append` " "
--      pos = let p = fst (getPosition sct) in "position " `T.append` posToTxt p
--      cnt = "words " `T.append` (T.pack $ show $ countWordsIn sct)
--  in  foldr1 T.append  [no, ttl, "(", pos, ", ", cnt, ")"]



-- textify

posToTxt :: Position -> Text
posToTxt (l,c) = T.pack (show l) `T.append` ":" `T.append` T.pack (show c)

inlinesToTxt :: [Inline] -> Text
inlinesToTxt inls = T.concat $ fmap inlineToTxt inls

inlineToTxt :: Inline -> Text
inlineToTxt (Word _ _ txt) = txt
inlineToTxt (Citation _ _ _ txt) = txt
inlineToTxt (InlineTech _ _ txt) = txt
inlineToTxt (InlineComment inls) = T.concat $ inlineToTxt <$> inls
inlineToTxt (Punct _ txt) = txt
inlineToTxt (Number _ _ n) = T.pack $ show n
inlineToTxt (Link _ urlid) = urlid
inlineToTxt (Image _ pthid) = pthid
inlineToTxt (Email _ _ nom dom) = nom `T.append` "@" `T.append` dom
inlineToTxt Space = " "
inlineToTxt Newline = "\n"
inlineToTxt ParEnd = "\n\n"
inlineToTxt _ = ""

-- get meta info

getAuthors :: forall a. Searchable a => a -> Either T.Text Expressions
getAuthors el =
  let expin m = let Meta emeta = m in mkexp <$> note "no authors found" (M.lookup AuthorKey emeta)
      mkexp (MetaSeq inlss) = Expressions $ fmap Expression inlss
      mkexp (MetaInlines inls) = Inlines $ filter wordlike inls
      mkexp _ = Inlines []
  in  expin =<< getMeta' el

getTitle :: forall a. Searchable a => a -> Either T.Text Inlines
getTitle el =
  let titleof m = let Meta emeta = m in mkttl <$> note "no title found" (M.lookup TitleKey emeta)
      mkttl (MetaInlines inls) = Inlines $ filter wordlike inls
      mkttl _ = Inlines []
  in  titleof =<< getMeta' el

getAbstract :: forall a. Searchable a => a -> Either T.Text Inlines
getAbstract el =
  let abstract m = let Meta emeta = m in mkabs <$> note "no abstract found" (M.lookup AbstractKey emeta)
      mkabs (MetaBlocks blcs) = Inlines $ getWordLikesFromDiv (SecBlocks blcs)
      mkabs _ = Inlines []
  in  abstract =<< getMeta' el


getMeta' :: forall a. Searchable a => a -> Either T.Text Meta
getMeta' el =
  let msg = T.pack $ "couldn't find any meta section "
  in  note msg $ case (filter isMeta $ gatherSections el) of
                   []     -> Nothing
                   metas  -> Just (head metas)

getMeta :: Document -> Either T.Text Meta
getMeta doc =
  let msg = T.pack $ "couldn't find any meta section "
  in  note msg $ case (filter isMeta $ getSections doc) of
                   []     -> Nothing
                   metas  -> Just (head metas)

getIntro :: Document -> Either T.Text Section
getIntro doc =
  let msg = T.pack $ "couldn't find any intro division before the beginning of sections "
  in  note msg $ case (filter isPureDiv $ getSections doc) of
                   []     -> Nothing
                   secs -> Just (head secs)


