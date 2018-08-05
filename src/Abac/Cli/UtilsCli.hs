{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Abac.Cli.UtilsCli where

import Text.Megaparsec hiding (count)
import Control.Applicative
import qualified Data.Text.IO as T (readFile,writeFile,putStrLn)
import qualified Data.Text as T
import Prelude hiding (Word)

import qualified Abac.Parser (doc)
import Abac.Parser.Internal (withoutAbbreviations)
import Abac.Traverse (toc,repackageMeta,Searchable,getMeta,hasAttrs)
import Abac.Types
import Abac.Cli.Command
import Abac.PartsOfSpeech
import Abac.PrettyResults (showRes,showPart)


-- Handling input and output
----------------------------


inputFile :: FilePath -> IO Text
inputFile f | f == defaultStr = getContents >>= return . withoutAbbreviations . T.pack
            | otherwise       = withoutAbbreviations <$> T.readFile f

outputIn :: (Show a) => String -> a -> IO ()
outputIn f | f == defaultStr = putStrLn . show
           | otherwise       = T.writeFile f . T.pack . show

-- process input and output files, or use stdin and stdout
procInput :: Input -> IO Text
procInput inpt = let FileInput fpath = inpt in inputFile fpath

procOutput :: (Show a) => Output -> a -> IO ()
procOutput outpt = let FileOutput str = outpt in outputIn str

procTextInput :: String -> IO (Either Err Document)
procTextInput txt = do
  edoc <- runParserT Abac.Parser.doc "" txt -- Abac.Parser.documentWithTags
  case edoc of
    Left err -> return $ Left $ Parsing $ T.pack $ show err
    Right doc -> return $ Right doc

-- display using the pretty or the native format
isPretty :: Command -> Bool
isPretty cmmd = if native cmmd == PrettyFlag then True else False

displayMode :: Command -> FinalResult -> IO ()
displayMode cmmd res | isPretty cmmd = putStrLn $ show $ showRes res
                     | otherwise     = putStrLn $ show res

isToc :: Command -> Bool
isToc cmmd = if Abac.Cli.Command.toc cmmd == TocFlag then True else False

hasMeta :: Command -> Bool
hasMeta cmmd = if meta cmmd == MetaFlag then True else False

displayMeta :: Document -> IO ()
displayMeta doc =
  case getMeta doc of
    Left txt -> putStrLn $ show $ Signal txt
    Right mt -> putStrLn $ show $ showPart $ repackageMeta mt


printCommand :: Command -> IO ()
printCommand cmmd = putStrLn $ show cmmd


cmmdToIO :: Command -> IO ()
cmmdToIO cmmd = do
  txt <- procInput $ inp cmmd
  edoc <- procTextInput $ T.unpack txt
  case edoc of
    Left err -> putStrLn $ show err
    Right doc -> sequenceIOActions cmmd doc

-- some exclusive & standalone flags (e.g. tocFlag) trigger IO actions before any other
-- flag or option
sequenceIOActions :: Command -> Document -> IO ()
sequenceIOActions cmmd doc | isToc cmmd   = T.putStrLn $ Abac.Traverse.toc doc
                           | hasMeta cmmd = displayMeta doc
                           | otherwise    = restToIO cmmd doc

-- the IO actions triggered by most of the flags/options (to be processed
-- only if some exclusive flags (see sequenceIOActions) are off
restToIO :: Command -> Document -> IO ()
restToIO cmmd doc = do
  eres <- runAbc $ cmmdToAbac cmmd doc -- :: IO FinalResult
  case eres of
    Left err -> putStrLn $ show err
    Right res -> displayMode cmmd res


cmmdToAbac :: Command -> Document -> Abac FinalResult
cmmdToAbac cmmd doc =
  let flags = activeFlags $ gatherFlags cmmd
      opts = activeOpts $ gatherExpOpts cmmd
      repr = rep cmmd
      --finp = inp cmmd
      outrep = fromAccFlagToArg repr
      f = fromFlagToArg $ head flags :: Searchable a => a -> Abac Res
      flgRes = interimToRes f f f f  :: Abac InterimTree -> Abac Res
      tree = sequencePosArgs cmmd (return doc) :: Abac InterimTree
  in  if null opts
         then outrep =<< flgRes tree
         else let g = fromExpOptToArg $ head opts :: Searchable a => a -> Abac Res
                  optRes = interimToRes g g g g :: Abac InterimTree -> Abac Res
              in  outrep =<< optRes tree

interimToRes ::
  (Inlines -> Abac Res) ->
  (Paragraph -> Abac Res) ->
  (Section -> Abac Res) ->
  (Document -> Abac Res) ->
  Abac InterimTree -> Abac Res
interimToRes f1 f2 f3 f4 abc = do
  tr <- abc
  case () of
    () | isInterimInlines tr   -> f1 =<< unwrapInlinesArg abc
       | isInterimParagraph tr -> f2 =<< unwrapParaArg abc
       | isInterimSection tr   -> f3 =<< unwrapSectionArg abc
       | otherwise             -> f4 =<< unwrapDocumentArg abc


sequencePosArgs :: Command -> Abac Document -> Abac InterimTree
sequencePosArgs cmmd abc =
  let js = not . isDefaultOpt $ sno cmmd
      jp = not . isDefaultOpt $ ind cmmd
      jl = not . isDefaultOpt $ lno cmmd

      sval = let SectNumber str = sno cmmd in T.pack str
      pval = let ParaIndex i = ind cmmd in i
      lval = let LineNumber n = lno cmmd in n

      sec1 = section sval   :: Document  -> Abac Section
      par1 = paragraph pval :: Section   -> Abac Paragraph
      par2 = paragraph pval :: Document  -> Abac Paragraph
      lne1 = line lval      :: Paragraph -> Abac Inlines
      lne2 = line lval      :: Section   -> Abac Inlines
      lne3 = line lval      :: Document  -> Abac Inlines

  in  case (js, jp, jl) of
        (True, True, True)    -> wrapInlinesArg $ lne1 =<< par1 =<< sec1 =<< abc
        (False, True, True)   -> wrapInlinesArg $ lne1 =<< par2 =<< abc
        (True, False, True)   -> wrapInlinesArg $ lne2 =<< sec1 =<< abc
        (False, False, True)  -> wrapInlinesArg $ lne3 =<< abc
        (True, True, False)   -> wrapParaArg    $ par1 =<< sec1 =<< abc
        (False, True, False)  -> wrapParaArg    $ par2 =<< abc
        (True, False, False)  -> wrapSectionArg $ sec1 =<< abc
        (False, False, False) -> wrapDocumentArg  abc


fromFlagToArg :: forall a. Searchable a => Flag -> a -> Abac Res
fromFlagToArg AuthorFlag      = author
fromFlagToArg TitleFlag       = title
fromFlagToArg AbstractFlag    = abstract
fromFlagToArg EmailFlag       = email
fromFlagToArg CitationFlag    = citation
fromFlagToArg NumberFlag      = number'
fromFlagToArg MathFlag        = Abac.PartsOfSpeech.math
fromFlagToArg CodeFlag        = Abac.PartsOfSpeech.code
fromFlagToArg EmphFlag        = emph'
fromFlagToArg BoldFlag        = bold'
fromFlagToArg ParenFlag       = parened
fromFlagToArg BracketFlag     = bracketed
fromFlagToArg SngQuoteFlag    = sngquoted
fromFlagToArg DblQuoteFlag    = dblquoted
fromFlagToArg AdwordFlag      = Abac.PartsOfSpeech.adword
fromFlagToArg ConnectiveFlag  = connective
fromFlagToArg IndexicalFlag   = Abac.PartsOfSpeech.indexical
fromFlagToArg NominalFlag     = nominalisation
fromFlagToArg PrepositionFlag = preposition
fromFlagToArg WeakVerbFlag    = weakverb
fromFlagToArg PassiveFlag     = passives
fromFlagToArg AnyWordFlag     = Abac.PartsOfSpeech.anyword
fromFlagToArg _               = Abac.PartsOfSpeech.anyword

-- | an alternative to fromFlagToArg
------------------------------------


argsToRes :: forall a. Searchable a => [CmmdArg] -> a -> Res
argsToRes args el = undefined

fromAttrFlagsToAttrs :: [AttrFlag] -> [Attr]
fromAttrFlagsToAttrs aflgs = undefined

filterByAttrs :: forall a. Searchable a => [AttrFlag] -> a -> Res
filterByAttrs aflgs =
  let attrs = fromAttrFlagsToAttrs aflgs
  in  snd . fromFilter' (`hasAttrs` attrs)




cmmdToAbac' :: Command -> Document -> Abac FinalResult
cmmdToAbac' cmmd doc =
  let flags = activeFlags $ gatherFlags cmmd
      opts = activeOpts $ gatherExpOpts cmmd
      repr = rep cmmd
      --finp = inp cmmd
      outrep = fromAccFlagToArg repr
      f = fromFlagToArg $ head flags :: Searchable a => a -> Abac Res
      flgRes = interimToRes f f f f  :: Abac InterimTree -> Abac Res
      tree = sequencePosArgs cmmd (return doc) :: Abac InterimTree
  in  if null opts
         then outrep =<< flgRes tree
         else let g = fromExpOptToArg $ head opts :: Searchable a => a -> Abac Res
                  optRes = interimToRes g g g g :: Abac InterimTree -> Abac Res
              in  outrep =<< optRes tree

interimToRes' ::
  (Inlines -> Res) ->
  (Paragraph -> Res) ->
  (Section -> Res) ->
  (Document -> Res) ->
  InterimTree -> Res
interimToRes' f1 f2 f3 f4 tr
  | isInterimInlines tr   = f1 $ unwrapInlinesArg' tr
  | isInterimParagraph tr = f2 $ unwrapParaArg' tr
  | isInterimSection tr   = f3 $ unwrapSectionArg' tr
  | otherwise             = f4 $ unwrapDocumentArg' tr


sequencePosArgs' :: Command -> Document -> InterimTree
sequencePosArgs' cmmd doc =
  let js = not . isDefaultOpt $ sno cmmd
      jp = not . isDefaultOpt $ ind cmmd
      jl = not . isDefaultOpt $ lno cmmd

      sval = let SectNumber str = sno cmmd in T.pack str
      pval = let ParaIndex i = ind cmmd in i
      lval = let LineNumber n = lno cmmd in n

      sec1 = section' sval   :: Document  -> Section
      par1 = paragraph' pval :: Section   -> Paragraph
      par2 = paragraph' pval :: Document  -> Paragraph
      lne1 = line' lval      :: Paragraph -> Inlines
      lne2 = line' lval      :: Section   -> Inlines
      lne3 = line' lval      :: Document  -> Inlines

  in  case (js, jp, jl) of
        (True, True, True)    -> InterimInlines   $ lne1 $ par1 $ sec1 doc
        (False, True, True)   -> InterimInlines   $ lne1 $ par2 doc
        (True, False, True)   -> InterimInlines   $ lne2 $ sec1 $ doc
        (False, False, True)  -> InterimInlines   $ lne3 $ doc
        (True, True, False)   -> InterimParagraph $ par1 $ sec1 doc
        (False, True, False)  -> InterimParagraph $ par2 $ doc
        (True, False, False)  -> InterimSection   $ sec1 $ doc
        (False, False, False) -> InterimDocument  doc



unwrapInlinesArg' :: InterimTree -> Inlines
unwrapInlinesArg' (InterimInlines inls) = inls
unwrapInlinesArg' _ = error "unwrapInlinesArg': expecting an InterimInlines value"

unwrapParaArg' :: InterimTree -> Paragraph
unwrapParaArg' (InterimParagraph para) = para
unwrapParaArg' _ = error "unwrapParaArg': expecting an InterimParagraph value"

unwrapSectionArg' :: InterimTree -> Section
unwrapSectionArg' (InterimSection sect) = sect
unwrapSectionArg' _ = error "unwrapSectionArg': expecting an InterimSection value"

unwrapDocumentArg' :: InterimTree -> Document
unwrapDocumentArg' (InterimDocument doc) = doc
unwrapDocumentArg' _ = error "unwrapDocumentArg': expecting an InterimDocument value"


activeFlags' :: [Flag] -> [Flag]
activeFlags' flgs = filter (/= AnyWordFlag) flgs ++ [AnyWordFlag]


-- end of alternative
---------------------

fromAccFlagToArg :: Flag -> Res -> Abac FinalResult
fromAccFlagToArg ListFlag        = list
fromAccFlagToArg WholeFlag       = count
fromAccFlagToArg PercentFlag     = percent
fromAccFlagToArg TimeFlag        = time
fromAccFlagToArg _               = list

fromExpOptToArg :: forall a. (Searchable a) => CommandOpt -> a -> Abac Res
fromExpOptToArg (Tagged str)     = tagged $ Tag $ T.pack str
fromExpOptToArg (MultiPrep n)    = prepexpr n
fromExpOptToArg (Ngram n)        = ngram n
fromExpOptToArg _                = \_ -> putError $ MiscErr msg
  where msg = "fromExpOptToArg: unexpected command option; expecting Tagged, MutiPrep or Ngram"

isMetaFlag :: Flag -> Bool
isMetaFlag ListFlag    = True
isMetaFlag WholeFlag   = True
isMetaFlag PercentFlag = True
isMetaFlag TimeFlag    = True
isMetaFlag _           = False

isAttrFlag :: Flag -> Bool
isAttrFlag EmphFlag     = True
isAttrFlag BoldFlag     = True
isAttrFlag ParenFlag    = True
isAttrFlag BracketFlag  = True
isAttrFlag SngQuoteFlag = True
isAttrFlag DblQuoteFlag = True
isAttrFlag _            = False

unwrapInlinesArg :: Abac InterimTree -> Abac Inlines
unwrapInlinesArg abc =
  let unwrap (InterimInlines inls) = inls
      unwrap _ = error "unwrapInlinesArg: expecting an InterimInlines value"
  in unwrap <$> abc

unwrapParaArg :: Abac InterimTree -> Abac Paragraph
unwrapParaArg abc =
  let unwrap (InterimParagraph para) = para
      unwrap _ = error "unwrapParaArg: expecting an InterimParagraph value"
  in  unwrap <$> abc

unwrapSectionArg :: Abac InterimTree -> Abac Section
unwrapSectionArg abc =
  let unwrap (InterimSection sect) = sect
      unwrap _ = error "unwrapSectionArg: expecting an InterimSection value"
  in  unwrap <$> abc

unwrapDocumentArg :: Abac InterimTree -> Abac Document
unwrapDocumentArg abc =
  let unwrap (InterimDocument doc) = doc
      unwrap _ = error "unwrapDocumentArg: expecting an InterimDocument value"
  in  unwrap <$> abc

wrapInlinesArg :: Abac Inlines -> Abac InterimTree
wrapInlinesArg abc = InterimInlines <$> abc

wrapParaArg :: Abac Paragraph -> Abac InterimTree
wrapParaArg abc = InterimParagraph <$> abc

wrapSectionArg :: Abac Section -> Abac InterimTree
wrapSectionArg abc = InterimSection <$> abc

wrapDocumentArg :: Abac Document -> Abac InterimTree
wrapDocumentArg abc = InterimDocument <$> abc


isInterimInlines :: InterimTree -> Bool
isInterimInlines (InterimInlines _) = True
isInterimInlines _                  = False

isInterimParagraph :: InterimTree -> Bool
isInterimParagraph (InterimParagraph _) = True
isInterimParagraph _                    = False

isInterimSection :: InterimTree -> Bool
isInterimSection (InterimSection _) = True
isInterimSection _                   = False

isInterimDocument :: InterimTree -> Bool
isInterimDocument (InterimDocument _)  = True
isInterimDocument _                    = False


-- collect all non-default opts
activeOpts :: [CommandOpt] -> [CommandOpt]
activeOpts [] = []
activeOpts (opt:opts) =
  case opt of
    FileInput _  -> activeOpts opts
    FileOutput _ -> activeOpts opts
    NullOpt      -> activeOpts opts
    otheropt     -> if isDefaultOpt otheropt
                       then activeOpts opts
                       else otheropt : activeOpts opts

-- collect all non-default flags plus one default flag (AnyWordFlag)
activeFlags :: [Flag] -> [Flag]
activeFlags [] = [AnyWordFlag]
activeFlags (flg:flgs) | flg == AnyWordFlag = activeFlags flgs
                       | otherwise          = flg : activeFlags flgs

