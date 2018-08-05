{-# LANGUAGE OverloadedStrings #-}
module Abac.Cli.Command where

import Data.List (isSuffixOf)
import Options.Applicative hiding (command)
import Data.Semigroup ((<>))
import Data.Default (Default (..))
import Prelude hiding (abs)

-- | Type synonyms for commandline options
type Input = CommandOpt -- FileInput FilePath deriving (Show,Eq)
type PIndex = CommandOpt -- Par Int deriving (Show,Eq)
type LNo = CommandOpt -- LNo Int deriving (Show,Eq)
type SecNo = CommandOpt -- SecNo String deriving (Show,Eq)
type Representation = CommandOpt -- Percent | Whole | Words deriving (Show,Eq)
type Output = CommandOpt -- FileOutput FilePath deriving (Show,Eq)
type Tagged = CommandOpt -- Tagged String deriving (Show,Eq)
type Ngram = CommandOpt -- Ngram Int deriving (Show,Eq)
type MultiPrep = CommandOpt -- MultiPrep Int deriving (Show,Eq)


data Formats = Percent | Whole | Words deriving (Show,Eq)

data Flag = AnyTreeFlag | AuthorFlag | TitleFlag | AbstractFlag | TocFlag |
  EmailFlag | CitationFlag | NumberFlag | PrettyFlag | NativeFlag | MetaFlag |
  MathFlag | CodeFlag | EmphFlag | BoldFlag | ParenFlag | BracketFlag |
  SngQuoteFlag | DblQuoteFlag | AdwordFlag | ConnectiveFlag |
  IndexicalFlag | NominalFlag | PrepositionFlag | WeakVerbFlag | PassiveFlag |
  AnyWordFlag | ListFlag | WholeFlag | PercentFlag | TimeFlag | NullFlag deriving (Show,Eq)

-- | Commandline options
data CommandOpt =
  FileInput FilePath
  | ParaIndex Int
  | LineNumber Int
  | SectNumber String
  | FileOutput FilePath
  | Ngram Int
  | MultiPrep Int
  | Tagged String
  | NullOpt
  deriving (Show, Eq)

-- default options (discarded in the final IO output, if non-default options present)
isDefaultOpt :: CommandOpt -> Bool
isDefaultOpt (FileInput "") = True
isDefaultOpt (ParaIndex (-1)) = True
isDefaultOpt (LineNumber (-1)) = True
isDefaultOpt (SectNumber "") = True
isDefaultOpt (FileOutput "") = True
isDefaultOpt (Ngram (-1)) = True
isDefaultOpt (MultiPrep (-1)) = True
isDefaultOpt (Tagged "") = True
isDefaultOpt NullOpt = True
isDefaultOpt _ = False

-- default flags
isDefaultFlag :: Flag -> Bool
isDefaultFlag AnyWordFlag = True
isDefaultFlag ListFlag = True
isDefaultFlag PrettyFlag = True
isDefaultFlag _ = False


-- collect record fields in separate lists of CommandOpts and Flags
gatherOpts :: Command -> [CommandOpt]
gatherOpts cmmd = inp cmmd : sno cmmd : ind cmmd : lno cmmd : other cmmd :
  out cmmd : []

gatherExpOpts :: Command -> [CommandOpt]
gatherExpOpts cmmd = other cmmd : []


gatherFlags :: Command -> [Flag]
gatherFlags cmmd = aut cmmd : ttl cmmd : abs cmmd :
  eml cmmd : cit cmmd : num cmmd : math cmmd : code cmmd :
  emph cmmd : bold cmmd : paren cmmd : bracket cmmd : squote cmmd :
  dquote cmmd : adword cmmd : connect cmmd : indexical cmmd : nominal cmmd :
  prep cmmd : weakvb cmmd : passive cmmd : anyword cmmd : native cmmd :
  toc cmmd : rep cmmd : []

-- | alternative type for all command line arguments
----------------------------------------------------

data CmmdArg =
  WithOpt CommandOpt
  | Markdown MdFlag
  | MetaInfo MetaFlag
  | Attrib AttrFlag
  | Gram GramFlag
  | ResOut ResFlag
  | Display PresFlag
  deriving (Show,Eq)

-- in addition to the types below, see also CommandOpt above;
-- MetaFlag was replaced by AllMetaFlg

data MdFlag = EmailFlg | CitationFlg | NumberFlg | MathFlg | CodeFlg deriving (Show,Eq)

data MetaFlag = AuthorFlg | TitleFlg | AbstractFlg | AllMetaFlg | TocFlg deriving (Show,Eq)

data AttrFlag = EmphFlg | BoldFlg | ParenFlg | BracketFlg |
  SngQuoteFlg | DblQuoteFlg deriving (Show,Eq)

data GramFlag = AdwordFlg | ConnectiveFlg | IndexicalFlg | NominalFlg |
  PrepositionFlg | WeakVerbFlg | PassiveFlg deriving (Show,Eq)

data ResFlag = ListFlg | WholeFlg | PercentFlg | TimeFlg deriving (Show,Eq)

data PresFlag = PrettyFlg | NativeFlg deriving (Show,Eq)


getCommandOpts :: Command -> [CommandOpt]
getCommandOpts cmmd = undefined

getMarkdownFlags :: Command -> [MdFlag]
getMarkdownFlags cmmd = undefined

getMetaFlags :: Command -> [MetaFlag]
getMetaFlags cmmd = undefined

getAttrFlags :: Command -> [AttrFlag]
getAttrFlags cmmd = undefined

getGramFlags :: Command -> [GramFlag]
getGramFlags cmmd = undefined

getResFlags :: Command -> [ResFlag]
getResFlags cmmd = undefined

getPresFlags :: Command -> [PresFlag]
getPresFlags cmmd = undefined


-- end of alternative type
--------------------------



-- option parsers

input :: Parser Input
input = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> value "DEFAULT-STDIN"
  <> help "File to be analyzed"
  )

parindex :: Parser PIndex
parindex = ParaIndex <$> option auto
  (  long "parindex"
  <> short 'p'
  <> metavar "INDEX"
  <> value (-1)
  <> help "Search at a given paragraph (INDEX)"
  )

lineindex :: Parser LNo
lineindex = LineNumber <$> option auto
  (  long "lineno"
  <> short 'l'
  <> metavar "NUM"
  <> value (-1)
  <> help "Search at a given line (NUM)"
  )

sectindex :: Parser SecNo
sectindex = SectNumber <$> strOption'
  (  long "secno"
  <> short 's'
  <> metavar "NUM"
  <> value ""
  <> help "Search at a given section (NUM)"
  ) where
      strOption' = (fmap . fmap) dropDotZero strOption
      dropDotZero :: String -> String
      dropDotZero strg | ".0" `isSuffixOf` strg = init . init $ strg
                       | "." `isSuffixOf` strg  = init strg
                       | otherwise              = strg

repres :: Parser Flag
repres = represWhole <|> represPercent <|> represTime

represTime :: Parser Flag
represTime = flag ListFlag TimeFlag
  (  long "time"
  <> short '\''
  <> help "Estimated reading time"
  )

represPercent :: Parser Flag
represPercent = flag ListFlag PercentFlag
  (  long "percent"
  <> short '%'
  <> help "Expression proportion relative to total"
  )

represWhole :: Parser Flag
represWhole = flag ListFlag WholeFlag
  (  long "whole"
  <> short '#'
  <> help "The absolute count of a given expression"
  )


output :: Parser Output
output = FileOutput <$> strOption
  (  long "output"
  <> short 'o'
  <> metavar "FILE"
  <> value "DEFAULT-STDOUT"
  <> help "Where to output the diagnostic (default: stdout)"
  )

-- flag parsers

authorFlag :: Parser Flag
authorFlag = flag AnyWordFlag AuthorFlag
  (  long "author"
  <> short 'a'
  <> help "Search for authors"
  )

titleFlag :: Parser Flag
titleFlag = flag AnyWordFlag TitleFlag
  (  long "title"
  <> short 't'
  <> help "Search for document title"
  )

abstractFlag :: Parser Flag
abstractFlag = flag AnyWordFlag AbstractFlag
  (  long "abstract"
  <> help "Search for the document abstract"
  )


emailFlag :: Parser Flag
emailFlag = flag AnyWordFlag EmailFlag
  (  long "email"
  <> short 'e'
  <> help "Search for emails"
  )

citationFlag :: Parser Flag
citationFlag = flag AnyWordFlag CitationFlag
  (  long "citation"
  <> short 'r'
  <> help "Search for citations (references) "
  )

numberFlag :: Parser Flag
numberFlag = flag AnyWordFlag NumberFlag
  (  long "number"
  <> short 'n'
  <> help "Search for numbers"
  )

mathFlag :: Parser Flag
mathFlag = flag AnyWordFlag MathFlag
  (  long "math"
  <> short 'm'
  <> help "Search for math strings"
  )

codeFlag :: Parser Flag
codeFlag = flag AnyWordFlag CodeFlag
  (  long "code"
  <> short 'c'
  <> help "Search for code strings"
  )

emphFlag :: Parser Flag
emphFlag = flag AnyWordFlag EmphFlag
  (  long "emph"
  <> long "italic"
  <> short 'i'
  <> help "Search for words in italic"
  )

boldFlag :: Parser Flag
boldFlag = flag AnyWordFlag BoldFlag
  (  long "bold"
  <> short 'b'
  <> help "Search for words in bold"
  )

parenFlag :: Parser Flag
parenFlag = flag AnyWordFlag ParenFlag
  (  long "paren"
  <> short '('
  <> help "Search for parenthesized text"
  )

bracketFlag :: Parser Flag
bracketFlag = flag AnyWordFlag BracketFlag
  (  long "bracket"
  <> short '['
  <> help "Search for words in brackets"
  )

sngQuoteFlag :: Parser Flag
sngQuoteFlag = flag AnyWordFlag SngQuoteFlag
  (  long "squote"
  <> short 'q'
  <> help "Search for single-quoted words"
  )

dblQuoteFlag :: Parser Flag
dblQuoteFlag = flag AnyWordFlag DblQuoteFlag
  (  long "dquote"
  <> short 'd'
  <> help "Search for double-quoted words"
  )

adwordFlag :: Parser Flag
adwordFlag = flag AnyWordFlag AdwordFlag
  (  long "adword"
  <> help "Search for abstract adverbs and adjectives"
  )

connectiveFlag :: Parser Flag
connectiveFlag = flag AnyWordFlag ConnectiveFlag
  (  long "connective"
  <> help "Search for connectives"
  )

indexicalFlag :: Parser Flag
indexicalFlag = flag AnyWordFlag IndexicalFlag
  (  long "indexical"
  <> help "Search for indexicals"
  )

nominalFlag :: Parser Flag
nominalFlag = flag AnyWordFlag NominalFlag
  (  long "nominal"
  <> help "Search for nominalisation"
  )

prepositionFlag :: Parser Flag
prepositionFlag = flag AnyWordFlag PrepositionFlag
  (  long "preposition"
  <> help "Search for prepositions"
  )

weakverbFlag :: Parser Flag
weakverbFlag = flag AnyWordFlag WeakVerbFlag
  (  long "weakverb"
  <> short 'w'
  <> help "Search for weak verbs"
  )

passiveFlag :: Parser Flag
passiveFlag = flag AnyWordFlag PassiveFlag
  (  long "passive"
  <> short 'v'
  <> help "Search for passives"
  )

anywordFlag :: Parser Flag
anywordFlag = flag AnyWordFlag AnyWordFlag
  (  long "anyword"
  <> help "Search for any words (default)"
  )


nativeFlag :: Parser Flag
nativeFlag = flag PrettyFlag NativeFlag
  ( long "native"
  <> help "Display results in the native format (default: pretty)"
  )

tocFlag :: Parser Flag
tocFlag = flag NullFlag TocFlag
  ( long "toc"
  <> help "Display table of contents and section word-counts"
  )

metaFlag :: Parser Flag
metaFlag = flag NullFlag MetaFlag
  ( long "meta"
  <> help "Display meta (author, title, and abstract) "
  )



-- other options

otherOpts :: Parser CommandOpt
otherOpts = taggedOpt <|> ngramOpt <|> multiPrepOpt <|> pure NullOpt

taggedOpt :: Parser Tagged
taggedOpt = Tagged <$> strOption
  (  long "tagged"
  <> metavar "TAG"
  <> value ""
  <> help "Search for words with a given tag"
  )

ngramOpt :: Parser Ngram
ngramOpt = Ngram <$> option auto
  (  long "ngram"
  <> short 'g'
    <> metavar "NUM"
  <> value defaultInt
  <> help "Generate n-grams using the given NUM argument"
  )

multiPrepOpt :: Parser MultiPrep
multiPrepOpt = MultiPrep <$> option auto
  (  long "multiprep"
  <> metavar "NUM"
  <> value defaultInt
  <> help "Search for expressions with NUM prepositions"
  )



-- | entry point for the parser of command line arguments
withInfo :: ParserInfo Command
withInfo = info (command <**> helper)
  ( fullDesc
  <> progDesc "Look TARGET (e.g. prepositions) up in FILE at (e.g. section) NUMBER"
  <> header "Abac is a text querying library for markdown authors" )



-- | general command parser
command :: Parser Command
command = Command <$>
  input
  <*> parindex
  <*> lineindex
  <*> sectindex
  <*> repres
  <*> output
  <*> otherOpts
  <*> authorFlag
  <*> titleFlag
  <*> abstractFlag
  <*> emailFlag
  <*> citationFlag
  <*> numberFlag
  <*> mathFlag
  <*> codeFlag
  <*> emphFlag
  <*> boldFlag
  <*> parenFlag
  <*> bracketFlag
  <*> sngQuoteFlag
  <*> dblQuoteFlag
  <*> adwordFlag
  <*> connectiveFlag
  <*> indexicalFlag
  <*> nominalFlag
  <*> prepositionFlag
  <*> weakverbFlag
  <*> passiveFlag
  <*> anywordFlag
  <*> nativeFlag
  <*> tocFlag
  <*> metaFlag

-- | general command type
data Command = Command
  { inp       :: Input      -- ^ input file
  , ind       :: PIndex     -- ^ paragraph index
  , lno       :: LNo        -- ^ line number
  , sno       :: SecNo      -- ^ section number
  , rep       :: Flag       -- ^ representation (list, integer, floating point)
  , out       :: Output     -- ^ output file
  , other     :: CommandOpt -- ^ tag, ngram, multiprep options
  , aut       :: Flag       -- ^ author flag
  , ttl       :: Flag       -- ^ title flag
  , abs       :: Flag       -- ^ abstract flag
  , eml       :: Flag       -- ^ email flag
  , cit       :: Flag       -- ^ citation flag
  , num       :: Flag       -- ^ number flag
  , math      :: Flag       -- ^ math flag
  , code      :: Flag       -- ^ code flag
  , emph      :: Flag       -- ^ emph flag
  , bold      :: Flag       -- ^ bold flag
  , paren     :: Flag       -- ^ parens flag
  , bracket   :: Flag       -- ^ brackets flag
  , squote    :: Flag       -- ^ single quote flag
  , dquote    :: Flag       -- ^ double quote flag
  , adword    :: Flag       -- ^ adverb or adjective flag
  , connect   :: Flag       -- ^ connective flag
  , indexical :: Flag       -- ^ indexical flag
  , nominal   :: Flag       -- ^ nominal flag
  , prep      :: Flag       -- ^ preposition flag
  , weakvb    :: Flag       -- ^ weak verb flag
  , passive   :: Flag       -- ^ passive verb flag
  , anyword   :: Flag       -- ^ any word flag
  , native    :: Flag       -- ^ native flag
  , toc       :: Flag       -- ^ table of contents flag
  , meta      :: Flag       -- ^ meta flag
  } deriving (Show,Eq)

instance Default Command where
  def = defaultCommand

defaultInt :: Int
defaultInt = (-1) :: Int

defaultStr :: String
defaultStr = "" :: String

defaultCommand :: Command
defaultCommand = Command
  { inp       = FileInput defaultStr
  , ind       = ParaIndex defaultInt
  , lno       = LineNumber defaultInt
  , sno       = SectNumber ""
  , rep       = ListFlag
  , out       = FileOutput defaultStr
  , other     = NullOpt
  , aut       = AnyWordFlag
  , ttl       = AnyWordFlag
  , abs       = AnyWordFlag
  , eml       = AnyWordFlag
  , cit       = AnyWordFlag
  , num       = AnyWordFlag
  , math      = AnyWordFlag
  , code      = AnyWordFlag
  , emph      = AnyWordFlag
  , bold      = AnyWordFlag
  , paren     = AnyWordFlag
  , bracket   = AnyWordFlag
  , squote    = AnyWordFlag
  , dquote    = AnyWordFlag
  , adword    = AnyWordFlag
  , connect   = AnyWordFlag
  , indexical = AnyWordFlag
  , nominal   = AnyWordFlag
  , prep      = AnyWordFlag
  , weakvb    = AnyWordFlag
  , passive   = AnyWordFlag
  , anyword   = AnyWordFlag
  , native    = PrettyFlag
  , toc       = NullFlag
  , meta      = NullFlag
  }
