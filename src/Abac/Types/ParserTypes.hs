{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Abac.Types.ParserTypes where

import qualified Data.Text as T
import qualified Data.Map.Strict as M (Map,findWithDefault,empty)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Prelude hiding (Word)


--Types
-------

type Text = T.Text

data Document = Doc [Section] deriving (Show,Read,Eq,Generic)

instance NFData Document -- needed for benchmarking

data Title = Title [Inline] deriving (Show,Read,Eq,Generic)

instance NFData Title


data MetaKey = AuthorKey | TitleKey | AbstractKey | DateKey | TagKey | OtherKey Text
  deriving (Show,Read,Eq,Ord,Generic)

instance NFData MetaKey


data MetaValue = MetaSeq [[Inline]]   -- multiple author names, institute names etc.
               | MetaNum [Int]        -- dates, times, etc.
               | MetaInlines [Inline] -- single author, title
               | MetaBool Bool
               | MetaBlocks [Block]   -- the abstract
               deriving (Show,Read,Eq,Generic)


instance NFData MetaValue

-- data Division = Section No Level Title [Division]
--               | Division [Block]
--               | M {keyval :: M.Map MetaKey MetaValue }
--               deriving (Show,Read,Eq)

data Section = Section No Level Title Section [Section]
             | SecBlocks [Block]
             | Meta (M.Map MetaKey MetaValue)
              deriving (Show,Read,Eq,Generic)

secno :: Section -> No
secno (Section no _ _ _ _) = no
secno _ = zeros

updateSecno :: Section -> No -> Section
updateSecno (Section _ lev ttl bdy subs) no = Section no lev ttl bdy subs
updateSecno sct _ = sct

seclev :: Section -> Level
seclev (Section _ lev _ _ _) = lev
seclev _ = (-1)

updateSeclev :: Section -> Level -> Section
updateSeclev (Section no _ ttl bdy subs) lev = Section no lev ttl bdy subs
updateSeclev sct _ = sct

secttl :: Section -> Title
secttl (Section _ _ ttl _ _) = ttl
secttl (Meta mp) =
  let MetaInlines inls = M.findWithDefault (MetaInlines []) TitleKey mp
  in  Title inls
secttl _ = defTitle

defTitle :: Title
defTitle = Title [Word [] (0,0) "No_Title"]

updateSecttl :: Section -> Title -> Section
updateSecttl (Section no lev _ bdy subs) ttl = Section no lev ttl bdy subs
updateSecttl sct _ = sct

secbdy :: Section -> Section
secbdy (Section _ _ _ bdy _) = bdy
secbdy sec = sec

updateSecbdy :: Section -> Section -> Section
updateSecbdy (Section no lev ttl _ subs) bdy = Section no lev ttl bdy subs
updateSecbdy sct _ = sct

updateSecbdyWith :: (Section -> Section) -> Section -> Section
updateSecbdyWith f (Section no lev ttl bdy subs) = Section no lev ttl (f bdy) subs
updateSecbdyWith _ sct = sct

secsbs :: Section -> [Section]
secsbs (Section _ _ _ _ subs) = subs
secsbs _ = []

updateSecsbs :: Section -> [Section] -> Section
updateSecsbs (Section no lev ttl bdy _) subs = Section no lev ttl bdy subs
updateSecsbs sct _ = sct

updateSecsbsWith :: (Section -> Section) -> Section -> Section
updateSecsbsWith f (Section no lev ttl bdy subs) = Section no lev ttl bdy (fmap f subs)
updateSecsbsWith _ sct = sct

secblcs :: Section -> [Block]
secblcs (SecBlocks blcs) = blcs
secblcs _ = []

updateSecblcs :: Section -> [Block] -> Section
updateSecblcs (SecBlocks _) blcs = SecBlocks blcs
updateSecblcs sct _ = sct

keyval :: Section -> M.Map MetaKey MetaValue
keyval (Meta mp) = mp
keyval _ = M.empty

--data Section = Section { secno :: No
--                       , seclev :: Level
--                       , secttl :: Title
--                       , secbdy :: Section -- only: SecBlocks [Block]
--                       , secsbs :: [Section]
--                       }
--              | SecBlocks { secblcs :: [Block] }
--              | Meta {keyval :: M.Map MetaKey MetaValue }
--              deriving (Show,Read,Eq,Generic)


instance NFData Section


type Level = Int
type No = (Int, Int, Int, Int, Int, Int)

zeros :: No
zeros = (0,0,0,0,0,0) :: No

data Block = Para [ParaPart]
           | Footnote Int [Block]
           | BlockEx [Example]
           | BlockQuote Paragraph -- used only in Yaml section
           | BlockQuotes [Block]
           | BlockTech Tech Position Text
           | BlockComment [Block]
           | LinkRef Id Position Url
           | ImageRef Id Position Path
           | Table [ParaPart]
           deriving (Show,Read,Eq,Generic)


instance NFData Block


data Example = Example OrderParam Level No Name ExBody [Example]
  deriving (Show,Read,Eq,Generic)

instance NFData Example

data OrderParam = Ordered | Unordered deriving (Show,Read,Eq,Generic)
instance NFData OrderParam


data ExBody = ExBody {exbdy :: [ParaPart] } deriving (Show,Read,Eq,Generic)
instance NFData ExBody

updateExBdy :: ([ParaPart] -> [ParaPart]) -> Example -> Example
updateExBdy f (Example o l no nom bdy exs) = Example o l no nom (ExBody $ f (exbdy bdy)) exs

updateSubex :: (Example -> Example) -> Example -> Example
updateSubex f (Example o l no nom bdy exs) = Example o l no nom bdy (fmap f exs)

data Marker = ExMark Level No Name
            | ItMark Level No Name
            | FtnMark No Name
            | SecMark Level
            | RefEx No Name
            | RefFtn No Name
            deriving (Show,Read,Eq,Generic)


instance NFData Marker

type Name = Text

-- type synonyms

type Meta = Section -- M M.Map MetaKey MetaValue
type Paragraph = Block -- Para [ParaPart]
type Quote = Block -- BlockQuote Paragraph
type BlockTech = Block -- BlockTech Tech Text
type FootnoteB = Block -- Footnote Int BigFtn
type FootnoteS = ParaPart -- Footnote Int SmlFtn
type Sentence = ParaPart -- Sentence [Inline]
type Word = Inline -- Word [Attr] Text
type Citation = Inline -- Citation Int Text
type InlineMath = Inline -- InlineTech Math Text
type InlineCode = Inline -- InlineTech Code Text
type Punctuation = Inline -- Punct PunctType Text
type Number = Inline -- Number Integer
type Email = Inline -- Email Text Text
type SentFtn = Inline -- SentFtn Int [Sentence]
type LinkRef = Block -- LinkRef Id Url
type ImageRef = Block -- ImageRef Id Path
type Table = Block -- Table [Inlines]
type Inlines = ParaPart -- Inlines [Inline]
type Expressions = ParaPart -- Expressions [Expression]
type Caption = ParaPart -- Caption [ParaPart]

type Url = Text
type Id = Text
type Path = Text

--NB: sentences must be terminated by end-of-sentence punctuation (.?!).

--NB: Footnote & ParaFtn footnotes are assumed to occur after the end of sentences,
--rather than within sentences.

data ParaPart = Sentence [Inline]
              | Inlines [Inline]
              | ParaFtn Int [Sentence]
              | ParaComment [ParaPart]
              | Caption [ParaPart]
              | Expressions [Expression]
              | NullPart
              deriving (Show,Read,Eq,Generic)


instance NFData ParaPart

data Inline = Word [Attr] Position Text
            | Citation [Attr] Int Position Text
            | InlineTech Tech Position Text
            | InlineComment [Inline]
            | Punct PunctType Text
            | Number [Attr] Position Double
            | Link [Inline] UrlOrId
            | Image [Inline] PathOrId
            | Email [Attr] Position Text Text
            | SentFtn Int [Sentence]
            | Other Marker
            | Space
            | Newline
            | ParEnd
            | Null
            deriving (Show,Read,Generic)


instance NFData Inline
instance NFData Expression

instance Eq Expression where
  (==) (Expression inls1) (Expression inls2) = areInlinesEq inls1 inls2

areInlinesEq :: [Inline] -> [Inline] -> Bool
areInlinesEq [] [] = True
areInlinesEq [] _ = False
areInlinesEq _ [] = False
areInlinesEq inls1 inls2 =
  let true x = x == True
      piecemealComparisons = [ inl1 == inl2 | (inl1, inl2) <- zip inls1 inls2 ]
  in all true piecemealComparisons

instance Eq Inline where
  (==) (Word _ _ txt1) (Word _ _ txt2) = txt1 == txt2
  (==) (Word _ _ _) _ = False
  (==) _ (Word _ _ _) = False
  (==) (Email _ _ txt1 txt2) (Email _ _ txt1' txt2') = (txt1 == txt1') && (txt2 == txt2')
  (==) (Email _ _ _ _) _ = False
  (==) _ (Email _ _ _ _) = False
  (==) (Citation _ _ _ txt1) (Citation _ _ _ txt2) = txt1 == txt2
  (==) (Citation _ _ _ _) _ = False
  (==) _ (Citation _ _ _ _) = False
  (==) (InlineTech _ _ txt1) (InlineTech _ _ txt2) = txt1 == txt2
  (==) (InlineTech _ _ _) _ = False
  (==) _ (InlineTech _ _ _) = False
  (==) (Number _ _  dbl1) (Number _ _  dbl2) = dbl1 == dbl2
  (==) (Number _ _ _) _ = False
  (==) _ (Number _ _ _) = False
  (==) (InlineComment inls1) (InlineComment inls2) = inls1 == inls2
  (==) (InlineComment _) _ = False
  (==) _ (InlineComment _) = False
  (==) (Punct _ txt1) (Punct _ txt2) = txt1 == txt2
  (==) (Punct _ _) _ = False
  (==) _ (Punct _ _) = False
  (==) (Link inls1 txt1) (Link inls2 txt2) = inls1 == inls2 && txt1 == txt2
  (==) (Link _ _) _ = False
  (==) _ (Link _ _) = False
  (==) (Image inls1 txt1) (Image inls2 txt2) = inls1 == inls2 && txt1 == txt2
  (==) (Image _ _) _ = False
  (==) _ (Image _ _) = False
  (==) Space Space = True
  (==) Space _ = False
  (==) _ Space = False
  (==) Newline Newline = True
  (==) Newline _ = False
  (==) _ Newline = False
  (==) ParEnd ParEnd = True
  (==) ParEnd _ = False
  (==) _ ParEnd = False
  (==) Null Null = True
  (==) Null _ = False
  (==) _ Null = False
  (==) (Other _) (Other _) = False

instance Ord Inline where
  compare (Word _ _ txt1) (Word _ _ txt2) = compare txt1 txt2
  compare (Word _ _ txt1) (Citation _ _ _ txt2) = compare txt1 txt2
  compare (Word _ _ txt1) (InlineTech _ _ txt2) = compare txt1 txt2
  compare (Word _ _ txt1) (Number _ _ dbl) = compare txt1 (T.pack $ show dbl)
  compare (Word _ _ _) _ = LT
  compare _ _ = LT


type LineNo = Int
type ColumnNo = Int
type Position = (LineNo,ColumnNo)

data Attr = Emph | Bold | Strike | Quoted | DoubleQuoted | Tag Text | Bracketed | Parenthetical | None
  deriving (Show,Read,Eq,Generic)
instance NFData Attr

type Tag = Attr

data Tech = Math | Code deriving (Show,Read,Eq,Generic)
instance NFData Tech

data PunctType = InSentence | EndSentence deriving (Show,Read,Eq,Generic)
instance NFData PunctType


type UrlOrId = Text
type PathOrId = Text
type Image = Inline
type Link = Inline

-- A type synonym for expressions, which are not part of the AST, but are used
-- in AST transformation.

data Expression = Expression [Inline] deriving (Show,Read,Generic)

--Type for all footnotes

data Footnote = FtnB Block | FtnS ParaPart deriving (Show,Read,Eq)

-- Abac does not distinguish between words and their parts, but for the sake of
-- documentation we define:

type Suffix = Word

