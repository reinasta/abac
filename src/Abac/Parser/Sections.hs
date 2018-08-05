module Abac.Parser.Sections where

import Data.Monoid ((<>))
import Control.Applicative hiding ((<|>))
import Text.Megaparsec

import Abac.Types.ParserTypes
import Abac.Internal (embedAllSections,levToNos,add,fillWith1s)
import Abac.Parser.Operations
import Abac.Parser.Internal
import Abac.Parser.Inlines
import Abac.Parser.Yaml
import Abac.Parser.Blocks

--An optional Yaml div should come first, followed by an optional pure division, followed
--by any sections

divisions :: Parser [Section]
divisions = yaml' <> intro <> sections
  where
    yaml' = (pure <$> try yaml) <|> return []
    intro = (fmap pure $ try $ SecBlocks <$> some block) <|> return []

--the new section

sections :: Parser [Section]
sections = numberSections . embedAllSections <$> (many (try bareSection))



bareSection :: Parser Section
bareSection = ( do
  (SecMark level) <- sectionMarker
  title <- many (try inlineSansNln) <* optional eosPunct <* some newln :: Parser [Inline]
  blcks <- many $ try block
  return $ Section zeros level (Title title) (SecBlocks blcks) []
  <?> "bareSection" )



--numbering sections

numberSections :: [Section] -> [Section]
numberSections secs' = cascadeNo <$> numberTopLevelSections secs'
  where
    numberTopLevelSections :: [Section] -> [Section]
    numberTopLevelSections [] = []
    numberTopLevelSections allsecs@(sec:secs) =
      case sec of
        (Section _ _ _ _ _) -> zipWith assignNo nos allsecs
        _                   -> sec : numberTopLevelSections secs
        where
          assignNo n sect = updateSecno sect n
          nos = fillWith1s <$> levToNos (seclev sec)

    cascadeNo :: Section -> Section
    cascadeNo sc@(Section _ _ _ _ []) = sc
    cascadeNo sc@(Section no l t bdy scs) =
      Section no l t bdy $ cascadeNo <$> zipWith assignNo nos scs
      where
        assignNo n sect = updateSecno sect n
        nos = updateRanges sc (seclev $ head scs)
    cascadeNo other = other


--auxiliary functions

updateRanges :: Section -> Level -> [No]
updateRanges supersec secLev = fillWith1s <$> add (secno supersec) <$> levToNos secLev




