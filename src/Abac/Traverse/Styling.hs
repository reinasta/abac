module Abac.Traverse.Styling where

import Abac.Types.ParserTypes
import Abac.Internal

import Abac.Traverse.Internal



emphDocPerBlock :: Document -> M.Map ParaIndex [Inline]
emphDocPerBlock doc = emph . getWordLikesFromBlock <$> (mapit $ getBlocks doc)

boldDocPerBlock :: Document -> M.Map ParaIndex [Inline]
boldDocPerBlock doc = bold . getWordLikesFromBlock <$> (mapit $ getBlocks doc)

emphInBlockNo :: ParaIndex -> Document -> Maybe [Inline]
emphInBlockNo i doc = M.lookup i $ emphDocPerBlock doc

boldInBlock :: ParaIndex -> Document -> Maybe [Inline]
boldInBlock i doc = M.lookup i $ boldDocPerBlock doc

emphDocPerSection :: Document -> M.Map No [Inline]
emphDocPerSection doc = emph . getInlinesFromDiv <$> sectionMap doc

boldDocPerSection :: Document -> M.Map No [Inline]
boldDocPerSection doc = bold . getInlinesFromDiv <$> sectionMap doc



emphInSectionNo :: Document -> No -> Maybe [Inline]
emphInSectionNo doc no = M.lookup no $ emphDocPerSection doc

boldInSection :: Document -> No -> Maybe [Inline]
boldInSection doc no = M.lookup no $ boldDocPerSection doc



emphInDoc :: Document -> [Inline]
emphInDoc doc = filter isEmph $ getInlines doc

boldInDoc :: Document -> [Inline]
boldInDoc doc = filter isBold $ getInlines doc

emphCountInDoc :: Document -> Int
emphCountInDoc doc = length $ emphInDoc doc

boldCountInDoc :: Document -> Int
boldCountInDoc doc = length $ boldInDoc doc


