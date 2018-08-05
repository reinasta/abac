module Abac.Traverse.Inlines where

import Prelude hiding (Word)


import Abac.Types.ParserTypes
import Abac.Internal

import Abac.Traverse.Internal
import Abac.Traverse.Searchable
import Abac.Traverse.Predicates

getInlines :: Document -> [Inline]
getInlines doc = concatMap unwrapParaPart' (getParaParts doc)

-- does not unwrap comments
unwrapParaPart' :: ParaPart -> [Inline]
unwrapParaPart' (Sentence inls) = inls
unwrapParaPart' (Inlines inls) = inls
unwrapParaPart' (ParaFtn _ snts) = concatMap unwrapParaPart' snts
unwrapParaPart' (Caption prts) = concatMap unwrapParaPart' prts
unwrapParaPart' _ = []

getWordLikes :: Document -> [Inline]
getWordLikes doc = filter wordlike $ getInlines doc

getCitations :: Document -> [Citation]
getCitations doc = filter isCit $ getInlines doc

getLinks :: Document -> [Link]
getLinks doc = filter isLink $ getInlines doc

getImages :: Document -> [Image]
getImages doc = filter isImage $ getInlines doc

getNumbers :: Document -> [Number]
getNumbers doc = filter isNumber $ getInlines doc

getPureWords :: Document -> [Word]
getPureWords doc = filter isWord $ getInlines doc

