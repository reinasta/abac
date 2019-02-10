{-# LANGUAGE OverloadedStrings #-}
module Abac.Parser.Document where

import Text.Megaparsec


import Abac.Types.ParserTypes
import Abac.Parser.Operations
import Abac.Parser.Sections
import Abac.Parser.Inlines (parend)
import Abac.Traverse.Internal (getAllSections,toc)

-- document parser
doc :: Parser Document
doc = do
  optional (try parend) -- skip irrelevant newlines
  divs <- divisions
  eof
  return $ Doc divs

