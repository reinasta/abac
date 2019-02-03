{-# LANGUAGE OverloadedStrings #-}
module Abac ( runAbac
            ) where

import Options.Applicative hiding (command)
import Prelude hiding (Word)

import Abac.Cli
--import Abac.Command




{- ISSUES
Parser.Examples -- subordinates -- doesn't check level at all
ParserTypes -- marker types are confusing

-}


runAbac :: IO ()
runAbac = cmmdToIO =<< execParser withInfo -- printCommand =<< execParser withInfo


{- API

Abac.Traverse.Internal : Searchable class, toc, get meta info

Abac.PartsOfSpeech : meta filters, etc. etc.

Abac.Types.ParserTypes
Abac.Types.Result

-}

-- Filters


-- Types

-- Results


