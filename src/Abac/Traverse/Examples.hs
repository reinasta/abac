module Abac.Traverse.Examples where

import Abac.Types.ParserTypes
import Abac.Internal


import Abac.Traverse.Internal
import Abac.Traverse.Searchable


--Example Numbering

numExamples :: Int -> [Example] -> [Example]
numExamples _ [] = []
numExamples n (ex:exs) =
  let intToNo i = (i,0,0,0,0,0)
  in  case ex of
        Example Ordered lev _ nom bdy exs' ->
          (Example Ordered lev (intToNo n) nom bdy exs') : numExamples (n + 1) exs
        _ -> numExamples (n + 1) exs

numberExamples :: Document -> [Example]
numberExamples doc = numExamples 1 $ gatherExamples doc



