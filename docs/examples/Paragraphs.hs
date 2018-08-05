module Paragraphs where

import qualified Data.Map.Strict as M (Map, fromList, elems)
import qualified Data.Text.IO as T (readFile,writeFile,putStrLn)

import Abac.ParserTypes
import Abac.Parser
import Abac.PartsOfSpeech.Sentences (paraLongAllCustom)
import Abac.Internal (blockPos)

main :: IO ()
main = do
  f <- T.readFile flatland.md
  T.putStrLn $ T.unpack f

longPars :: Document -> M.Map Position Paragraph
longPars doc = M.fromList $ parPos <$> pars doc
  where
    parPos :: Paragraph -> (Position, Paragraph)
    parPos p = (fst (blockPos p), p)
    pars :: Document -> [Paragraph]
    pars dc = M.elems $ paraLongAllCustom 50 dc


-- blockPos :: Block -> (Position, Position)
