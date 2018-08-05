> module Abac.Tutorials.Make where

> import qualified Data.Map.Strict as M (elems,keys,fromList,Map)
> import Text.Megaparsec hiding (count,getPosition)


> import Abac.Types.ParserTypes
> import qualified Abac.Parser as P
> import Abac.PartsOfSpeech.Sentences (paraLongAllCustom)
> import Abac.Traverse (Searchable,blockPos,gatherParaParts,gatherInlines,
>   getPosition,)
> import Abac.Internal (isPureInlines,isSentence,wordlike)
> import Abac.Parser.Internal (withoutAbbreviations')

Having seen some of the basic building blocks of searching document trees, it's time
to look at how to produce such a tree, and then use it for a more complicated task.
We'll (1) parse a text into a tree (the step we skipped above by using ready-made treelets)
and then (2) perform a search on the resulting tree. Our first task will be to spot long
paragraphs.


Long paragraphs
===============

Long paragraphs are challenging for the impatient reader, especially online. We may want to
split them into several smaller ones in the editing phase. What we want then is to get a
list of long paragraphs out of a text file. We work with the flatland text file, which is
under the docs/examples directory in this repository.

We first read the flatland file into a string. Then parse the string into
a Document structure which we store in `doc`. We handle errors in the
next two lines and print the long paragraphs (in Map form, as described below).

> getLongPars :: IO ()
> getLongPars = do
>   txt <- readFile "docs/examples/flatland.md"
>   doc <- runParserT P.doc "" (withoutAbbreviations' txt)
>   let errOrPars = fmap longPars doc
>   let result = either (\_ -> show "parsing error") show errOrPars
>   putStrLn result

To search for long paragraphs we define the helper function `longPars`. The key
function within `longPars` is `paraLongAllCustom`, which takes as arguments an
integer and a document (i.e. a value with type Document, whose structure is defined
by Abac). The integer itself represents the minimal number of words a paragraph
needs to have in order to pass as long. Here we consider a paragraph to be long if
it's over 80 words.

The rest of the code above is mere glue. `M.elems` collects the list of long
paragraphs out of a Map (a dictionary), since we don't need the paragraph indices
represented by the Map. So `pars` will return the list of long paragraphs in the document
structure that `pars` takes as input. To be able to identify the long paragraphs,
we use their position, namely the line and column numbers corresponding to the first
and last word of each paragraph (the job of `parPos`). Finally, we construct
a Map with paragraph positions as keys and paragraphs lengths as values. This Map is
what our `getLongPars` action prints in the end.

> longPars :: Document -> M.Map Position Int
> longPars doc = M.fromList $ parPos <$> pars doc
>   where
>     parPos :: Paragraph -> (Position, Int)
>     parPos p = (fst (blockPos p), lengthInWords p)

>     lengthInWords p = length $ filter wordlike (gatherInlines p)

>     pars :: Document -> [Paragraph]
>     pars dc = M.elems $ paraLongAllCustom 80 dc


As you'll see by evaluating `getLongPars`, there are quite a number of long paragraphs
in the flatland.md file (31, to be more exact). The output is a map of paragragraph
positions and their associated paragraph lengths. (The paragraph lengths are the word
count of each long paragraphs, leaving punctuation aside; cf. lengthInWords.)


We can generalize getLongPars by defining getLongParsWith. This uses an auxiliary
function f to control which results about long paragraphs are being displayed. If we want,
for instance, just the number of long paragraphs (instead of the entire list) we should
use (length . M.elems) for the f parameter of our generalized function, getLongParsWith.
Using getLongParsWith we can also write a function that returns the positions of the long
paragraphs as well as another version of getLongPars itself.

> getLongParsWith :: Show a => (M.Map Position Int -> a) -> IO ()
> getLongParsWith f = do
>   txt <- readFile "docs/examples/flatland.md"
>   doc <- runParserT P.doc "" (withoutAbbreviations' txt)
>   let errOrPars = fmap (f . longPars) doc
>   let result = either (\_ -> show "parsing error") show errOrPars
>   putStrLn result

> getNumLongPars :: IO ()
> getNumLongPars = getLongParsWith (length . M.elems)

> getPosLongPars :: IO ()
> getPosLongPars = getLongParsWith (removeHeadingSpc . posToStr . M.keys)
>   where
>     removeHeadingSpc :: String -> String
>     removeHeadingSpc = dropWhile (== ' ')
>     posToStr :: [(Int,Int)] -> String
>     posToStr [] = ""
>     posToStr ((l,c):ps) = " " ++ show l ++ ":" ++ show c ++ posToStr ps

> getLongPars' :: IO ()
> getLongPars' = getLongParsWith id

Sentence lengths
================

Writing texts with sentences of (roughly) the same length may appear monotonous. To
check that the lengths of sentences vary to a satisfactory extent, we may want to
produce a Map whose values are sentence lengths and whose keys are the position of
the corresponding sentences. In this way, it is easy to spot length uniformity and
go to the offending positions in the text file.

So what are the lengths of sentences in flatland.md? The first step is to read the file
into a text string and parse the string (handling any potential error), just as we did
in the previous section.

> getSentenceLengths :: IO ()
> getSentenceLengths = do
>   txt <- readFile $ "docs/examples/flatland.md"
>   doc <- runParserT P.doc "" (withoutAbbreviations' txt)
>   let errOrMap = fmap (sentPosLen . sentences) doc
>   let result = either (\_ -> show "parsing error") show errOrMap
>   putStrLn result

After storing the document tree in `doc`, the rest of the code calls the key functions
that compute sentence lengths, and does some rudimentary error handling. Our key
functions select sentences (`sentences`) and give their lengths and positions (`sentPosLen`).

A sentence is a paragraph part -- a value of type ParaPart. `gatherParaParts` takes
as argument any element of a document tree and returns all the paragraph parts within
that element, including sentences. Apart from sentences, other paragraph parts include
footnotes and sets of inlines without end-of-sentence punctuation (".!?"). Out of all
these paragraph parts we'll select just the sentences (type Sentence) or inlines (type
Inlines). We include Inlines values together with sentences because they resemble sentences
well enough; as I said, they are lists of inline elements without end-of-sentence punctuation.

> sentences :: (Searchable a) => a -> [ParaPart]
> sentences el = filter (\x -> isSentence x || isPureInlines x) (gatherParaParts el)

We now have a list of sentence-like elements, namely the result of filtering
all the paragraph parts in a document element `el` according to whether such
parts are Sentence or Inlines values. Note: the Inlines (mark the 's') type is
just a wrapper around a list of inlines, while individual inlines are of type
Inline (without 's').

All that remains for us to do is to extract the positions of each sentence-like
element, take the length of each element and build a map out of the resulting
(`sentence position`, `sentence length`) pairs. In calculating the length of a sentence
we are discounting punctuation; only words are counted.

> sentPosLen :: [ParaPart] -> M.Map Position Int
> sentPosLen sents =
>   let lengthInWords snt = length $ filter wordlike (gatherInlines snt)
>   in  M.fromList [ (fst (getPosition sent), lengthInWords sent) | sent <- sents ]

Putting together the previous two functions, we get a function that takes a document
element and returns, for each sentence, its position and length. This function is
`sentPosLen . sentences` and is responsible for the result printed by the main action,
`getSentencesLength`.


