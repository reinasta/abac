> module Abac.Tutorials.Counter where


> import Text.Megaparsec (runParserT)

> import qualified Abac.Parser as P
> import Abac.Types.ParserTypes
> import Abac.Traverse.Predicates (isSentFtn, isFootnoteS, isFootnoteB)
> import Abac.Traverse (gatherParaParts, gatherBlocks, gatherInlines, countWordsInInlines,
>   countWordsInParaParts', countWordsInDocument, countWordsInBlocks)
> import Abac.Parser.Internal (withoutAbbreviations')

Two word-counts
===============

Abac's word-counting functions count the words in the main text and the words in footnotes,
though not also those in comments. But in some cases one may want to exclude the footnotes
from the counting. In this tutorial we write two simple functions that count words in a
document, by either counting in or counting out the footnote words, as the case may require.

The strategy is the following. We have a function, countWordsInDocument, that returns the total
number of words in a document (without the words in comments, as mentioned earlier). We also have
a way of determining the number of words in footnotes; this will be done via countWordsInFootnotes,
which is given below. Then all we have to do in order to get a word-count that excludes words in
footnotes is to subtract the number of words in footnotes from the total number of words.

To count words in footnotes, we have to take into account that Abac has three different notions
of footnote: block-level, sentence-level and inline-level. The function countWordsInFootnotes
counts words in each of these kinds of footnote and then sums up the results.

> countWordsInFootnotes :: Document -> Int
> countWordsInFootnotes doc =
>   (countWordsInInlines . filter isSentFtn . gatherInlines) doc +
>   (countWordsInParaParts' . filter isFootnoteS . gatherParaParts) doc +
>   (countWordsInBlocks . filter isFootnoteB . gatherBlocks) doc

At this point we have the total number of words, which is given by countWordsInDocument, as
well as the number of words in footnotes, which is given by countWordsInFootnotes. The following
functions return, respectively, the total count and the difference between the total count and the
footnote count. These word-counts are produced on the flatland.md document, which has, apart from
the main text, a single, nine-word footnote; so the integers returned by the functions below should
differ by this margin.

> countWordsWithFootnotes :: IO ()
> countWordsWithFootnotes = do
>   txt <- readFile "docs/examples/flatland.md"
>   doc <- runParserT P.doc "" (withoutAbbreviations' txt)
>   let errOrWdCount = fmap countWordsInDocument doc
>   let result = either (\_ -> show "parsing error") show errOrWdCount
>   putStrLn result

> countWordsWithoutFootnotes :: IO ()
> countWordsWithoutFootnotes = do
>   txt <- readFile "docs/examples/flatland.md"
>   doc <- runParserT P.doc "" (withoutAbbreviations' txt)
>   let errOrWdCount = fmap countWordsInDocument doc
>   let errOrFnCount = fmap countWordsInFootnotes doc
>   let errOrAggregate = (-) <$> errOrWdCount <*> errOrFnCount
>   let result = either (\_ -> show "parsing error") show errOrAggregate
>   putStrLn result


NB: In the current case, Abac returns results in an Either-type value of the form
'Either error count'. In each of the two functions above, the computation within
the result variable handles the potential errors encoded in the Either-type value.
Also, the computation within the errOrAggregate in the penultimate function is meant
to apply subraction (-) to the 'count' parameters inside two Either-type values,
errOrWdCount and errOrFnCount, which potentially hold -- if no error occurred --
two integers resulting from counting the total number of words and the words in
footnotes.
