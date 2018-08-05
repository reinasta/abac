> {-# LANGUAGE OverloadedStrings #-}
> module Abac.Tutorials.Search where

> import Abac.Types
> import Abac.PartsOfSpeech
> import Abac.Tutorials.Texts

Audience: this tutorial is aimed at novice Haskell programmers with some programming
experience. You may get by with no Haskell knowledge if what you want is the big
picture of how Abac works.

Abac takes texts, parses them into a tree structure consisting of words, sentences,
paragraphs, sections and other basic text elements, and then traverses the tree in
order to search for the elements the user is interested in.

Searching
=========

We look first at Abac's search capabilities, assuming that we have the parse trees
ready. In the final section, we'll see how to actually use Abac to parse text into
similar tree structures.

The documents that we are going to search are stored in `treelet1` and `treelet2`.
They have type Document which is the most general type of a parsed text string (it
represents a document and includes virtually any document element such as metadata,
sections, paragraphs, and inlines).

> treelet1, treelet2 :: Document
> treelet1 = pdoc2_res
> treelet2 = pdoc5_res

The result of the search will be a value of the Either type, which I use in order
to account for the possiblity of a failed search. An Either value can take one of
the following forms: a text string representing the error we bumped into while
searching (the left value) or the result of the succesful state (the right value).

Basic searches
--------------

The most basic search is looking for the word count of a document:

> wordcount :: InAbac CountResult
> wordcount = runAbc $ count =<< anyword =<< return treelet1

The code used for getting the word count is a pipeline of functions modifying the
input treelet (given on the rightmost side). So we are considering any word in
treelet and count the result.

Abac does not count punctuation elements and similar markers. To see what is
actually counted by the function above we can choose to list the words instead of
counting them:

> wordlist :: InAbac ListResult
> wordlist = runAbc $ list =<< anyword =<< return treelet1

The `wordlist` function collects every word in `treelet1` and returns the resulting
list.

If we want to count some particular type of word, instead of just any word, we
should simly replace `anyword` with other Abac labels. For instance, `preposition`:

> prepcount :: InAbac CountResult
> prepcount = runAbc $ count =<< preposition =<< return treelet1

You can also count abstract words (label `adword`), weak verbs (label `weakverb`),
connectives (label `connective`) and many others.

In Abac's markdown syntax, citatons receive a special marker, so we can search
treelet2 for them:

> citlist ::  InAbac ListResult
> citlist = runAbc $ list =<< citation =<< return treelet2

Likewise, we can find any emphasised words:

> italiclist, boldlist :: InAbac ListResult
> italiclist = runAbc $ list =<< emph' =<< return treelet2
> boldlist = runAbc $ list =<< bold' =<< return treelet2

In addition to counting and listing words, we can also caluculate their
percentages

> italicratio :: InAbac PercentResult
> italicratio = runAbc $ percent =<< emph' =<< return treelet2

If no error occurs, the `italiclist` variable will store the percentage of italicized
words in treelet2, that is, the number of italicized words over the total number of
words in treelet times 100.

Relative searches
-----------------

Abac can do relative searches -- searches within certain elements of the document structure.

The previous percentages were given relative to the entire document, treelet2. But
we can narrow down the search for a percentage to a particular element in our treelet,
for example a certain paragraph. Let's look for the percentage of italicized words in
paragraph 3:

> italicsInPara3 :: InAbac PercentResult
> italicsInPara3 = runAbc $ percent =<< emph' =<< paragraph 3 =<< return treelet2

The new element in our query is the `paragraph 3` filter. The behaviour of such a
filter depends on what it is applied to. In our case it is applied to treelet2, which
is an entire document (type Document). So the search targets the third paragraph of
the document. If instead we wanted to look into the third paragraph of section 3, we
would apply the `paragraph 3` filter to the result of filtering `treelet2` (type
Document) through a `section 3` filter. (This result is of type Section.) Thus, the
code

> italicsInSect3Para3 :: InAbac PercentResult
> italicsInSect3Para3 = runAbc $ percent =<< emph' =<< paragraph 3 =<< section "3" =<< return treelet2

looks into the third paragraph of the third section, rather than the third paragraph of
the document.

We can produce an even more fine grained search by using line filters. Note the new
`line 1` filter in the following query:

> italicsInLine3Sect3Para3 :: InAbac PercentResult
> italicsInLine3Sect3Para3 =
>   runAbc $
>   percent
>   =<< emph'
>   =<< line 1
>   =<< paragraph 3
>   =<< section "3"
>   =<< return treelet2

What we are getting here is the percentage of italicised words in the first line of the
third paragraph of the third section of the treelet2 document.

It may seem surprising that the argument to `section` is a text string rather than an
integer. The reason it's a string is that we must accept arguments like "1.2.1" to
look into the corresponding subsection. Then we can also look into the (yaml) meta
section, if the text contains any metadata, by giving `section` the "meta" string as
argument. Another argument for `section` is "intro", which is a label for the unnumberd
and unnamed section between the meta (which is supposed to come first in the document)
and the first numbered section, if such section exists at all. To illustrate:

> italicsInMeta, italicsInIntro :: InAbac PercentResult
> italicsInMeta = runAbc $ percent =<< emph' =<< section "meta" =<< return treelet1
> italicsInIntro = runAbc $ percent =<< emph' =<< section "intro" =<< return treelet1

The two variables will store the percentages of italicised words in the meta section
(which contains the title, author names, abstract and similar information) and in the
intro section.

Of course, you can perform similar relative searches for things other than percentages
of italic words. For example, you can try to look for lists of citations or the number of
numeric values that occur in the first line of the third paragraph of the third section
of the treelet2 document.

Gramamtical searches
--------------------

We have already seen some queries based on the grammatical categories of words. Among
other things, we looked for prepositions. Now prepositions are typically easy to spot
just by searching for words with a certain shape (e.g. "in", "by" and so on). But this
search method has its pitfalls. For instance if you are looking for demonstratives (words
used to refer to something in the context, e.g. "this" and "that") just by testing words
for equality with such forms, you'll get systematic errors. Clearly, the form "that" is
not doing the same job in "I like that behaviour" and "I like that you've come early".

To overcome the limitations of string-equality tests, Abac brings in proper grammatical
tagging, the so-called part-of-speech (POS) tagging. This feature is optional, as it
slows down the parser, but, conveniently, treelet1 is the result of parsing a text file
and already contains tags. So let's go ahead and look for certain grammatical categories.
What about past participles in the second paragraph?

> pastpartInPara2 :: InAbac ListResult
> pastpartInPara2 = runAbc $ list =<< tagged (Tag "VBN") =<< paragraph 2 =<< return treelet1

Assuming everything went well, the variable `pastpartInPara2` will store all the words in
treelet1 that bear the "VBN" tag. (The full tag is actually: Tag "VBN". But nevermind.)
"VBN" is just the tag for past participles. Abac builds on the Chatter NLP package, which
uses the tag conventions of the Connal corpus. Other interesting tags are "JJ" for
adjectives, and "RB" for adverbs. Just stick one of these tags in the place of "VBN" above
to get the adjectives or the adverbs in the second paragraph of treelet1.

Before getting on, let me point out two problems with Abac's grammatical searches.

Note first that even if POS tagging -- as incorporated by Abac -- is better than string-equality,
it is far from perfect. To become accurate, a POS tagger should be trained on already tagged
text. What Abac relies on is just a minimal model provided by the Chatter library, which is
not yet good enough.

Another limitation of Abac's POS tagging is that it's really slow. Abac depends on markdown
syntax in order to find citations, sections, examples, links and so on. The sluggishness
of POS tagging is due to my hacky way of circumventing the markdown syntax. I haven't yet
found a good way to get around markdown formatting. The problem is that by running the POS
tagger directly on marked up documents, the tags get really messed up. What grammatical
category to assign to "*word*", "$1+1=2$"? The tagger does not recognize such strings. The
way I handle the problem of markdown syntax is to repack each sentence in the AST into a
text string without any markup, then run the tagger on the resulting sentence, and finally
plug the tags back into their adequate slots in the AST. And this must be done for every
sentence in the document!

