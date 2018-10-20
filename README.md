
# Intro

Text queries for markdown authors

We often use texts not only for our reading pleasure but also for storing,
retrieving and modifying information. Books are one traditional store of
information. Indexes, tables of contents, and page numbers offer
ways to organize and access information. Working in a digital environment opens
up new ways of interacting with texts. Abac's aim is to ease authors'
interaction with Markdown documents. Abac provides a rich representation
of text documents, which goes beyond Markdown-specific formatting to include
text elements such as italicized words, quotations, sections, meta data, and
even some grammatical categories like prepositions and passive verbs.


Tags: text processing, natural language processing, library, command line utility,
plain text, markdown, word counting, text query

> Caveat: Abac is not yet a functional piece. 
> I give some indication of what's missing in the road map. 
> In the project outline I'm talking more of what Abac *should* be, rather
> than what it is. 
> To show how Abac is supposed to work, I'll focus on functionality that works
> on suitably restricted texts---texts I chose by hand so as to avoid crashes. 

Abac is a text querying library targeting markdown documents. It aims
to offer a comprehensive representation of text documents, in order to
create more meaningful ways of searching, quantifying and modifying texts. 
Abac can be used as a writing assistant (prose linter) or style analysis tool.
The library is accompanied by a command line interface to showcase its key
features.
    
Abac can search within elements of markdown documents. It can tell you,
first, how many words there are in parts of a text such as the title, abstract,
footnotes, sentences, paragraphs, comments, sections etc. So, at its most basic,
Abac is a discriminate word counter: it counts elements within the structural
parts of markdown documents. 

Second, Abac adopts natural language processing (NLP) functionality like part
of speech (POS) tagging to improve on its word search, so that we can 
better distinguish between (and search for) words on grammatical grounds,
rather than just based on string characters comparisons. We can thus search
for (and count and calculate percentages of) the prepositions, passive verbs,
connectives, determiners, abstract nouns etc. in the text divisions of a
markdown document.

Third, Abac tracks the styling of words so it can search for (count etc.) the words
in italics or bold. It can also retrieve words in quotation marks or parentheses.

So, besides producing the word count for an entire text, you can determine
how many passive verbs occur in, say, paragraph 3, in the first section,
in the abstract, or on line 55 of the document.

Yet counting is not all Abac can do. With a bit of assistance Abac can
reinforce a writing style. And this is why I started the Abac project. I ask
myself how many footnotes and citations can a reader bare. How long should
a paragraph be? How many passives should I use, and when? How much italicized
text should I allow per paragraph? Once I have answers to such questions I can
proceed to track the number of footnotes, citations, passive verbs, and the
lengths of paragraphs, and cut down on the items in excess.

I built Abac to serve authors of prose and especially technical writers,
including academics. But anyone interested in text and natural language
processing on markdown files may take an interest in the project.


# Install

To use Abac the only quick option for now is to download the binary. It should
work on Linux systems and Mac OS. Apologies to Windows users, as I haven't yet
got round to make Abac work on their systems as well.

# Quick start

Here I'll focus on the command line utility. The library's documentation will be
given separately.

Let's give ourselves a text to work with. Save the following text in `test.txt`:

```shell
cat > test.txt
In all the higher forms this process cannot be kept up indefinitely. After a while they succumb; they die. The creature is not equal to the task of indefinite self-renewal. But continuity of the life process is not dependent upon the prolongation of the existence of any one individual. Reproduction of other forms of life goes on in continuous sequence. And though, as the geological record shows, not merely individuals but also species die out, the life process continues in increasingly complex forms. As some species die out, forms better adapted to utilize the obstacles against which they struggled in vain come into being. Continuity of life means continual readaptation of the environment to the needs of living organisms. (John Dewey, Democracy and Education) 
```


Finish with Ctrl-D to save the file and to bring the terminal prompt back.

Download the abac executable (called simply `abac`) in the same folder as `text.txt`.

## Listing and counting words

The simplest thing Abac can do is give you the word count for the entire text:

`abac -f test.txt --anyword --whole`

> Note: if the previous command returns 'command not found' (even if the abac
> executable is in the current directory) you might need the `./` prefix to the 
> abac executable, that is, `./abac` followed by the rest of the command.
> Alternatively, you can add the current directory to the path
> via `export PATH=$PATH:.` and forget the prefix.

If you want the word count at a certain paragraph, add the `-p` parameter
to the previous command, as I do below:

`abac -f test.txt --anyword --whole -p 1`

Abac will respond with the number of words used in paragraph 1. (The text in file `test.txt`
has just one paragraph so if you ask anything about any other paragraph you'll get an error.)

To search for a specific kind of word rather than any word, replace the '--anyword' option with
one corresponding to one of the word categories that Abac recognizes. For instance, if you need
a list of the prepositions occurring in `test.txt` do

`abac -f test.txt --preposition`

Note: each preposition in the list will be prefixed with the line and column number corresponding
to its position in the document. For instance, the preposition "in" occurs at the
beginning of line 1 (hence at position 1:0). To change the way the commandline utility
displays the results you can opt for the native representation using the '--native' option.

If instead you want the preposition count just add `-#` (or `--whole`) to the previous command,
to get the new commad:

`abac test.txt --preposition -#`

The output will be the number of prepositions in the text (namely, 18). As before,
this command can be further specified with the paragraph number to get the preposition
count at that paragraph. 

But paragraph numbers are usually difficult to track. If you use a text editor,
you have easy access to line numbers. Abac can count at line numbers too. Just do

`abac test.txt --preposition -# -l 1`

which produces the preposition count at line 1.

Now suppose that you are not particularly interested in individual words but in
groups of n adjacent words, where n is a positive integer. These groups are
called 'ngrams'. See if you can guess what the following is doing:

`abac text.txt --ngram 3 --whole`

What about the following?

`abac text.txt --ngram 7 -l 1`

The previous two commands are (respectively) counting expressions of 3 words in
the whole `test.txt` document and listing the expressions of 7 words at line 1.

Note: ngrams consists of only word-like, inline elements; punctuation elements
are skipped.

There is much more functionality as well as some useful shortcuts; all these you
can consult by running `abac --help**. 

If these ideas seem appealing go to the tutorials where the Abac library is
discussed more at length.

## Styling-based searches

Let's give ourselves another text to work with to show off styling-based searches.
At the terminal prompt type:

```shell 
cat > style_test.md
**Sed ut perspiciatis *unde* omnis iste natus error sit** voluptatem
accusantium doloremque laudantium, totam (rem *aperiam*), 
[eaque ipsa quae ab illo **inventore veritatis** et quasi architecto 
beatae vitae dicta sunt explicabo]. 
```
On the first line there is one italicized word. We can find it with the following
command:

`abac -f style_test.md --italic -l 1`

To search for words in bold on the first line, we do

`abac -f style_test.md --bold -l 1`

To search for words in italic and bold, we use

`abac -f style_test.md --italic --bold -l 1`

To find the words enclosed in parenthesis on line two, we run

`abac -f style_test.md --paren -l 2`

The italic words enclosed in parentheses can be obtained by running

`abac -f style_test.md --italic --paren -l 2`

In a similar fashion we identify the words in bold in the first paragraph:

`abac -f style_test.md --bold -p 1`

To find out the number of bolded words in the first paragraph we add the `--whole`
flag to the previous command to get:

`abac -f style_test.md --bold -p 1 --whole`

To find all the words that appear in bold and italic we run:

`abac -f style_test.md --bold --italic -p 1`

> Note: Abac is not always working as expected when given multiple
> styling and formatting flags

## Grammar-based searches

TO DO

# Design and features

Abac thinks of documents the same way we think about them.
Documents contain words, sentences, paragraphs, and sections.
Words make up sentences. 
Sentences make up paragraphs. 
Paragraphs make up sections, along with other section constituents such
as titles and potential subsections, which in turn have their own titles
and potential subsections.
The document structure just described is richer than the one exposed by most markdown parsers.
(E.g. For Abac, titles belong to sections rather than being yet another adjacent block,
as in the case of typical markdown parsers.) 
Abac parses documents to fit into this richer -- and more familiar -- structure. 

The key benefit of Abac's document model is that it makes it easy to ask direct questions
about words, sentences, paragraphs, and sections. We can search for -- and count in --
any of these document elements.

Several other distinguishing features of Abac are worth mentioning: 

- Abac explicitly registers styling and formatting features of words, e.g.
if they are in quotation marks, brackets, italics, or bold; so you can search for
words that have any of these features.
- Abac keeps track of the words' location in the document, making it possible
to relate the relevant words back to the original document.
- Abac gives grammatical (part of speech) information about the words in the document,
which means that words can be searched according to their grammatical category.

So Abac is able to query markdown formatted texts for a broad range of text entities,
and can be used as a discriminate word counter and, more ambitiously, as a writing
assistant and analyzer.


# Motivation

There are two sorts of motivation for using Abac, one is to do with its word-counting
capabilities while the other concerns the control of one's writing style. The first
motivation is more concrete, so let's start with it.

For many word counting tasks, we can make do with the features provided by text
editors. We word-count all the text, and use 'count under selection' for
more fine grained counts (like the count in the last two paragraphs).

But how would the traditional, text-editor methods fair in the following situations?

- your paper or blog post contains latex macros; 
- you want the overall word count and have comments not meant to appear in
the output text.
- what if you don't want to include the footnotes and the references in your count? 
- what if you want to know the relative lengths of the essay's sections?
- and what do you do if you need some control over the lengths of the paragraphs in
your blog post or the lengths of slides in a presentation?

In such situations a whole-text count becomes unreliable and the counting under
selection gets unbearably tedious.

When it comes to inline-elements spread throughout the text the standard methods
of word counting are totally inadequate. Counting the citations, the links, the words
in bold or italics etc. is simply not the job of a word counter, and using the
search facilities of a text editor is too laborious and time consuming.

With Abac, you can do all these and much more.

The second motivation for Abac is that it can be set up as a writing assistant. 

The number and type of words that you use are essential to the style of
your writing. Abac knows how to count and can recognize some word types.
As such it captures an aspect of writing style. Bloating your text with
prepositions and abstract nouns makes it more difficult to digest. Using
weak verbs makes it dull or unconvincing.

Abac takes the edge off the first editing stage. It picks out what's potentially
problematic in you writing and exposes it to you for revision.
You can configure Abac to your own liking. 
You can also play with Abac on texts you like (or dislike) in order to adopt (or
avoid) some of their stylistic features.

While Abac will not make you a great stylist, it will help you avoid some of the
common pitfalls of dull and wooden writing.


# Tools

Abac is a library and command-line utility written in Haskell. It works with
plain text files. Its main feature is the support of markdown documents,
which means that we can ask questions about a number of different parts of a
document such as sections and footnotes.

To be able to do its job, Abac supports a subset of Markdown syntax, extended
with a subset of Pandoc's markdown (itself an extension of Markdown).

The key module of the Abac library is the parser.

Abac combines a parser for the main elements of a markdown document using
intermediate container types for paragraph elements (e.g. sentences) and sections
with an NLP tagger to get a tree of the document annotated with formatting information
(emphasised text, quotations, citations etc.) as well as the grammatical category
of most words. This makes it possible to query marked up texts for a number of
typographical and grammatical features.


# Abac's markdown

Abac's markdown syntax comes with a number of quirks:

- an example (or bullet point) block should be preceded by at least a newline,
and by two newlines if the example block occurs after another block (e.g. paragraph)
- definition lists are not yet supported
- there is just one way to add emphasis to text, namely using asterisks:
  + italic text is marked up \*like this\*, and
  + bold text is marked up \*\*like this\*\*
- the yaml section (containing the title, abstract etc.) can appear only at the
beginning of the document; and there is no other special way of marking up meta data
- there is a limit to the level of embedding accepted for sections, examples and
bullet points, and that limit is 6. So you can have a section like 1.2.3.4.5.6, but
this section cannot embed any additional subsection (like 1.2.3.4.5.6.1). Likewise
for bullet points and numbered examples.
- footnotes may appear only at the end of a sentence, after the end-of-sentence
punctuation, or at the end of a group of inlines that is not part of a sentence [fixed]
- footnotes should not contain square brackets, '[' and ']' [fixed]
- small footnotes (within paragraphs) should contain full sentences, which end
with adequate punctuation (one or more of '.?!') [fixed]
- backticks are reserved for code blocks and cannot be used as openning single quotes
- colon must be followed by a white space character unless it appears in references
(of the form @author:year)
- paragraph-level matching punctuation (quotations, parentheses etc.) is not yet supported.
In other words, two paragraphs cannot be enclosed in parentheses or be quoted together.
- matching punctuation starting mid-sentence should be closed before the sentence ends;
in other words matching punctuation cannot cross sentence boundaries while preserving
a correct parse of sentences. A corollary:
  + inner sentence quotations must precede the full stop "like so". Not "like so." The
latter will be parsed but probably not as intended ("like so." will be considered a full
sentence rather than just a part of sentence)
- inner word styling (e.g. \*un\*cool) is not parsed correctly.

I expect to fix these sooner or later.


# Getting involved

If you are a writer looking for a new feature or a developer willing to
implement such a feature (or both), please write to me via the github interface or
simply by email. Abac is in an experimental phase and any (conceptual or technical)
discussion will be helpful at this stage, until I get clear on what this library
might become.


# Road map

Abac is still a broad idea with a rough execution plan. If you look at the
code or try running Abac, you'll notice a number of significant problems.
Many of the core features remain yet unimplemented or buggy. The code base
is messy. It is not tested well enough.  Abac's performance is poor; the POS
tagging layer, in particular, requires cool patience.

In the next couple of months, I'll be focusing on the paths leading on to
(or from) Abac's abstract syntax tree, and more specifically on

- implementing a consistent use of the Text or String type (Parser modules)
- removing the quirks in Abac's markdown syntax (Parser modules)
- making Abac work on Windows (Parser and Cli modules)
- completing the test suite for the Abac parser (Spec modules)
- improving the POS tagging layer (Parser and ParserTypes modules)
- adding a writer for Abac's abstract syntax tree
- writing a better result type for Abac's output


# Similar software

The projects listed below are more mature than Abac and may differ significantly
from Abac in the technology used, their specific goals and ambitions. The common
thread is that these projects can serve as writing assistants or text-analysis tools.

- [hemingway](http://www.hemingwayapp.com/): writing assistant, free to use online, but proprietary.

- [newspaper](https://github.com/codelucas/newspaper/): nice library for querying web pages

- [pandoc](http://pandoc.org/) excellent command-line utility and library aimed at converting text between different formats (e.g. docx and tex); the library can be used to query markdown documents (e.g. list the links in the document)

- [proselint](https://github.com/amperser/proselint): very interesting and comprehensive project; it offers warnings about misuse of punctuation, words and phrases.

- [writer's diet](http://writersdiet.com/test.php): free to use online for texts of no more than 1000 words; interesting features but more of a toy app and limited UI

Please let me know of any other open source projects that may fall within the broad category of
writing assistants and would complement this list.



