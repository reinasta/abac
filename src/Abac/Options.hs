{-# LANGUAGE OverloadedStrings #-}
module Abac.Options where


import Prelude hiding (Word)

import Abac.Types.ParserTypes
import Abac.Internal (mkWords)


adwordKW :: [Suffix]
adwordKW = mkWords
 ["able", "ac", "al", "ant", "ary", "ent", "ful",
  "ible", "ic", "ive", "less", "ous"]

connectiveKW :: [Word]
connectiveKW = mkWords
  ["but", "and", "however", "nevertheless","nonetheless", "moreover",
  "thus", "hence", "so", "therefore"]

indexicalKW :: [Word]
indexicalKW = mkWords
  ["this", "that", "those", "these"]

abstractKW :: [Suffix]
abstractKW = mkWords
  ["ion", "ism", "ty", "ment", "ness",
  "ance","ence"]

prepositionKW :: [Word]
prepositionKW = mkWords
  ["in", "on", "of", "which", "that", "to", "for", "at",
  "by", "with"]

verbweakKW :: [Word]
verbweakKW = mkWords
  ["have", "do", "show", "is", "am", "are",
  "was", "were", "be", "being", "been"]

pastpartKW :: [Word]
pastpartKW = mkWords
  ["arisen", "beat", "been", "become", "begun", "bent", "bet",
  "bitten", "bid", "bet", "bled", "blown", "bred", "built",
  "blown", "born", "broken", "brought","burst", "bought",
  "cast", "caught", "chosen", "clung", "come", "cut", "cost",
  "crept", "dealt", "done", "drawn", "drunk", "driven",
  "eaten", "fallen", "fed", "felt", "fought", "found", "flown",
  "forbidden", "forecast", "forgotten", "forgiven", "frozen",
  "got", "gotten", "given", "gone", "grown", "had", "heard",
  "hidden", "held", "hurt", "kept", "knelt", "known", "laid",
  "led", "left", "lent", "let", "lain", "lost", "made", "mislaid",
  "misled", "mistaken", "met", "overcome", "overtaken", "paid",
  "pled", "put", "quit", "read", "rid", "ridden", "run", "rung",
  "risen", "said", "sat", "seen", "shed", "sought", "sold",
  "sent", "shaken", "shone", "shot", "shrunk", "shut", "sung",
  "sunk", "sat", "slit", "spread", "slept", "spoken", "spent",
  "sprung", "stood", "stolen", "stunk", "struck", "strung",
  "striven", "sworn", "swept", "swum", "swung", "taken",
  "taught", "thrust", "torn", "told", "trodden","thought",
  "thrown", "understood", "undertaken", "upset", "waked",
  "woken", "worn", "won", "written", "withdrawn", "withstood",
  "wrung", "woven"]

auxiliarKW :: [Word]
auxiliarKW = mkWords
  ["was", "were", "is", "am", "are", "been", "be"]

stopwordKW :: [Word]
stopwordKW = mkWords
   ["a", "about", "above", "across", "after", "afterwards",
  "again", "against", "all", "almost", "alone", "along",
   "already", "also", "although", "always", "am", "among",
   "amongst", "amoungst", "amount", "an", "and", "another",
   "any", "anyhow", "anyone", "anything", "anyway", "anywhere",
   "are", "around", "as", "at", "back", "be", "became",
   "because", "become", "becomes", "becoming", "been",
   "before", "beforehand", "behind", "being", "below",
   "beside", "besides", "between", "beyond", "bill", "both",
   "bottom", "but", "by", "call", "can", "cannot", "cant",
   "co", "computer", "con", "could", "couldnt", "cry", "de",
   "describe", "detail", "did", "do", "done", "down", "due",
   "during", "each", "eg", "eight", "either", "eleven", "else",
   "elsewhere", "empty", "enough", "etc", "even", "ever",
   "every", "everyone", "everything", "everywhere", "except",
   "few", "fifteen", "fifty", "fill", "find", "fire", "first",
   "five", "for", "former", "formerly", "forty", "found",
   "four", "from", "front", "full", "further", "get", "give",
   "go", "had", "has", "hasnt", "have", "he", "hence", "her",
   "here", "hereafter", "hereby", "herein", "hereupon", "hers",
   "herself", "him", "himself", "his", "how", "however",
   "hundred", "i", "ie", "if", "in", "inc", "indeed",
   "interest", "into", "is", "it", "its", "itself", "keep",
   "last", "latter", "latterly", "least", "less", "ltd", "made",
   "many", "may", "me", "meanwhile", "might", "mill", "mine",
   "more", "moreover", "most", "mostly", "move", "much",
   "must", "my", "myself", "name", "namely", "neither", "never",
   "nevertheless", "next", "nine", "no", "nobody", "none",
   "noone", "nor", "not", "nothing", "now", "nowhere", "of",
   "off", "often", "on","once", "one", "only", "onto", "or",
   "other", "others", "otherwise", "our", "ours", "ourselves",
   "out", "over", "own", "part", "per", "perhaps", "please",
   "put", "rather", "re", "s", "same", "see", "seem", "seemed",
   "seeming", "seems", "serious", "several", "she", "should",
   "show", "side", "since", "sincere", "six", "sixty", "so",
   "some", "somehow", "someone", "something", "sometime",
   "sometimes", "somewhere", "still", "such", "system", "take",
   "ten", "than", "that", "the", "their", "them", "themselves",
   "then", "thence", "there", "thereafter", "thereby",
   "therefore", "therein", "thereupon", "these", "they",
   "thick", "thin", "third", "this", "those", "though", "three",
   "three", "through", "throughout", "thru", "thus", "to",
   "together", "too", "top", "toward", "towards", "twelve",
   "twenty", "two", "un", "under", "until", "up", "upon",
   "us", "very", "via", "was", "we", "well", "were", "what",
   "whatever", "when", "whence", "whenever", "where",
   "whereafter", "whereas", "whereby", "wherein", "whereupon",
   "wherever", "whether", "which", "while", "whither", "who",
   "whoever", "whole", "whom", "whose", "why", "will", "with",
   "within", "without", "would", "yet", "you", "your",
   "yours", "yourself", "yourselves", "b", "c", "d", "e", "f",
   "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r",
    "s", "t", "u", "v", "w", "x", "y", "z"]



