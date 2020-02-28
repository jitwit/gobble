#!chezscheme
(eval-when (compile load) (optimize-level 3))
(library (dictionary)
  (export prefix?
          word?
          definition
          completions
          anagrams
          collins
          get-collins
          get-collins-word-list)
  (import (chezscheme)
          (only (euler) sort-on nub-eq)
          (trie)
          (only (srfi :13) string-join string-tokenize))
  (include "code/dictionary.scm"))
