#!chezscheme
(eval-when (compile load) (optimize-level 3))
(library (dictionary)
  (export prefix?
          word?
          definition
          suffixes
          completions
          collins)
  (import (chezscheme)
          (trie)
          (only (srfi :13) string-join string-tokenize))
  (include "code/dictionary.scm"))
