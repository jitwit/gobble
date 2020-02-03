#!chezscheme
(eval-when (compile load) (optimize-level 3))
(library (dictionary)
  (export prefix?
          word?
          collins)
  (import (chezscheme)
          (trie))
  (include "code/dictionary.scm"))
