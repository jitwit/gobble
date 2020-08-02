#!chezscheme
(eval-when (compile load) (optimize-level 3))
(library (dictionary)
  (export prefix?
          word?
          definition
          completions
	  dictionary?
	  dictionary-language
	  dictionary-trie
          anagrams
          collins
	  yawl
          get-collins
	  get-yawl
          get-collins-word-list)
  (import (chezscheme)
          (only (euler) sort-on nub-eq square)
          (trie)
          (only (srfi :13) string-join string-tokenize)
          (only (srfi :14) char-set char-set-complement))
  
  (include "code/dictionary.scm"))
