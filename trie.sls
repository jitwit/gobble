#!chezscheme
(eval-when (compile load) (optimize-level 3))
(library (trie)
  (export trie?
          trie-element
          trie-tries
          trie-ref  ; string
          trie-ref* ; list
          lookup
          lookup-char
          trie-prefix?
          trie-member?
          dictionary->trie
          store-trie
          fetch-trie
          trie-paths)
  (import (prefix (patricia) t:)
          (only (srfi :1) append-map)
          (chezscheme))
  (include "code/trie.scm"))
