#!chezscheme
(eval-when (compile load) (optimize-level 3))
(library (trie)
  (export trie?
          lookup-prefix
          lookup
          trie-prefix?
          trie-member?
          dictionary->trie
          store-trie
          fetch-trie)
  (import (prefix (patricia) t:)
          (chezscheme))
  (include "code/trie.scm"))
