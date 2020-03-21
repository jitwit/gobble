load'~/code/gobble/joggle/gobble.ijs'
coinsert'gobble'

test=: monad define
echo 'basic tests'

echo 'testing worlist is sorted' 
assert. (-: /:~) WORDS

echo 'demo board as right number of solutions'
assert. 29 = # gobble ,|:4#"0'DEMO'

echo 'scoring words works'
assert. 11 11 11 5 3 2 1 1 0 0 -: score_word\. 'abcdefghij'

echo 'scoring word list works'
assert. 0 = a: score gobble 'DADA'
assert. 1 = (<'DADA') score gobble 'DADA'
assert. 2 = (;:'DADA ADD') score gobble 'DADA'
assert. 0 = (;:'ADD BAD') score gobble 'DADA'

echo 'DONE'
)

test''