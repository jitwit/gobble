load'~/code/gobble/joggle/gobble.ijs'
coinsert'gobble'

test=: monad define
echo 'basic tests'

echo 'testing worlist is sorted' 
assert. (-: /:~) WORDS

echo 'demo board as right number of solutions'
assert. 29 = # gobble ,|:4#"0'DEMO'

echo 'scoring words works'
assert. 11 11 11 5 3 2 1 1 0 0 -: > score_word &.> <\. 'abcdefghij'

echo 'scoring submission works'
assert. 0 = a: score solve 'DADA'
assert. 1 = (<'DADA') score solve 'DADA'
assert. 2 = (;:'DADA ADD') score solve 'DADA'
assert. 0 = (;:'ADD BAD') score solve 'DADA'

echo 'DONE'
)

test''