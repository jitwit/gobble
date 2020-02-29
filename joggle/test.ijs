load'~/code/gobble/joggle/gobble.ijs'
coinsert'gobble'

test_solver=: monad define
echo 'basic tests for solver'

echo 'testing worlist is sorted' 
assert. (-: /:~) WORDS

echo 'demo board as right number of solutions'
assert. 29 = # gobble ,|:4#"0'DEMO'

echo 'DONE'
)

test_solver''