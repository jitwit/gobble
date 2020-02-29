require'~/code/gobble/joggle/gobble.ijs'
coinsert'gobble'

demo=: monad define
board=. roll dice4
echo ,. gobble board
echo _4<"0\ board
)

demo''