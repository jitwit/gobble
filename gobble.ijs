words=: 'b'freads<'~/code/gobble/input/collins.txt'

dice4=: _6]\'NAEAEGEGNWEHCSOAHPLVERDYTOATOWPKAFFSHRVTWEHQUMNIEITSSORLTYETTITSYDLXEDIRTOIMCUBAOBOJNLNHZRENSIEU'

roll=: monad : '(? (#"1 y)) {"0 1 y'

board=: roll dice4

moves=: 0 0 -.~ ,/ ,"0/~ i:1
reify_grid=: [:-.&_1&.>[:,[:<"_2[:,"_2/moves|.!._1]
grid0=: reify_grid i. 4 4

adj=: [:>{&grid0
grow=: ,"_ 0]-.~[:adj{:

prefix=: monad : 'y e. (#>y)&{. &.> words {~ (#words) | (,~ <:) words I. y'
exact=: -:[:{&words words&I.

solve=: verb define
board=. y[words=. '' [ curr=. |: ,: i.#y
while. #curr do.
  ws =. ''[next =. ,: 0,{.curr
  for_j. curr do.
    for_k. grow j do. w=. <('Q';'QU') stringreplace k{board
      if. exact w do. ws=. ws,w end.
      if. prefix w do. next=. next,k end.
    end.
  end. curr=. }.next[words=. words,~.ws
end. ,. (/: #&.>) /:~ words
)

demo=: monad define
board=: roll dice4
echo ,. solve board
echo _4]\<"0 board
)
