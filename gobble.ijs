words=: 'b'freads<'~/code/gobble/input/collins.txt'

dice4=: _6]\'NAEAEGEGNWEHCSOAHPLVERDYTOATOWPKAFFSHRVTWEHQUMNIEITSSORLTYETTITSYDLXEDIRTOIMCUBAOBOJNLNHZRENSIEU'
roll=: monad : '(? (#"1 y)) {"0 1 y'
board=: roll (16?16) { dice4

reify_grid=: [:-.&_1&.>[:,[:<"_2[:,"_2/(0 0-.~,/,"0/~i:1)|.!._1]
adj=: ] ,"_ 0 ] -.~ [ {::~ [: {: ]
prefix=: monad : 'y e. (#>y)&{. &.> words {~ (#words) | (,~ <:) words I. y'
exact=: -:[:{&words words&I.

solve=: monad define
graph=. reify_grid i. 2 # %: # y
board=. y[words=. '' [ curr=. |: ,: i.#y
while. #curr do. ws =. ''[next =. ,: 0,{.curr
  for_j. curr do.
    for_j. graph adj j do. w=. <('Q';'QU') stringreplace j{board
      if. exact w do. ws=. ws,w end.
      if. prefix w do. next=. next,j end.
    end.
  end. curr=. }.next[words=. words,~.ws
end. (/: #&.>) /:~ words
)

demo=: monad define
board=: roll dice4
echo ,. solve board
echo _4]\<"0 board
)
