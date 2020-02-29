coclass 'gobble'

N=:#]WORDS=: 'b'freads<'~/code/gobble/input/collins.txt' NB. DICT=: '@'readdsv<'~/code/gobble/input/clns.txt'
dice4=: _6]\'NAEAEGEGNWEHCSOAHPLVERDYTOATOWPKAFFSHRVTWEHQUMNIEITSSORLTYETTITSYDLXEDIRTOIMCUBAOBOJNLNHZRENSIEU'
roll=: ({~?~@#) {"0 1~ [: ? #"1

reify_grid=: [:-.&_1&.>[:,[:<"_2[:,"_2/(0 0-.~,/,"0/~i:1)|.!._1]
adj=: ],"_ 0]-.~[{::~[:{:]
prefix=: monad : 'y e.(#>y)&{.&.>WORDS{~N|(,~<:)WORDS I. y'
exact=: -:[:{&WORDS WORDS&I.

gobble=: monad define
graph=. reify_grid i. 2 # %: # ]board=. y[words=. ''[curr=. |:,:i.#y
while. #curr do. ws =. ''[next =. ,: 0,{.curr
  for_j. curr do.
    for_j. graph adj j do. w=. <('Q';'QU') stringreplace j{board
      if. exact w do. ws=. ws,w end.
      if. prefix w do. next=. next,j end.
    end.
  end. curr=. }.next[words=. words,~.ws
end. (/: #&.>) /:~ words
)