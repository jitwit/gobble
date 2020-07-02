NB. coclass 'gobble'
init=: 3 : 0
(9!:43) 0
COLLINS=: jpath '~/code/gobble/cobble/share/definitions.txt'
N=: # WORDS=: {."1  DICTIONARY=: ([: <;._1 (9{a.)&,);._2 (1!:1) < COLLINS
define=: DICTIONARY {~ WORDS I. <@toupper
isprefix=: 3 : 'y e.(#>y)&{.&.>WORDS{~N|(,~<:)WORDS I. y'
prefix=: isprefix@<@toupper
exact=: -: [: {&WORDS WORDS&I.
'ok'
)

dice4=: _6]\'NAEAEGEGNWEHCSOAHPLVERDYTOATOWPKAFFSHRVTWEHQUMNIEITSSORLTYETTITSYDLXEDIRTOIMCUBAOBOJNLNHZRENSIEU'
roll=: ({~?~@#) {"0 1~ [: ? #"1
score_word=: 0 0 0 1 1 2 3 5 11{~8<.#
score=: ([:>score_word&.>@[) +/ .* [:<:[:+:e.
view_board=: -@%:@#<"0\]
definition=: [: {: COLN {~ WORDS&I.

reify_grid=: [:-.&_1&.>[:,[:<"_2[:,"_2/(0 0-.~,/,"0/~i:1)|.!._1]
adj=: ],"_ 0]-.~[{::~[:{:]

gobble=: 3 : 0
graph=. reify_grid i. 2 # %: # ]board=. y[words=. ''[curr=. |:,:i.#y
while. #curr do. ws =. ''[next =. ,: 0,{.curr
  for_j. curr do.
    for_j. graph adj j do. w=. <('Q';'QU') stringreplace j{board
      if. exact w    do. ws=. ws,w end.
      if. isprefix w do. next=. next,j end.
    end.
  end. curr=. }.next[words=. words,~.ws
end. (/: #&.>) /:~ words
)

init''