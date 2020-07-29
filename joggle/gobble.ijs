NB. coclass 'gobble'
init=: 3 : 0
(9!:43) 0
COLLINS=: jpath '~/code/gobble/cobble/share/definitions.txt'
N=: # WORDS=: {."1  DICTIONARY=: ([: <;._1 (9{a.)&,);._2 (1!:1) < COLLINS
PREFX=: /:~ ~. ; <\ &.> WORDS
define=: DICTIONARY {~ WORDS I. <@toupper
isprefix=: 3 : 'y e.(#>y)&{.&.>WORDS{~N|(,~<:)WORDS I. y'
prefix=: -: [: {&PREFX PREFX&I.
NB. prefix=: isprefix@<@toupper
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
board_graph =: reify_grid i. 4 4
paths0 =: ,:"0 i. 16
unique=: -: ~.

path_word =: (]`(('Q';'QU')&stringreplace)@.('Q'&e.)) @: {

NB. single. x is board, y is single path
NB. m is board
expand_path=: 4 : 0
  (#~ (prefix@<@(path_word&x))"1) y,"_ 0/ y -.~ ({:y) {:: board_graph
)

expand_paths =: ([: < [: ; [ <@expand_path"_ 1 >@])`]@.(0=(*/)@$@>@])

boggle =: 3 : 0
  candidates =. (y&expand_paths) ^: a: < ,:"0 i. # y
  matches =. (#~ exact"0) ; (<@(path_word&y))"1 &.> candidates
  (/: #&>) /:~ ~. matches
)

gobble=: 3 : 0
graph=. reify_grid i. 2 # %: # ]board=. y[words=. ''[curr=. ,:"0 i.#y
while. #curr do. ws =. ''[next =. ,: 0,{.curr
  for_j. curr do.
    for_j. graph adj j do. w=. < j path_word board
      if. exact w  do. ws=. ws,w end.
      if. prefix w do. next=. next,j end.
    end.
  end. curr=. }.next[words=. words,~.ws
end. (/: #&.>) /:~ words
)

init''