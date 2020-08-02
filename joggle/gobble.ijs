init=: 3 : 0
(9!:43) 0
COLLINS=: jpath '~/code/gobble/cobble/share/definitions.txt'
N=: # WORDS=: {."1  DICTIONARY=: ([: <;._1 (9{a.)&,);._2 (1!:1) < COLLINS
PREFX=: /:~ ~. ; <\ &.> WORDS
NB. look up definitions, prefixes, exact matches
define=: DICTIONARY {~ WORDS I. <@toupper
prefix=: prefix_in&PREFX
exact=: -: [: {&WORDS WORDS&I.
'ok'

prefix_in=: [ = ] {~ ] I. [

NB. shake/roll some boggle dice
shake=: ({~?~@#) {"0 1~ [: ? #"1
NB. the standard boggle dice 
dice4=: _6]\'NAEAEGEGNWEHCSOAHPLVERDYTOATOWPKAFFSHRVTWEHQUMNIEITSSORLTYETTITSYDLXEDIRTOIMCUBAOBOJNLNHZRENSIEU'

score=: 0 0 0 1 1 2 3 5 11{~8<.# NB. score: index by word length
view_board=: -@%:@#<"0\]

NB. take i. x y board and list graph as array with box
reify_grid=: [:-.&_1&.>[:,[:<"_2[:,"_2/(0 0-.~,/,"0/~i:1)|.!._1]
NB. boggle graph
BOG =: reify_grid i. 4 4

sub_Q =: ('Q';'QU')&stringreplace^:('Q'&e.)
NB. adverb to turn indices into words, substituting Q for Qu
letters =: 1 : '(<@sub_Q)"1 @ ({&u)'

NB. x is board, y is single path expand based on board graph, dropping
NB. squares that have been used or are not valid prefixes
expand_path=: 4 : '(#~ prefix @ (x letters)) y,"_ 0/ y -.~ ({:y) {:: BOG'
expand =: ([: < [: ; [ <@expand_path"_ 1 >@])^: (0 < (*/) @ $@>@])

NB. solve a boggle board by starting with a path at each square and
NB. continually expanding valid paths until no further expansion is
NB. possible. Finally, copy the exact matches and present after
NB. sorting.
gobble =: 3 : 0
  matches =. ; (y letters)"1 &.> y & expand ^: a: < ,:"0 i. # y
  (/: #&>) /:~ ~. (#~ exact"0) matches
)

init''
