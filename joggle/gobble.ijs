require 'dictionary.ijs stats/bonsai'

shake=: ({~?~@#) {"0 1~ [: ? #"1
dice4=: _6]\'NAEAEGEGNWEHCSOAHPLVERDYTOATOWPKAFFSHRVTWEHQUMNIEITSSORLTYETTITSYDLXEDIRTOIMCUBAOBOJNLNHZRENSIEU'
score=: 0 0 0 1 1 2 3 5 11 {~ 8 <. #
nhood=: (,/ ,"0/~ i:1) -. 0 0
graph_of =: [: -.&_1 &.> [: , [: <"_2 [: ,"_2/ nhood |.!._1 ]
sub_Q =: ('Q';'QU')&stringreplace^:('Q'&e.)
letters =: 1 : '(<@sub_Q)"1 @ ({&u)'
expand_path=: 2 : '< (#~ prefix @ (u letters)) y,"_ 0/ y -.~ ({:y) {:: v'
expand =: 2 : '([: < [: ; u expand_path v"1 @ >) ^: (0<#@>@]) y'
boggle =: 3 : 0
  b =. , y [ g =. graph_of i. $ y
  (/: #&>) /:~ ~. (#~ exact"0) ; b letters"1 &.> b expand g ^: a: < ,:"0 i. # b
)

test =: 3 : 0
assert. 24 = +/ score & > boggle 4 4 $ 'DEMODEMODEMODEMO'
assert. 1254 = # boggle 4 4 $ 'PSLMEAIARNTRGESO'
assert. 189 = # boggle 8 2 $ 'PSLMEAIARNTRGESO'
assert. 318 = # boggle 4 4 $ 'YVUPESTAGOLEOWNV'
assert. 8 = # boggle 2 2 $ 'ABBA'
assert. 25 = # boggle 3 2 $ 'GOOGLE'
'aok'
)

test''
