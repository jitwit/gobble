require 'dictionary.ijs stats/bonsai'

shake=: ({~?~@#) {"0 1~ [: ? #"1
dice4=: _6]\'NAEAEGEGNWEHCSOAHPLVERDYTOATOWPKAFFSHRVTWEHQUMNIEITSSORLTYETTITSYDLXEDIRTOIMCUBAOBOJNLNHZRENSIEU'
score=: 0 0 0 1 1 2 3 5 11 {~ 8 <. #
nhood=: <:3 3#:4-.~i.9
graph_of =: [: -.&_1 &.> [: , [: <"_2 [: ,"_2/ nhood |.!._1 ]
sub_Q =: ('Q';'QU')&stringreplace^:('Q'&e.)
letters =: 1 : '(<@sub_Q)"1 @ ({&u)'
expand_path=: 2 : 0
  < (#~ prefix @ (u letters)) y,"_ 0/ y -.~ ({:y) {:: v
)
expand =: 2 : 0
  ([: < [: ; u expand_path v"1 @ >) ^: (0<#@>@]) y
)
boggle =: 3 : 0
  ps =. < ,:"0 i. # b =. , y [ g =. graph_of i. $ y
  ws =. (#~ exact) ; b letters"1 &.> b expand g ^: a: ps
  (/: #&>) /:~ ~. ws
)

NB. shape x, length y
random_walk =: 4 : 0
  assert. (1 <: y) *. y <: */ x
  p =. ? # g =. graph_of i. x
  while. y > #p do.
    v =. p -.~ g {::~ {. p
    if. 0=#v do. p =. ? # g
    else. p =. p,~({~ ?@#)v end.
  end. p
)

NB. x is shape, y is word
board =: 4 : 0
  s =. (*/x) # ' '
  pth =. x random_walk #y
  out =. (i.$s) -. pth
  ltr =. (#out) {. shake dice4
  x $ (y,ltr) (pth,out)}s
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
