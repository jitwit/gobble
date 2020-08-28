require 'dictionary.ijs'

defn =: 3 : 0 :: ''
'a def' =. define y =. toupper y
assert. a -: y
('''';' ') stringreplace ^: ('''' e. def) def
)

letter =: , 65 97 +/ i. 26
brack =: 0 = +/\ @ e.&'[]'  NB. drop bracketted extras
token =: letter e.~ a.&i.   NB. find lower case words (the definition)
allgood =: *./ @ token
edges =: (#~ allgood &>) @ ;: @ (#~ brack) @ defn
step =: [: ~. ] , [: ; edges&.>
index =: WORDS&I.
