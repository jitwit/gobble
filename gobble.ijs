words=: 'b'freads<'input/collins.txt'

dice4=: _6]\'NAEAEGEGNWEHCSOAHPLVERDYTOATOWPKAFFSHRVTWEHQUMNIEITSSORLTYETTITSYDLXEDIRTOIMCUBAOBOJNLNHZRENSIEU'

roll=: monad : '(? (#"1 dice4)) {"0 1 dice4'

board=: roll''

moves=: 0 0 -.~ ,/ ,"0/~ i:1
reify_grid=: [:-.&_1&.>[:,[:<"_2[:,"_2/moves|.!._1]
grid0=: reify_grid i. 4 4

adj=: [:>{&grid0
grow=: ,"_ 0]-.~[:adj{:

prefix=: monad : 'y e. (#>y)&{. &.> words {~ (#words) | (,~ <:) words I. y'
exact=: -: [: {&words words&I.

solve=: verb define
words=. '' [ curr=. |: ,: i.#y
while. #curr do.
  next =. ,: 0,{.curr
  ws =. ''
  for_j. curr do.
    for_k. grow j do.
      w=. < k { y
      if. exact w do. ws=. ws,w end.
      if. prefix w do. next=. next,k end.
    end.
  end. curr=. }.next[words=. words,~.ws
end. words
)

demo=: monad define
board=: roll''
echo ,. solve board
echo (_4]\board)
)

NB. from rosetta
anas=: </.~ /:~&>
nletter=: monad : 'words #~ >(y=#)&.>words'

NB. grp=: 1,2([:-.0&{::-:&(/:~)1&{::)\] NB. group anagrams together
NB. grpl=: 1,2([:-.0&{::-:&#1&{::)\]
NB. combos=: [: (grpl <;.1]) [: (/:#&.>) [: (a:-.~grp<;.1]) [: (/:/:~&.>) nletter
