board=: 'CRDETTELOIIINYAT'

moves=: 0 0 -.~ ,/ ,"0/~ i:1
reify_grid=: [:-.&_1&.>[:,[:<"_2[:,"_2/moves|.!._1]
grid0=: reify_grid i. 4 4
adj=: [: > {&grid0
grow=: ,"_ 0]-.~[:adj{:
lookup=: [: < {&board

prefix=: verb define
y e. (#>y)&{. &.> words {~ (#words) | (,~ <:) words I. y
)

exact=: -: [: {&words words&I.

solve=: verb define
ws=. ''
curr=. |: ,: i.#y
while. #curr do.
  next =. ,: 0,{.curr
  for_j. curr do.
    for_k. grow j do.
      w=. < k { y
      if. exact w do. ws=. w,ws end.
      if. prefix w do. next=. next,k end.
    end.
  end. curr =. }. next
end. ~. ws
)

del2s=: #~ ([: > ([:-. 2=#) &.>)