load 'stats/bonsai regex'
coinsert'jregex'

benchfile=. '../report/bench.txt'
NB. ignores runs that had gc...
cpupat=. '^    [[:digit:]]\.[[:digit:]]+s elapsed cpu time$'

run=: 3 : 0
'dot' plot samp=. > ([: ". [: > [: {. [: <;._1 's'&,) &.> cpupat rxall 1!:1 < benchfile
summarize1 samp
)

run''