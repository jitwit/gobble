load 'stats/bonsai regex'
coinsert'jregex'

benchfile=: '../report/bench.txt'
NB. ignores runs that had gc...
cpupat=: '^    [[:digit:]]\.[[:digit:]]+s elapsed cpu time'

run=: 4 : 0
samp=. > ([: ". [: > [: {. [: <;._1 's'&,) &.> cpupat rxall 1!:1 < benchfile
'dot' plot ^: x samp
bs_summarize samp
)

0 run''