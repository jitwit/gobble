load'stats/bonsai regex'
coinsert'jregex'

benchfile=. '../report/bench.txt'
cpupat=. '^    [[:digit:]]\.[[:digit:]]+s elapsed cpu time$'

run=: 3 : 0
'dot' plot samp=. > ([: ". [: > [: {. [: <;._1 's'&,) &.> cpupat rxall 1!:1 < benchfile
({:@regress_bench) bsbca samp

)

run''