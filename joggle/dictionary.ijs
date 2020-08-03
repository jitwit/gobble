COLLINS=: jpath '~/code/gobble/cobble/share/definitions.txt'
N=: # WORDS=: {."1  DICTIONARY=: ([: <;._1 (9{a.)&,);._2 (1!:1) < COLLINS
PREFX=: /:~ ~. ; <\ &.> WORDS
define=: DICTIONARY {~ WORDS I. <@toupper
prefix_in=: [ = ] {~ I.~
prefix=: (prefix_in&PREFX)
exact=: -: [: {&WORDS WORDS&I.
