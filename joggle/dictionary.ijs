COLLINS=: jpath '~/code/gobble/cobble/share/definitions.txt'
N=: # WORDS=: {."1  DICTIONARY=: ([: <;._1 (9{a.)&,);._2 (1!:1) < COLLINS
define=: DICTIONARY {~ WORDS I. <@toupper
words=: /:~ @: (DICTIONARY {~ (?&N))
isprefix=: 3 : 'y e.(#>y)&{.&.>WORDS{~N|(,~<:)WORDS I. y'
prefix=: isprefix@<@toupper
