COLLINS=: jpath '~/code/gobble/cobble/share/definitions.txt'
WORDS=: {."1  DICTIONARY=: ([: <;._1 (9{a.)&,);._2 (1!:1) < COLLINS
define=: DICTIONARY {~ WORDS I. <@toupper
words=: /:~ @: (DICTIONARY {~ (?&(#WORDS)))