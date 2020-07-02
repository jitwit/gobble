
COLN=: 1!:1 < jpath '~/code/gobble/cobble/share/definitions.txt'
COLN=: ([: <;._1 (9{a.)&,);._2 COLN
WORDS=: {."1 COLN
N=: # WORDS

def=: COLN {~ WORDS I. <

words=: /:~ @: (COLN {~ (?&N))

