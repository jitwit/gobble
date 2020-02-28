words=: 'b'freads<'input/collins.txt'

a2z=: a.{~65+i.26

letters=: ' '-.~,>words

freqs=: +/"1 a2z=/letters

freq_chars=: \:&freqs a2z ;"0 (%+/) freqs

NB. 
anas=: </.~ /:~&>
nletter=: monad : 'words #~ >(y=#)&.>words'

grp=: 1,2([:-.0&{::-:&(/:~)1&{::)\] NB. group anagrams together
grpl=: 1,2([:-.0&{::-:&#1&{::)\]
combos=: [: (grpl <;.1]) [: (/:#&.>) [: (a:-.~grp<;.1]) [: (/:/:~&.>) nletter
