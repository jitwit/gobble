require 'regex'

refpat=: '/tous-les-mots-par-lettre/[^"]+'

lettres=: a. {~ (a. i. 'a')+i.26
url_bas=: 'https://www.liste-de-mots.com'&,
url_de=: 'https://www.liste-de-mots.com/tous-les-mots-par-lettre/'&,

lettre_file=: 'ldm/',],":@[

tele=: 4 : 0
file =. x lettre_file y
((2!:0) ^: (0 = 1!:4 :: 0: < file)) 'wget -O ',file,' ',url_de,y,'/',(":x),'/'
)

lettre_urls=: 3 : 0
file1 =: 1 lettre_file y
1 tele ^: (0 = 1!:4 :: 0: < file1) y
>: i.&.<: # refpat rxall 1!:1 < 1 lettre_file y
)

tele_lettre=: 3 : 0
(lettre_urls y) tele"0 _ y
)
