require'~/code/gobble/joggle/gobble.ijs'
coinsert'jgl2 gobble'

joggle_form=: noun define
pc joggle closeok; pn "Joggle"; minwh 500 500;
bin h;
  cc brd isidraw; set brd wh 256 256;
  bin v;
    cc typ edit; set typ wh 100 30;
  bin z;
bin z;
pshow; psel joggle; ptop;
)

joggle_close=: monad define
wd'psel joggle;pclose;'
)

joggle_typ_button=: monad define
wd'psel joggle'
echo]LIST=: LIST,~<wd 'get typ text'
wd'set typ text ""'
)

SIZE=: 256
BOARD=: ''
LIST=: ''
SOLS=: ''

draw_board=: monad define
wd'psel joggle'
glsel 'brd'
glclear''
NB. lines

NB. the chars
glfont '"lucida console" 25'
for_c. BOARD do.
  xy=. (4&|)`([:<.%&4)`:0 c_index
  gltextxy 8 15 -~ (SIZE%8) + 4 %~ SIZE * xy
  gltext c
end.
glpaint''
)

init=: monad define
LIST=: ''
fresh_board''
)

fresh_board=: monad define
SOLS=: gobble]BOARD=: roll dice4
draw_board''
)

mush=: verb define
if. IFQT do. wd joggle_form[joggle_close^:(wdisparent'joggle')''
             fresh_board''
else. echo 'no qt' end.
)

mush''
