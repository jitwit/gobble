require'~/code/gobble/joggle/gobble.ijs'
coinsert'jgl2 gobble'

joggle_form=: noun define
pc joggle closeok; pn "Joggle"; minwh 500 500;
bin h;
  cc brd isidraw; set brd wh 256 256;
  bin v;
    cc typ edit; set typ wh 100 30;
    cc wds table;
      set wds wh 100 225;
      set wds shape 0 1;
      set wds hdr "words";
      set wds protect 1;
  bin z;
bin z;
pshow; psel joggle; ptop;
)

joggle_close=: monad define
wd'psel joggle;pclose;'
)

update_words=: monad define
wd'psel joggle'
wd'set wds shape ',(":#LIST),' 1'
for_w. LIST do.
  wd'set wds block ',(":w_index),' 0'
  wd'set wds data ',>w
end.
score_list''
echo SCORE
)

NB. type box, submitting words
joggle_typ_button=: monad define
wd'psel joggle'
LIST=: ~.LIST,~<' '-.~toupper wd 'get typ text'
wd'set typ text ""'
update_words''
)

joggle_wds_mblup=: monad define NB. smoutput wdq
wd'psel joggle'
ix=: {.".wds_select
LIST=: (ix{.LIST) , (>:ix)}.LIST
update_words''
)

NB. draw the current board
draw_board=: monad define
wd'psel joggle'
glsel 'brd'
glclear''
glfont '"lucida console" 25'
for_c. BOARD do.
  xy=. (4&|)`([:<.%&4)`:0 c_index
  gltextxy 8 15 -~ (SIZE%8) + 4 %~ SIZE * xy
  gltext c
end.
glpaint''
)

score_list=: monad define
ixs=. <: +: LIST e. SOLS
scores=. > score_word &.> LIST
SCORE=: ixs +/ .* scores
)

init=: monad define
SIZE=: 256
BOARD=: ''
SOLS=: ''
LIST=: ''
SCORE=: 0
fresh_board''
)

fresh_board=: monad define
SOLS=: gobble]BOARD=: roll dice4
draw_board''
)

mush=: verb define
if. IFQT do.
  wd joggle_form[joggle_close^:(wdisparent'joggle')''
  init''
else.
  echo 'no qt'
end.
)

mush''