/+  zink=zink-zink, *zere-test-gen
/*  smart-lib-noun  %noun  /lib/zig/compiled/smart-lib/noun
/*  zink-cax-noun   %noun  /lib/zig/compiled/hash-cache/noun
=/  smart=vase  ;;(vase (cue q.q.smart-lib-noun))
=/  cax         ;;(cache:zink (cue q:q:;;((pair * (pair * @)) zink-cax-noun)))
::
|%
++  dec  [[6 [5 [0 6] 1 0] [0 0] 7 [[0 95] [1 0] 0 6] 6 [5 [0 7] 4 0 6] [0 6] 9 2 10 [6 4 0 6] 0 1] 0 0]
++  dec-jet  [[11 [500.151.969.402 [1 0 6.514.020] 0 6] 7 [10 [2 0 47] 0 1] 6 [5 [0 6] 1 0] [0 0] 7 [[0 95] [1 0] 0 6] 6 [5 [0 7] 4 0 6] [0 6] 9 2 10 [6 4 0 6] 0 1] 0 0]
++  mint-mk
  |=  txt=@
  test:mk(cax cax, s q.smart, f q:(~(mint ut p.smart) %noun (ream txt)))
--
^-  json
%-  finish:mk
%-  tests:mk
:~  :-  'dec-gates'
    %-  tests:mk
    :~  dec+test:mk(s dec, f [9 2 10 [6 1 5] 0 1])
        dec-jet+test:mk(s dec-jet, f [9 2 10 [6 1 5] 0 1])
    ==
    :-  'jets'
    %-  tests:mk
    :~  dec+(mint-mk '(dec 5)')
        add+(mint-mk '(add 5 32)')
    ==
==
