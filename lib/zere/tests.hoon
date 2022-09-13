/+  *zere-test-gen, set-tests=zere-test-set, map-tests=zere-test-map
::
|%
++  dec  [[6 [5 [0 6] 1 0] [0 0] 7 [[0 95] [1 0] 0 6] 6 [5 [0 7] 4 0 6] [0 6] 9 2 10 [6 4 0 6] 0 1] 0 0]
++  dec-jet  [[11 [500.151.969.402 [1 0 6.514.020] 0 6] 7 [10 [2 0 47] 0 1] 6 [5 [0 6] 1 0] [0 0] 7 [[0 95] [1 0] 0 6] 6 [5 [0 7] 4 0 6] [0 6] 9 2 10 [6 4 0 6] 0 1] 0 0]
:: ++  test-set  [n=11 l=[8 l=~ r=~] r=[n=12 l=[n=6 l=[n=10 [n=7 l=[n=5 l=~ r=~] r=~] r=~] r=~] r=[n=4 l=[n=2 l=[n=9 l=~ r=[n=1 ~ ~]] r=[n=3 l=~ r=~]] r=~]]]
++  test-set  [n=10 l=[n=3 l=[n=4 [n=12 l=~ r=~] r=~] r=[n=2 l=[n=1 l=~ r=[n=11 l=~ r=~]] r=[n=7 l=~ r=~]]] r=[n=5 l=[n=6 l=~ r=[n=8 l=~ r=~]] r=[n=9 l=~ r=~]]]
--
^-  json
:: ~&  (crip "(~(has pin {<test-set>}) 5)")  ~
%-  finish:mk
%-  tests:mk
:~  :-  'dec-gates'
    %-  tests:mk
    :~  dec+test:mk(s dec, f [9 2 10 [6 1 5] 0 1])
        dec-jet+test:mk(s dec-jet, f [9 2 10 [6 1 5] 0 1])
    ==
    :-  'jets'
    %-  tests:mk
    :~  dec+(mint:mk '(dec 5)')
        add+(mint:mk '(add 5 32)')
        turn+(mint:mk '(turn ~[1 2 3] |=(a=@ +(a)))')
        roll+(mint:mk '(roll `(list @)`~[1 2 3] add)')
        reel+(mint:mk '(reel `(list @)`~[1 2 3] add)')
        set+set-tests
        map+map-tests
    ==
==