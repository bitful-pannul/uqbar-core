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
:~  :-  'basic-nock'
    %-  tests:mk
    :~  '7'^test:mk(s 4, f [7 [1 3] 0 1])
        '8'^test:mk(s 4, f [8 [1 3] 0 1])
        '6-cons'^test:mk(s 8, f [[6 [1 0] [1 2] 1 3] 6 [1 1] [1 2] 1 3])
        '10'^test:mk(s [7 8 9], f [10 [6 1 4] 0 1])
        '5-cons'^test:mk(s [[2 3] 4 4], f [[5 [0 4] 0 5] 5 [0 6] 0 7])
        '3-cons'^test:mk(s [4 2 3], f [[3 0 2] 3 0 3])
        '0-1-2-4'^test:mk(s [14 4 0 1], f [2 [0 2] 0 3])
    ==
    :-  'basic-nock-1'
    %-  tests:mk
    :~  '7'^test:mk(s 4, f [7 [1 3] 0 1])
        '8'^test:mk(s 4, f [8 [1 3] 0 1])
        '6-cons'^test:mk(s 8, f [[6 [1 0] [1 2] 1 3] 6 [1 1] [1 2] 1 3])
        '10'^test:mk(s [7 8 9], f [10 [6 1 4] 0 1])
        '5-cons'^test:mk(s [[2 3] 4 4], f [[5 [0 4] 0 5] 5 [0 6] 0 7])
        '3-cons'^test:mk(s [4 2 3], f [[3 0 2] 3 0 3])
        '0-1-2-4'^test:mk(s [14 4 0 1], f [2 [0 2] 0 3])
    ==
    :-  'dec-gates-0'
    %-  tests:mk
    :~  dec2+test:mk(s dec, f [9 2 10 [6 1 2] 0 1])
        dec3+test:mk(s dec, f [9 2 10 [6 1 3] 0 1])
        dec4+test:mk(s dec, f [9 2 10 [6 1 4] 0 1])
        dec5+test:mk(s dec, f [9 2 10 [6 1 5] 0 1])
        ::  dec-jet+test:mk(s dec-jet, f [9 2 10 [6 1 5] 0 1])
    ==
::    :-  'jets'
::    %-  tests:mk
::    :~  dec+(mint:mk '(dec 5)')
::        add+(mint:mk '(add 5 32)')
::        turn+(mint:mk '(turn ~[1 2 3] |=(a=@ +(a)))')
::        roll+(mint:mk '(roll `(list @)`~[1 2 3] add)')
::        reel+(mint:mk '(reel `(list @)`~[1 2 3] add)')
::        set+set-tests
::        map+map-tests
::    ==
==
