/+  *zere-test-gen
|%
++  test-map  [n=[p=10 q=106] l=[n=[p=3 q=99] l=[n=[p=4 q=100] l=[n=[p=12 q=108] l=~ r=~] r=~] r=[n=[p=2 q=98] l=[n=[p=1 q=97] l=~ r=[n=[p=11 q=107] l=~ r=~]] r=[n=[p=7 q=103] l=~ r=~]]] r=[n=[p=5 q=101] l=[n=[p=6 q=102] l=~ r=[n=[p=8 q=104] l=~ r=~]] r=[n=[p=9 q=105] l=~ r=~]]]
++  balanced-a  [[2 100] [[1 101] ~ ~] [[3 102] ~ ~]]
  ::  Doesn't follow vertical ordering
  ::
++  unbalanced-a  [[1 100] [[2 101] ~ ~] [[3 102] ~ ~]]
++  unbalanced-b  [[1 100] ~ [[2 101] ~ ~]]
++  unbalanced-c  [[1 100] [[2 101] ~ ~] ~]
  ::  Doesn't follow horizontal ordering
  ::
++  unbalanced-d  [[2 100] [[3 101] ~ ~] [[1 102] ~ ~]]
  ::  Doesn't follow horizontal & vertical ordering
  ::
++  unbalanced-e  [[1 100] [[3 101] ~ ~] [[2 102] ~ ~]]
--
%-  tests:mk
:~  
    :-  'has'
    %-  tests:mk
    :~  
        has-pby+(mint:mk (crip "(~(has pby `(pmap)`{<test-map>}) 7)"))
        has-pby-head+(mint:mk (crip "(~(has pby `(pmap)`{<test-map>}) 10)"))
        not-has-pby-branch-right+(mint:mk (crip "(~(has pby `(pmap)`{<test-map>}) 32)")) :: FAILING for some unknown reason...
        not-has-pby-branch-left+(mint:mk (crip "(~(has pby `(pmap)`{<test-map>}) 39)"))
        not-has-pby-head-branch-left+(mint:mk '(~(has pby `(pmap)`[[34 34] ~ ~]) 65)')
        not-has-pby-head-branch-right+(mint:mk '(~(has pby `(pmap)`[[34 34] ~ ~]) 10)')
    ==
    :-  'get'
    %-  tests:mk
    :~  get-pby+(mint:mk (crip "(~(get pby `(pmap)`{<test-map>}) 7)"))
        get-pby-head+(mint:mk (crip "(~(get pby `(pmap)`{<test-map>}) 10)"))
        not-get-pby-branch-right+(mint:mk (crip "(~(get pby `(pmap)`{<test-map>}) 32)")) :: FAILING for some unknown reason...
        not-get-pby-branch-left+(mint:mk (crip "(~(get pby `(pmap)`{<test-map>}) 39)"))
        not-get-pby-head-branch-left+(mint:mk '(~(get pby `(pmap)`[[34 34] ~ ~]) 65)')
        not-get-pby-head-branch-right+(mint:mk '(~(get pby `(pmap)`[[34 34] ~ ~]) 10)')
    ==
    :-  'put'
    %-  tests:mk
    :~  put-pby+(mint:mk (crip "(~(put pby `(pmap)`{<test-map>}) 20 111)"))
        put-pby-exists+(mint:mk (crip "(~(put pby `(pmap)`{<test-map>}) 11 108)"))
        put-pby-exists-head+(mint:mk (crip "(~(put pby `(pmap)`{<test-map>}) 10 105)"))
    ==
    :-  'tap'
    %-  tests:mk
    :~  tap-pby+(mint:mk (crip "~(tap pby `(pmap)`{<test-map>})"))
        tap-pby-empty+(mint:mk (crip "~(tap pby `(pmap)`~)"))
        tap-pby-one+(mint:mk (crip "~(tap pby `(pmap)`[n=[10 1] ~ ~])"))
        tap-pby-two-left+(mint:mk (crip "~(tap pby `(pmap)`[n=[10 2] l=[n=[3 4] ~ ~] ~])"))
        tap-pby-two-right+(mint:mk (crip "~(tap pby `(pmap)`[n=[10 3] ~ r=[n=[5 7] ~ ~]])"))
        tap-pby-three-right+(mint:mk (crip "~(tap pby `(pmap)`[n=[10 4] l=[n=[3 5] ~ ~] r=[n=[5 6] ~ ~]])"))
    ==
    :-  'apt'
    %-  tests:mk
    :~  apt-balanced-a+(mint:mk (crip "~(apt pby `(pmap)`{<balanced-a>})"))
        apt-unbalanced-a+(mint:mk (crip "~(apt pby `(pmap)`{<unbalanced-a>})"))
        apt-unbalanced-b+(mint:mk (crip "~(apt pby `(pmap)`{<unbalanced-b>})"))
        apt-unbalanced-c+(mint:mk (crip "~(apt pby `(pmap)`{<unbalanced-c>})"))
        apt-unbalanced-d+(mint:mk (crip "~(apt pby `(pmap)`{<unbalanced-d>})"))
        apt-unbalanced-e+(mint:mk (crip "~(apt pby `(pmap)`{<unbalanced-e>})"))
    ==
==