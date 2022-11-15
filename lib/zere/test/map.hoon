/+  mk=zere-test-gen
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
%-  tests
:~  
    :-  'has'
    %-  tests
    :~  
        has-pby+(mint (crip "(~(has pby `(pmap)`{<test-map>}) 7)"))
        has-pby-head+(mint (crip "(~(has pby `(pmap)`{<test-map>}) 10)"))
        not-has-pby-branch-right+(mint (crip "(~(has pby `(pmap)`{<test-map>}) 32)")) :: FAILING for some unknown reason...
        not-has-pby-branch-left+(mint (crip "(~(has pby `(pmap)`{<test-map>}) 39)"))
        not-has-pby-head-branch-left+(mint '(~(has pby `(pmap)`[[34 34] ~ ~]) 65)')
        not-has-pby-head-branch-right+(mint '(~(has pby `(pmap)`[[34 34] ~ ~]) 10)')
    ==
    :-  'get'
    %-  tests
    :~  get-pby+(mint (crip "(~(get pby `(pmap)`{<test-map>}) 7)"))
        get-pby-head+(mint (crip "(~(get pby `(pmap)`{<test-map>}) 10)"))
        not-get-pby-branch-right+(mint (crip "(~(get pby `(pmap)`{<test-map>}) 32)")) :: FAILING for some unknown reason...
        not-get-pby-branch-left+(mint (crip "(~(get pby `(pmap)`{<test-map>}) 39)"))
        not-get-pby-head-branch-left+(mint '(~(get pby `(pmap)`[[34 34] ~ ~]) 65)')
        not-get-pby-head-branch-right+(mint '(~(get pby `(pmap)`[[34 34] ~ ~]) 10)')
    ==
    :-  'put'
    %-  tests
    :~  put-pby+(mint (crip "(~(put pby `(pmap)`{<test-map>}) 20 111)"))
        put-pby-exists+(mint (crip "(~(put pby `(pmap)`{<test-map>}) 11 108)"))
        put-pby-exists-head+(mint (crip "(~(put pby `(pmap)`{<test-map>}) 10 105)"))
    ==
    :-  'tap'
    %-  tests
    :~  tap-pby+(mint (crip "~(tap pby `(pmap)`{<test-map>})"))
        tap-pby-empty+(mint (crip "~(tap pby `(pmap)`~)"))
        tap-pby-one+(mint (crip "~(tap pby `(pmap)`[n=[10 1] ~ ~])"))
        tap-pby-two-left+(mint (crip "~(tap pby `(pmap)`[n=[10 2] l=[n=[3 4] ~ ~] ~])"))
        tap-pby-two-right+(mint (crip "~(tap pby `(pmap)`[n=[10 3] ~ r=[n=[5 7] ~ ~]])"))
        tap-pby-three-right+(mint (crip "~(tap pby `(pmap)`[n=[10 4] l=[n=[3 5] ~ ~] r=[n=[5 6] ~ ~]])"))
    ==
    :-  'apt'
    %-  tests
    :~  apt-balanced-a+(mint (crip "~(apt pby `(pmap)`{<balanced-a>})"))
        apt-unbalanced-a+(mint (crip "~(apt pby `(pmap)`{<unbalanced-a>})"))
        apt-unbalanced-b+(mint (crip "~(apt pby `(pmap)`{<unbalanced-b>})"))
        apt-unbalanced-c+(mint (crip "~(apt pby `(pmap)`{<unbalanced-c>})"))
        apt-unbalanced-d+(mint (crip "~(apt pby `(pmap)`{<unbalanced-d>})"))
        apt-unbalanced-e+(mint (crip "~(apt pby `(pmap)`{<unbalanced-e>})"))
    ==
==
