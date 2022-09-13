/+  *zere-test-gen
|%
++  test-map  [n=[10 10] l=[n=[3 3] l=[n=[4 4] [n=[12 12] l=~ r=~] r=~] r=[n=[2 2] l=[n=[1 1] l=~ r=[n=[11 11] l=~ r=~]] r=[n=[7 7] l=~ r=~]]] r=[n=[5 5] l=[n=[6 6] l=~ r=[n=[8 8] l=~ r=~]] r=[n=[9 9] l=~ r=~]]]
--
%-  tests:mk
:~  :-  'has'
    %-  tests:mk
    :~  
        has-pby+(mint:mk (crip "(~(has pby `(pmap)`{<test-map>}) 7)"))
        has-pby-head+(mint:mk (crip "(~(has pby `(pmap)`{<test-map>}) 10)"))
        not-has-pby-branch-right+(mint:mk (crip "(~(has pby `(pmap)`{<test-map>}) 32)"))
        not-has-pby-branch-left+(mint:mk (crip "(~(has pby `(pmap)`{<test-map>}) 39)"))
        not-has-pby-head-branch-left+(mint:mk '(~(has pby `(pmap)`[[34 34] ~ ~]) 65)')
        not-has-pby-head-branch-right+(mint:mk '(~(has pby `(pmap)`[[34 34] ~ ~]) 10)')
    ==
==
