/-  *zere-test
/+  *zink-zink
/*  smart-lib-noun  %noun  /lib/zig/compiled/smart-lib/noun
/*  zink-cax-noun   %noun  /lib/zig/compiled/hash-cache/noun
=/  smart=vase  ;;(vase (cue q.q.smart-lib-noun))
=/  smart-cax=cache:zink  ~  ::  ;;(cache:zink (cue q:q:;;((pair * (pair * @)) zink-cax-noun)))
=<
  =|  test-args
  =*  args  -
  |%
  ++  test
    ^-  named-test
    |=  [cax=cache name=(unit @t)]
    ^-  [json cache]
    ~?  ?=(^ name)  `@t`(rap 3 '/' u.name ~)
    (gen-test-hints cax args)
  ::
  ++  mint
    |=  txt=@
    test:+>(s q.smart, f q:(~(mint ut p.smart) %noun (ream txt)))
  ::  
  ++  tests
    |=  tests=(list [name=@tas test=named-test])
    ^-  named-test
    |=  [cax=cache name=(unit @t)]
    =|  jons=(list json)
    |-  ^-  [json cache]
    ?~  tests  [%a jons]^cax
    =^  hints  cax
      %+  test.i.tests  cax
      ?~  name  `name.i.tests
      `(rap 3 u.name '/' name.i.tests ~)
    %_    $
        tests     t.tests
        args  *test-args
        jons
      [%a [s+name.i.tests hints ~]]^jons
    ==
  ::
  ++  run
    |=  test=named-test
    ^-  json
    -:(test smart-cax ~)
  ::
  --
|%
++  gen-test-hints
  =/  scry-type  -:!>(*granary-scry)
  |=  [cax=cache test-args]
  |^  ^-  [json cache]
  =;  =book
      =/  res  p.p.book
      =>  ?.(=(scrys scrys.q.book) ~|(%scrys-mismatch !!) .)
      =/  out  (en-hints book)^cax.q.book
      ~!  out
      ~!  cax.q.book
      ?:  &(?=(%& -.res) ?=(~ p.res))  out
      =/  mres  (rock [s f] scrys)
      ~|  %real-result-mismatch
      ?>  ?|  &(?=([%& ~ *] res) =([%0 u.p.res] mres))
              &(?=(%| -.res) |(=(%1 mres) =(%2 mres)))
          ==
      out
  %.  [s f |]
  %*  .  zink
      app  [cax ~ bud scrys]
  ==
  ::
  ++  crash-scry
    ^-  granary-scry
    |=  ^
    ^-  (unit (unit))
    !!
  ::
  ++  rock
    |=  [[s=* f=*] scrys=(list *)]
    %+  mink  [s f]
    %+  roll  scrys
    |:  [inner=** outer=crash-scry]
    |=  ^
    =-  ?:(?=(%0 -<) ``product.- ~)
    %+  mink  [inner [9 2 10 [6 1 +<] 0 1]]
    !<(granary-scry [scry-type s])
  ::
  ++  en-hints
    |=  =book
    ^-  json
    %-  pairs:enjs:format
    :~  book+(book:enjs book)
        scrys+a+~ :: TODO
        bud+?~(bud ~ (num:enjs u.bud))
        result+(en-result book)
    ==
  ::
  ++  en-result
    |=  =book
    ^-  json
    =/  res
      ?:  ?=(%| -.p.p.book)  %crash
      ?:  ?=([%& ~] p.p.book)  %out-of-gas
      %success^p.p.p.book
    =/  status=json  ?:(?=(^ res) s+-.res s+res)
    =/  bud=json  ?~(bud.q.book ~ (num:enjs u.bud.q.book))
    %-  pairs:enjs:format
    :~  status+status
        bud+bud
    ==
  --
--
