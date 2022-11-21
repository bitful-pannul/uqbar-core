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
    |=  [=app name=(unit @t)]
    ^-  [json ^app]
    ~?  ?=(^ name)  `@t`(rap 3 '/' u.name ~)
    (gen-test-hints app args)
  ::
  ++  mint
    |=  txt=@
    test:+>(s q.smart, f q:(~(mint ut p.smart) %noun (ream txt)))
  ::  
  ++  tests
    |=  tests=(list [name=@tas test=named-test])
    ^-  named-test
    |=  [=app name=(unit @t)]
    =|  jons=(list json)
    |-  ^-  [json ^app]
    ?~  tests  [%a jons]^app
    =^  hints  app
      %+  test.i.tests  app
      ?~  name  `name.i.tests
      `(rap 3 u.name '/' name.i.tests ~)
    %_    $
        tests     t.tests
        jons
      [%a [s+name.i.tests hints ~]]^jons
    ==
  ::
  ++  run
    |=  test=named-test
    ^-  json
    =/  =app  [~ ~ smart-cax ~]
    =^  tests=json  app  (test app ~)
    =+  (arena:enjs ar.app cax.app)
    %-  pairs:enjs:format
    :~  nindex+nindex
        arena+arena
        tests+tests
        zints+a+(flop hit.app)
    ==
  ::
  --
|%
+$  app  [preds=(map [s=* f=*] *) ar=arena cax=cache hit=(list json)]
+$  named-test  $-([app (unit @t)] [json app])
+$  test-args  [bud=(unit @ud) scrys=(list *) s=* f=* test-mode=_&]
++  gen-test-hints
  =/  scry-type  -:!>(*granary-scry)
  |=  [=app test-args]
  |^  ^-  [json ^app]
  =;  =book
      =+  app
      =.  app
          %_  app
            preds  preds.q.book
            cax  cax.q.book
            ar   arena.q.book
            hit  (zint:enjs q.p.book)^hit.app
          ==
      =/  res  p.p.book
      =>  ?.(=(scrys scrys.q.book) ~|(%scrys-mismatch !!) .)
      =/  out  (en-hints book)^app
      :: todo: add back when jets are on good nock
      ::?:  &(?=(%& -.res) ?=(~ p.res))  out
      :: =/  mres  (rock [s f] scrys)
      :: ~|  %real-result-mismatch
      :: ~|  s=s
      :: ~|  f=f
      :: ~|  p=res
      :: ~|  r=mres
      :: ?>  ?|  &(?=([%& ~ *] res) =([%0 u.p.res] mres))
      ::         &(?=(%| -.res) |(=(%1 mres) =(%2 mres)))
      ::     ==
      out
  %.  [s f |]
  %*  .  zink
      app  [preds.app cax.app ar.app bud scrys]
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
    :~  pred+(pred-from-book book)
        scrys+a+~ :: TODO
        bud+?~(bud ~ (num:enjs u.bud))
        result+(en-result book)
    ==
  ++  pred-from-book
    |=  =book
    ^-  json
    %-  en-pred:enjs
    ?:  ?=(?(%1 %memo) -.q.p.book)  pred.q.p.book
    ?<  ?=(?(%jet %invalid) -.q.p.book)
    pred.q.p.book
 
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
