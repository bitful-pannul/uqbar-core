/-  *zere-test
/+  *zink-zink
=<  =|  test-args
    =*  args  -
    |%
    ++  mk
      |_  cax=cache
      +*  mk-core  .
      ++  test
        ^-  [test-hints _mk-core]
        =^  hint  cax  (gen-test-hints args(cax (~(uni by cax) cax.args)))
        :_  mk-core(args *test-args)
        %&^hint
      ::  
      ++  tests
        |=  tests=(list [name=@tas args=[test-hints _mk-core]])
        =|  =test-fils
        |-  ^-  [test-hints _mk-core]
        ?~  tests  [%|^test-fils]^mk-core
        =^  hints  mk-core  args.i.tests
        %_    $
            tests     t.tests
            mk-core   mk-core(args *test-args)
            test-fils
          ?:  ?=(%& -.hints)
            [/[name.i.tests]/json p.hints]^test-fils
          %+  weld  test-fils
          (turn p.hints |=(test-fil `test-fil`[[name.i.tests^fil]^jon]))
        ==
      ::
      ++  finish
        |=  [=test-hints cor=_mk-core]
        ^-  test-fils
        ?>  ?=(%| -.test-hints)
        p.test-hints
    --
  --
|%
++  gen-test-hints
  |=  test-args
  |^  ^-  [json cache]
  =-  ~|  %scrys-mismatch
      ?>  =(scrys scrys.q.-)
      :-  (en-hints -)
      cax.q.-
  %.  [s f |]
  %*  .  zink
      app  [cax bud scrys]
  ==
  ::
  ++  en-hints
    |=  =book
    ^-  json
    =/  hs  (hash s cax.q.book)
    =/  hf  (hash f cax.q.book)
    %-  pairs:enjs:format
    :~  hints+(hints:enjs q.p.book)
        subject+(num:enjs hs)
        formula+(num:enjs hf)
        scrys+a+(turn scrys |=(s=* (num:enjs (hash scrys cax.q.book))))
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
    =/  hp=json  ?:(?=(^ res) (num:enjs (hash u.+.res cax.q.book)) ~)
    =/  status=json  ?:(?=(^ res) s+-.res s+res)
    =/  bud=json  ?~(bud.q.book ~ (num:enjs u.bud.q.book))
    %-  pairs:enjs:format
    :~  product+hp
        status+status
        bud+bud
    ==
  --
--