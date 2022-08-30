/-  *zere-test
/+  *zink-zink
=<  =|  test-args
    =*  args  -
    |%
    ++  mk  :: if we want to preserve cache, we probably want to fire at the end, actually
      |_  cax=cache
      +*  mk-core  .
      ++  test
        ^-  [json _mk-core]
        =^  json  cax  (gen-test-hints args(cax (~(uni by cax) cax.args)))
        :_  mk-core(args *test-args) :: todo: preserve cache
        json
      ::  
      ++  tests
        |=  tests=(list [name=@tas args=[json _mk-core]])
        =|  jons=(list json)
        |-  ^-  [json _mk-core]
        ?~  tests  [%a jons]^mk-core
        =^  hints  mk-core  args.i.tests
        %_    $
            tests     t.tests
            mk-core   mk-core(args *test-args) :: todo: preserve cache
            jons
          [%a [s+name.i.tests hints ~]]^jons
        ==
      ::
      ++  finish
        |=  [=json cor=_mk-core]
        json
      ::
      --
    ::
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