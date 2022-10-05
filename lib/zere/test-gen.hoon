/-  *zere-test
/+  *zink-zink
/*  smart-lib-noun  %noun  /lib/zig/compiled/smart-lib/noun
/*  zink-cax-noun   %noun  /lib/zig/compiled/hash-cache/noun
=/  smart=vase  ;;(vase (cue q.q.smart-lib-noun))
=/  smart-cax=cache:zink  ~  ::  ;;(cache:zink (cue q:q:;;((pair * (pair * @)) zink-cax-noun)))
=<  =|  test-args
    =*  args  -
    |%
    ++  mk  :: if we want to preserve cache, we probably want to fire at the end, actually
      |_  cax=cache
      +*  mk-core  .
      +$  named-test  $-((unit @t) [json _mk-core]) 
      ++  test
        ^-  named-test
        |=  name=(unit @t)
        ^-  [json _mk-core]
        ~?  ?=(^ name)  `@t`(rap 3 '/' u.name ~)
        =^  json  cax  (gen-test-hints args(cax (~(uni by cax) cax.args)))
        :_  =-  -(cax cax)  mk-core(args *test-args)
        json
      ::
      ++  mint
      |=  txt=@
      test:mk-core(cax smart-cax, s q.smart, f q:(~(mint ut p.smart) %noun (ream txt)))
      ::  
      ++  tests
        |=  tests=(list [name=@tas test=named-test])
        ^-  named-test
        |=  name=(unit @t)
        =|  jons=(list json)
        |-  ^-  [json _mk-core]
        ?~  tests  [%a jons]^mk-core
        =^  hints  mk-core
          %-  test.i.tests
          ?~  name  `name.i.tests
          `(rap 3 u.name '/' name.i.tests ~)
        %_    $
            tests     t.tests
            mk-core   =-  -(cax cax)  mk-core(args *test-args)
            jons
          [%a [s+name.i.tests hints ~]]^jons
        ==
      ::
      ++  finish
        |=  test=named-test
        ^-  json
        -:(test ~)
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
      app  [cax ~ 0 bud scrys]
  ==
  ::
  ++  en-hints
    |=  =book
    ^-  json
    %-  pairs:enjs:format
    :~  hints+(hints:enjs q.p.book)
        nouns+(nouns:enjs arena.q.book) 
    ::    scrys+a+(turn scrys |=(s=* (num:enjs (hash scrys cax.q.book))))
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
