/-  *new-zink
/+  *zink-pedersen, *zink-new-json
=>  |%
    +$  good      (unit *)
    +$  fail      (list [@ta *])
    +$  res       (each good fail)
    +$  body      [p=res q=hints]
    +$  appendix  [cax=cache cas=@ bud=(unit @ud) scrys=(list *)]
    +$  book      (pair body appendix)
    --
|%
++  create-hints
  |=  [h=hints cax=cache]
  ^-  json
  %-  pairs:enjs:format
  :~  nouns+(nouns:enjs cax)
      hints+(hints:enjs h) 
  ==
::
++  zink
  =|  appendix
  =*  app  -
  =|  trace=fail
  |=  [s=* f=*]
  ^-  book
  =-  -(q.p q.p.-)
  |^  ^-  book
  =^  si  app  (cache-noun s)
  =^  fi  app  (cache-noun f)
  ?+    f
    ?@  f  [%|^trace [%invalid %&^f]~]^app
    ?>  ?=(@ -.f)
    [%|^trace [%invalid ~]~]^app
  ::
      [%1 const=*]
    =/  pi  (~(get by cax) const.f)
    [[%& `const.f] [%1 [si fi +>:pi]]~]^app
  ::
      [%2 sub=* for=*]
    =^  [=sub=res =sub=hints]  app
      $(f sub.f)
    ?:  ?=(%| -.sub-res)
      ~&  99  [%|^trace [%2 [si fi 0] sub-hints ~ ~]~]^app
    ?~  p.sub-res  [%&^~ [%2 [si fi 0] sub-hints ~ ~]~]^app
    =^  [=for=res =for=hints]  app
      $(f for.f)
    =/  hit=hints  [%2 [si fi 0] sub-hints for-hints ~]~
    ?:  ?=(%| -.for-res)
      ~&  103  [%|^trace hit]^app
    ?~  p.for-res  [%&^~ hit]^app
    =^  [=prod=res =prod=hints]  app
      $(s u.p.sub-res, f u.p.for-res)
    ?~  p.prod-res  [prod-res [%2 [si fi 0] sub-hints for-hints ~]~]^app
    ?:  =(%| -.prod-res)  [%|^trace [%2 [si fi 0] sub-hints for-hints ~]~]^app
    =/  pi  +>:(~(get by cax) +>.prod-res)
    [prod-res [%2 [si fi pi] sub-hints for-hints prod-hints]~]^app
  ::
      [%3 arg=*]
    :: TODO : out of gas errors
    ::        sf errors (sf itself errors or returns ~)
    =^  [=arg=res =arg=hints]  app
      $(f arg.f)
    ?:  ?=(%| -.arg-res)
      ~&  114  [%|^trace [%3 [si fi 0] ~]~]^app
    ?~  p.arg-res  [%&^~ [%3 [si fi 0] ~]~]^app
    ?@  u.p.arg-res
      =^  pi  app  (cache-noun %.n)
      :_  app
      :-  [%& ~ %.n]
      [%3 [si fi pi] arg-hints]~
    =^  pi  app  (cache-noun %.y)
    :_  app
    :-  [%& ~ %.y]
    [%3 [si fi pi] arg-hints]~
  ::
      [%4 arg=*]
    :: TODO : out of gas errors
    ::        proper sf errors
    ::        error if sf returns a cell, use the hash-req struct e.g. `%cell^[-.u.p.arg-res +.u.p.arg-res]]
    ::        error if sf returns ~ (this is here but not sure if it is correct)
    =^  [=arg=res =arg=hints]  app
      $(f arg.f)
    ?:  ?=(%| -.arg-res)
      ~&  131  [%|^trace [%4 [si fi 0] ~]~]^app
    ?~  p.arg-res  [%&^~ [%4 [si fi 0] arg-hints]~]^app
    ?^  u.p.arg-res
      ~&  135  [%|^trace [%4 [si fi 0] ~]~]^app
    =/  p  .+(u.p.arg-res)
    =^  pi  app  (cache-noun p)
    :_  app
    :-  [%& ~ p]
    [%4 [si fi pi] arg-hints]~
  ::
      [%5 a=* b=*]
    =^  [=a=res =a=hints]  app
      $(f a.f)
    ?:  ?=(%| -.a-res)
      ~&  146  [%|^trace [%5 [si fi 0] ~ ~]~]^app
    ?~  p.a-res  [%&^~ [%5 [si fi 0] ~ ~]~]^app
    =^  [=b=res =b=hints]  app
      $(f b.f)
    ?:  ?=(%| -.b-res)
      ~&  150  [%|^trace [%5 [si fi 0] a-hints b-hints]~]^app
    ?~  p.b-res  [%&^~ [%5 [si fi 0] a-hints b-hints]~]^app
    =/  p   =(u.p.a-res u.p.b-res)
    =^  pi  app  (cache-noun p)
    [[%& ~ =(u.p.a-res u.p.b-res)] [%5 [si fi pi] a-hints b-hints]~]^app
  ::
  ::  6 is special
  ::  if [subject test] returns anything but 0 1, fail
  ::  so we never have to hash yes/no in that case, hence 2
      [%6 test=* yes=* no=*]
    =^  [=t=res =t=hints]  app
      $(f test.f)
    ?:  ?=(%| -.t-res)
      ~&  164  [%|^trace [%6 [si fi 0] t-hints ~]~]^app
    ?~  p.t-res  [%&^~ [%6 [si fi 0] t-hints ~]~]^app
    =^  [=sf2=res =sf2=hints]  app
      ?+  u.p.t-res  `book`[%|^trace ~]^app
        %&  $(f yes.f)
        %|  $(f no.f)
      ==
    ?:  ?=(%| -.sf2-res)
      ~&  164  [%|^trace [%6 [si fi 0] sf2-hints ~]~]^app
    ?~  p.sf2-res  [%&^~ [%6 [si fi 0] sf2-hints ~]~]^app
    =/  pi  (~(get by cax) u.p.sf2-res)
    [[sf2-res [%6 [si fi +>:pi] t-hints sf2-hints]~]]^app
  ::
      [%7 subj=* next=*]
    =^  [=sub=res =sub=hints]  app
      $(f subj.f)
    ?:  ?=(%| -.sub-res)  ~&  179  [%|^trace [%7 [si fi 0] sub-hints ~] ~]^app
    ?~  p.sub-res  [%&^~ [%7 [si fi 0] sub-hints ~] ~]^app
    =^  subi  app  (cache-noun u.p.sub-res)
    =^  [=nex=res =nex=hints]  app
      $(s u.p.sub-res, f next.f)
    ?~  p.nex-res  [nex-res [%7 [si fi 0] sub-hints nex-hints]~]^app
    ?:  =(%| -.nex-res)  [%|^trace [%7 [si fi 0] sub-hints ~]~]^app
    =/  pi  +>:(~(get by cax) +>.nex-res)
    [nex-res [%7 [si fi pi] sub-hints nex-hints]~]^app
  ::
  ==
  ++  cache-noun
    |=  [n=*]
    ^-  [index appendix]
    =/  hit  (~(get by cax) n)
    ?^  hit  [q.u.hit app]
    ?@  n
      =/  ni  +(cas.app)
      :-  ni
      app(cax (~(put by cax) n [atom+n ni]), cas ni)
    =^  hi  app  $(n -.n)
    =^  ti  app  $(n +.n)
    =/  ni  +(cas.app)
    :-  ni
    app(cax (~(put by cax) n [cell+[hi ti] ni]), cas ni)
  --
--
