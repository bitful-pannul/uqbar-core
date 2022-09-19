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
  :: hints+(hints:enjs h) 
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
    =/  pi  +:(~(get by cax) const.f)
    [[%& `const.f] [%1 [si fi pi]]~]^app
  ::
      [%3 arg=*]
    :: TODO : out of gas errors
    ::        sf errors (sf itself errors or returns ~)
    =^  [=arg=res =arg=hints]  app
      $(f arg.f)
    ?:  ?=(%| -.arg-res)
      ~&  114  [%|^trace [%3 [si fi ~] ~]~]^app
    ?~  p.arg-res  [%&^~ [%3 [si fi ~] ~]~]^app
    ?@  u.p.arg-res
      =^  pi  app  (cache-noun %.n)
      :_  app
      :-  [%& ~ %.n]
      [%3 [s f pi] arg-hints]~
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
      ~&  131  [%|^trace [%4 [si fi ~] ~]~]^app
    ?~  p.arg-res  [%&^~ [%4 [si fi ~] arg-hints]~]^app
    ?^  u.p.arg-res
      ~&  135  [%|^trace [%4 [si fi ~] ~]~]^app
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
      ~&  146  [%|^trace [%5 [si fi ~] ~ ~]~]^app
    ?~  p.a-res  [%&^~ [%5 [si fi ~] ~ ~]~]^app
    =^  [=b=res =b=hints]  app
      $(f b.f)
    ?:  ?=(%| -.b-res)
      ~&  150  [%|^trace [%5 [si fi ~] a-hints b-hints]~]^app
    ?~  p.b-res  [%&^~ [%5 [si fi ~] a-hints b-hints]~]^app
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
      ~&  164  [%|^trace [%6 [si fi ~] t-hints ~]~]^app
    ?~  p.t-res  [%&^~ [%6 [si fi ~] t-hints ~]~]^app
    =^  [=sf2=res =sf2=hints]  app
      ?+  u.p.t-res  `book`[%|^trace ~]^app
        %&  $(f yes.f)
        %|  $(f no.f)
      ==
    ?:  ?=(%| -.sf2-res)
      ~&  164  [%|^trace [%6 [si fi ~] sf2-hints ~]~]^app
    ?~  p.sf2-res  [%&^~ [%6 [si fi ~] sf2-hints ~]~]^app
    [[sf2-res [%6 [s f u.p.sf2-res] t-hints sf2-hints]~]]^app
  ::
      [%7 subj=* next=*]
    =^  [=sub=res =sub=hints]  app
      $(f subj.f)
    ?:  ?=(%| -.sub-res)  ~&  179  [%|^trace [%7 [si fi ~] sub-hints ~] ~]^app
    ?~  p.sub-res  [%&^~ [%7 [si fi ~] sub-hints ~] ~]^app
    =^  subi  app  (cache-noun u.p.sub-res)
    =^  [=nex=res =nex=hints]  app
      $(s u.p.sub-res, f next.f)
    ?~  p.nex-res  [nex-res [%7 [si fi ~] sub-hints nex-hints]~]^app
    ?:  =(%| -.nex-res)  [%|^trace [%7 [si fi ~] sub-hints ~]~]^app
    [nex-res [%7 [si fi +:(~(get by cax) +>.nex-res)] sub-hints nex-hints]~]^app
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
