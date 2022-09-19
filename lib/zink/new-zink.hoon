/-  *new-zink
/+  *zink-pedersen :: , *zink-json
=>  |%
    +$  good      (unit *)
    +$  fail      (list [@ta *])
    +$  res       (each good fail)
    +$  body      [p=res q=hints]
    +$  appendix  [cax=cache cas=@ bud=(unit @ud) scrys=(list *)]
    +$  book      (pair body appendix)
    --
|%
++  zink
  =|  appendix
  =*  app  -
  =|  trace=fail
  |=  [s=* f=*]
  ^-  book
  =-  -(q.p q.p.-)
  |^  ^-  book
  =.  app  (cache-noun s)
  =.  app  (cache-noun f)
  ?+    f
    ?@  f  [%|^trace [%invalid %&^f]~]^app
    ?>  ?=(@ -.f)
    [%|^trace [%invalid ~]~]^app
  ::
      [%1 const=*]
    [[%& `const.f] [%1 [s f const.f]]~]^app
  ::
      [%3 arg=*]
    :: TODO : out of gas errors
    ::        sf errors (sf itself errors or returns ~)
    =^  [=arg=res =arg=hints]  app
      $(f arg.f)
    ?:  ?=(%| -.arg-res)
      ~&  114  [%|^trace [%3 [s f ~] ~]~]^app
    ?~  p.arg-res  [%&^~ [%3 [s f ~] ~]~]^app
    ?@  u.p.arg-res
      =.  app  (cache-noun %.n)
      :_  app
      :-  [%& ~ %.n]
      [%3 [s f %.n] arg-hints]~
    =.  app  (cache-noun %.y)
    :_  app
    :-  [%& ~ %.y]
    [%3 [s f %.y] arg-hints]~
  ::
      [%4 arg=*]
    :: TODO : out of gas errors
    ::        proper sf errors
    ::        error if sf returns a cell, use the hash-req struct e.g. `%cell^[-.u.p.arg-res +.u.p.arg-res]]
    ::        error if sf returns ~ (this is here but not sure if it is correct)
    =^  [=arg=res =arg=hints]  app
      $(f arg.f)
    ?:  ?=(%| -.arg-res)
      ~&  131  [%|^trace [%4 [s f ~] ~]~]^app
    ?~  p.arg-res  [%&^~ [%4 [s f ~] arg-hints]~]^app
    ?^  u.p.arg-res
      ~&  135  [%|^trace [%4 [s f ~] ~]~]^app
    =/  p  .+(u.p.arg-res)
    =.  app  (cache-noun p)
    :_  app
    :-  [%& ~ p]
    [%4 [s f p] arg-hints]~
  ::
      [%5 a=* b=*]
    =^  [=a=res =a=hints]  app
      $(f a.f)
    ?:  ?=(%| -.a-res)
      ~&  146  [%|^trace [%5 [s f ~] ~ ~]~]^app
    ?~  p.a-res  [%&^~ [%5 [s f ~] ~ ~]~]^app
    =^  [=b=res =b=hints]  app
      $(f b.f)
    ?:  ?=(%| -.b-res)
      ~&  150  [%|^trace [%5 [s f ~] a-hints b-hints]~]^app
    ?~  p.b-res  [%&^~ [%5 [s f ~] a-hints b-hints]~]^app
    =/  p   =(u.p.a-res u.p.b-res)
    =.  app  (cache-noun p)
    [[%& ~ =(u.p.a-res u.p.b-res)] [%5 [s f p] a-hints b-hints]~]^app
  ::
  ==
  ++  cache-noun
    |=  [n=*]
    ^-  appendix :: returning [index appendix] makes it slightly more efficient but slightly more ugly...I think you can fix with a |^
    ?^  (~(get by cax) n)  app
    ?@  n
      =/  ni  +(cas.app)
      app(cax (~(put by cax) n [atom+n ni]), cas ni)
    =.  app  $(n -.n)
    =.  app  $(n +.n)
    =/  li  (~(get by cax.app) -.n) :: why do I have to use +> instead of q.u.li?
    =/  ri  (~(get by cax.app) +.n)
    =/  ni  +(cas.app)
    app(cax (~(put by cax) n [cell+[+>.li +>.ri] ni]), cas ni)
  --
--