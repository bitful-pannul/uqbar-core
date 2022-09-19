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
  |=  [s=* f=* test-mode=?]
  ^-  book
  =-  -(q.p q.p.-)
  |^  ^-  book
  ?+    f
    ?@  f  [%|^trace [%invalid %&^f]~]^app
    ?>  ?=(@ -.f)
    [%|^trace [%invalid ~]~]^app
  ::
      [%1 const=*]
    =^  si  app  (cache-noun s)
    =^  fi  app  (cache-noun f)
    [[%& `const.f] [%1 [s f const.f]]~]^app
  ::
      [%3 arg=*]
    :: TODO : out of gas errors
    ::        sf errors (sf itself errors or returns ~)
    =^  si  app  (cache-noun s)
    =^  fi  app  (cache-noun f)
    =^  [=arg=res =arg=hints]  app
      $(f arg.f)
    ?:  ?=(%| -.arg-res)
      ~&  114  [%|^trace [%3 [s f ~] ~]~]^app
    ?~  p.arg-res  [%&^~ [%3 [s f ~] ~]~]^app
    ?@  u.p.arg-res
      =^  ni  app  (cache-noun %.n)
      :_  app
      :-  [%& ~ %.n]
      [%3 [s f %.n] arg-hints]~
    =^  yi  app  (cache-noun %.y)
    :_  app
    :-  [%& ~ %.y]
    [%3 [s f %.y] arg-hints]~
  ::
      [%4 arg=*]
    :: TODO : out of gas errors
    ::        proper sf errors
    ::        error if sf returns a cell, use the hash-req struct e.g. `%cell^[-.u.p.arg-res +.u.p.arg-res]]
    ::        error if sf returns ~ (this is here but not sure if it is correct)
    =^  si  app  (cache-noun s)
    =^  fi  app  (cache-noun f)
    =^  [=arg=res =arg=hints]  app
      $(f arg.f)
    ?:  ?=(%| -.arg-res)
      ~&  131  [%|^trace [%4 [s f ~] ~]~]^app
    ?~  p.arg-res  [%&^~ [%4 [s f ~] arg-hints]~]^app
    ?^  u.p.arg-res
      ~&  135  [%|^trace [%4 [s f ~] ~]~]^app
    =/  inc  .+(u.p.arg-res)
    =^  ni  app  (cache-noun inc)
    :_  app
    :-  [%& ~ inc]
    [%4 [s f inc] arg-hints]~
  ::
  ==
  ++  cache-noun :: TODO this should probably just return appendix
    |=  [n=*]
    ^-  [index appendix]
    =/  inc  (~(get by cax) n)
    ?^  inc  [+.u.inc app]
    ?@  n
      =/  ni  +(cas.app)
      :-  ni
      app(cax (~(put by cax) n [atom+n ni]), cas ni)
    =^  li  app  $(n -.n)
    =^  ri  app  $(n +.n)
    =/  ni  +(cas.app)
    :-  ni
    app(cax (~(put by cax) n [cell+[li ri] ni]), cas ni)
  --
--
