/-  *new-zink
/+  *zink-pedersen :: , *zink-json
=>  |%
    +$  good      (unit *)
    +$  fail      (list [@ta *])
    +$  res       (each good fail)
    +$  body      [p=res q=hints]
    +$  appendix  [cax=cache bud=(unit @ud) scrys=(list *)]
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
      :_  app
      :-  [%& ~ %.n]
      [%3 [s f %.n] arg-hints]~
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
    :_  app
    :-  [%& ~ .+(u.p.arg-res)]
    [%4 [s f .+(u.p.arg-res)] arg-hints]~
  ::
  ==
  --
--
