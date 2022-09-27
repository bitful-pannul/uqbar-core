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
      [^ *]
    =^  [=hed=res =hed=hints]  app
      $(f -.f)
    ?:  ?=(%| -.hed-res)
      ~&  61  [%|^trace [%cons [si fi 0] hed-hints ~]~]^app
    ?~  p.hed-res  [%&^~ [%cons [si fi 0] hed-hints ~]~]^app
    =^  [=tal=res =tal=hints]  app
      $(f +.f)
    ?:  ?=(%| -.tal-res)
      ~&  65  [%|^trace [%cons [si fi 0] hed-hints tal-hints]~]^app
    ?~  p.tal-res  [%&^~ [%cons [si fi 0] hed-hints tal-hints]~]^app
    =/  prod  [+>.hed-res +>.tal-res]
    =^  pi  app  (cache-noun prod)
    [[%& `prod] [%cons [si fi pi] hed-hints tal-hints]~]^app
  ::
      [%0 axis=@]
    ?:  =(axis 0)  [%|^trace [%0 [si fi 0] ~]~]^app
    =/  tups  (frag axis.f s)
    =/  pi  +>:(~(get by cax) -.tups)
    :_  app
    [%& `-.tups]^[%0 [si fi pi] +.tups]~
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
      [%8 hed=* next=*]
    =^  [=hed=res =hed=hints]  app
      $(f hed.f)
    =/  hit  [%8 [si fi 0] hed-hints ~]
    ?:  ?=(%| -.hed-res)  ~&  198  [%|^trace hit ~]^app
    ?~  p.hed-res  [%&^~ hit ~]^app
    =^  [=nex=res =nex=hints]  app
      $(s [u.p.hed-res s], f next.f)
    ?:  ?=(%| -.nex-res)  ~&  198  [%|^trace [%8 [si fi 0] hed-hints nex-hints] ~]^app
    ?~  p.nex-res  [%&^~ [%8 [si fi 0] hed-hints nex-hints]~]^app
    =/  pi  +>:(~(get by cax) +>.nex-res)
    [nex-res [%8 [si fi pi] hed-hints nex-hints]~]^app
  ::
      [%9 axis=@ core=*]
    ?:  =(axis 0)
      ~&  256  [%|^trace [%9 [si fi 0] ~ 0 ~]~]^app
    =^  [=core=res =core=hints]  app
      $(f core.f)
    ?:  ?=(%| -.core-res)
      ~&  211  [%|^trace [%9 [si fi 0] core-hints 0 ~]~]^app
    ?~  p.core-res  [%|^trace [%9 [si fi 0] core-hints 0 ~]~]^app
    =/  arm  (frag axis.f u.p.core-res)
    ?:  ?=(%| -.p.arm)
      ~&  269+[s axis.f]
      :_  app
      [%|^trace [%9 [si fi 0] core-hints `@ud`axis.f q.arm]~]
    =^  [=res =hints]  app  $(s u.p.core-res, f p.arm)
    =/  pi  +>:(~(get by cax) +>.res)
    [res [%9 [si fi pi] core-hints `@ud`axis.f q.arm]~]^app
  ::
      [%10 [axis=@ value=*] target=*]
    ?:  =(0 axis.f)
      ~&  232  [%|^trace [%10 [si fi 0] ~ ~ 0 ~]~]^app
    =^  [=val=res =val=hints]  app
      $(f value.f)
    ?:  ?=(%| -.val-res)
      ~&  239  [%|^trace [%10 [si fi 0] val-hints ~ 0 ~]~]^app
    ?~  p.val-res
      [%&^~ [%10 [si fi 0] val-hints ~ 0 ~]~]^app
    =^  [=tar=res =tar=hints]  app
      $(f target.f)
    ?:  ?=(%| -.tar-res)
      ~&  235
      :_  app
      :-  %|^trace
      [%10 [si fi 0] val-hints tar-hints 0 ~]~
    ?~  p.tar-res
      :_  app
      :-  %&^~
      [%10 [si fi 0] val-hints tar-hints 0 ~]~
    =^  mutant  app
      (edit axis.f u.p.tar-res u.p.val-res)
    =^  pi  app  (cache-noun mut.p.mutant)
    =/  ol  +>:(~(get by cax) +.p.mutant)
    :_  app
    :-  %&^`p.mutant
    :_  ~
    :*  %10  [si fi pi]  val-hints
        tar-hints  ol  q.mutant
    ==
  ::
      [%11 tag=@ next=*]
    =^  [=next=res =next=hints]  app
      $(f next.f)
    ?:  ?=(%| -.next-res)  ~&  260  [%|^trace [%11 [si fi 0] next-hints %|^tag.f]~]^app
    ?~  p.next-res  [%&^~ [%11 [si fi 0] next-hints %|^tag.f]~]^app
    =/  p  .*(s [11 tag.f 1 u.p.next-res])
    =/  pi  +>:(~(get by cax) p)
    :_  app
    [%&^`p [%11 [si fi pi] next-hints %|^tag.f]~]
  ::
      [%11 [tag=@ clue=*] next=*]
    =^  [=clue=res =clue=hints]  app
      $(f clue.f)
    ?:  ?=(%| -.clue-res)
      ~&  269
      [%|^trace [%11 [si fi 0] ~ %&^[tag.f clue-hints]]~]^app
    ?~  p.clue-res  [%&^~ ~]^app
    ::  if jet exists for this tag, and sample is good,
    ::  replace execution with jet
    =^  [=next=res =next=hints]  app
      :: TODO %zfast jetting here
      $(f next.f)
    ?:  ?=(%| -.next-res)
      ~&  190
      [%|^trace [%11 [si fi 0] next-hints %&^[tag.f clue-hints]]~]^app
    ?~  p.next-res  [%&^~ [%11 [si fi 0] next-hints %&^[tag.f clue-hints]]~]^app
    =/  pi  +>:(~(get by cax) +>.next-res)
    :_  app
    :_  [%11 [si fi pi] next-hints %&^[tag.f clue-hints]]~
    ?:  =(%fast tag.f)  %&^p.next-res
    :+  %&  ~
    .*  s
    [11 [tag.f 1 u.p.clue-res] 1 u.p.next-res]
  ::
  ==
  ::
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
  ::
  ++  edit
    :: TODO you need to also include the edit along the path - i.e. [?(%2 %3) oldcellindex  newcellindex]
    |=  [axis=@ target=* value=*]
    ^-  [(pair [mut=* old=*] (list (pair ?(%2 %3) index))) appendix]
    ?~  axis  !!
    =/  frg  (frag axis target)
    =/  mutant  .*(target [10 [axis 1 value] 0 1])
    [mutant^p.frg q.frg]^app
  ++  frag
    |=  [axis=@ s=*]
    =|  path=(list (pair ?(%2 %3) index))
    =/  start-axis  axis
    |^  ^-  (pair * _path) :: TODO add crash axis?
    ?:  =(1 axis)
      [s path]
    ?~  axis  !!
    ?@  s  [%|^s^(gep-a start-axis axis) path]
    =/  pick  (cap axis)
    =/  child  ?-(pick %2 -.s, %3 +.s)
    =/  pari  +>:(~(get by cax) s)
    %=  $
      s     child
      axis  (mas axis)
      path  [[pick pari] path]
    ==
    ::
    ::  solve for a in c = (peg a b)
    ++  gep-a
      |=  [p=@ b=@]
      =/  metb  (met 0 b)
      (rsh [0 (dec metb)] p)
    --
  --
--
