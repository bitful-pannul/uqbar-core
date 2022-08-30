/-  *zink
/+  *zink-pedersen, *zink-json
=>  |%
    +$  good      (unit *)
    +$  fail      (list [@ta *])
    +$  res       (each good fail)
    +$  body      [p=res q=hints]
    +$  appendix  [cax=cache bud=(unit @ud) scrys=(list *)]
    +$  book      (pair body appendix)
    --
|%
++  zebra                                                 ::  bounded zk +mule
  |=  [bud=(unit @ud) cax=cache scry=(unit granary-scry) [s=* f=*] test-mode=?]
  ^-  book
  %.  [s f test-mode]
  %*  .  zink
    app  [cax bud ?~(scry ~ [`*`u.scry ~])]
  ==
::
++  hash
  |=  [n=* cax=cache]
  ^-  phash
  ?@  n
    ?:  (lte n 12)
      =/  ch  (~(get by cax) n)
      ?^  ch  p.u.ch
      (hash:pedersen n 0)
    (hash:pedersen n 0)
  ?^  ch=(~(get by cax) n)
    p.u.ch
  =/  hh  $(n -.n)
  =/  ht  $(n +.n)
  (hash:pedersen hh ht)
::
++  create-hints
  |=  [n=^ h=hints cax=cache]
  ^-  json
  =/  hs  (hash -.n cax)
  =/  hf  (hash +.n cax)
  %-  pairs:enjs:format
  :~  hints+(hints:enjs h)
      subject+(num:enjs hs)
      formula+(num:enjs hf)
  ==
::
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
    =^  htal  app  (hash +.f)
    [%|^trace [%invalid %|^[-.f htal]]~]^app
  ::
      [^ *]
    =^  oob  app  (take-bud 4)
    ?:  oob  [%&^~ [%cons [0 ~] [0 ~]]~]^app
    =^  hhed  app  (hash -.f)
    =^  htal  app  (hash +.f)
    =^  [=hed=res =hed=hints]  app
      $(f -.f)
    ?:  ?=(%| -.hed-res)
      ~&  61  [%|^trace [%cons [hhed hed-hints] [htal ~]]~]^app
    ?~  p.hed-res  [%&^~ [%cons [hhed hed-hints] [htal ~]]~]^app
    =^  [=tal=res =tal=hints]  app
      $(f +.f)
    =/  hit  [%cons [hhed hed-hints] [htal tal-hints]]~
    ?:  ?=(%| -.tal-res)
      ~&  65  [%|^trace hit]^app
    ?~  p.tal-res  [%&^~ hit]^app
    :_  app
    [%& `u.p.hed-res^u.p.tal-res]^hit
  ::
      [%0 axis=@]
    =^  oob  app  (take-bud 1)
    ?:  oob
      =^  haxis  app  (hash axis.f)
      [%&^~ [%0 %| haxis]~]^app
    ?:  =(axis 0)  [%|^trace [%0 %& 0 %&^0 ~]~]^app
    =/  proof-cost  (dec (met 0 axis.f))
    =^  oob  app  (take-bud proof-cost)
    ?:  oob
      [%&^~ [%0 %& axis.f %|^[0 0] ~]~]^app
    =^  hsibs  app  (frag axis.f s)
    ?:  ?=(%| -.p.hsibs)
      [%|^trace [%0 %& axis.f p.hsibs q.hsibs]~]^app
    =^  rh  app  (hash p.p.hsibs) :: this will always be a cache hit. dec?
    =/  hit  [%0 %& axis.f %&^rh q.hsibs]~
    :_  app
    [%& `p.p.hsibs]^hit
  ::
      [%1 const=*]
    =^  hres  app  (hash const.f)
    [[%& `const.f] [%1 hres]~]^app
  ::
      [%2 sub=* for=*]
    =^  oob  app  (take-bud 3)
    ?:  oob
      =^  htal  app  (hash +.f)
      [%&^~ [%2 %|^htal]~]^app
    =^  hsub  app  (hash sub.f)
    =^  hfor  app  (hash for.f)
    =^  [=sub=res =sub=hints]  app
      $(f sub.f)
    ?:  ?=(%| -.sub-res)
      ~&  99  [%|^trace [%2 %& [hsub sub-hints] [hfor ~]]~]^app
    ?~  p.sub-res  [%&^~ [%2 %& [hsub sub-hints] [hfor ~]]~]^app
    =^  [=for=res =for=hints]  app
      $(f for.f)
    =/  hit=cairo-hint  [%2 %& [hsub sub-hints] [hfor for-hints]]
    ?:  ?=(%| -.for-res)
      ~&  103  [%|^trace hit ~]^app
    ?~  p.for-res  [%&^~ hit ~]^app
    =-  -(q.p hit^q.p.-)
    %_  $
      s    u.p.sub-res
      f    u.p.for-res
    ==
  ::
      [%3 arg=*]
    =^  oog  app  (take-bud 3)
    ?:  oog
      =^  htal  app  (hash +.f)
      [%&^~ [%3 %|^htal]~]^app
    =^  harg  app  (hash arg.f)
    =^  [=arg=res =arg=hints]  app
      $(f arg.f)
    ?:  ?=(%| -.arg-res)
      ~&  114  [%|^trace [%3 %& [harg arg-hints] ~]~]^app
    ?~  p.arg-res  [%&^~ [%3 %& [harg arg-hints] ~]~]^app
    ?@  u.p.arg-res
      :_  app
      :-  [%& ~ %.n]
      [%3 %& [harg arg-hints] ~ %atom u.p.arg-res]~
    ::  should be cached. dec?
    =^  hhash  app  (hash -.u.p.arg-res)
    =^  thash  app  (hash +.u.p.arg-res)
    :_  app
    :-  [%& ~ %.y]
    [%3 %& [harg arg-hints] ~ %cell hhash thash]~
  ::
      [%4 arg=*]
    =^  oob  app  (take-bud 3)
    ?:  oob
      =^  htal  app  (hash +.f)
      [%&^~ [%4 %|^htal]~]^app
    =^  harg  app  (hash arg.f)
    =^  [=arg=res =arg=hints]  app
      $(f arg.f)
    ?:  ?=(%| -.arg-res)
      ~&  131  [%|^trace [%4 %& [harg arg-hints] ~]~]^app
    ?~  p.arg-res  [%&^~ [%4 %& [harg arg-hints] ~]~]^app
    ?^  u.p.arg-res
      =^  hhed  app  (hash -.u.p.arg-res)
      =^  htal  app  (hash +.u.p.arg-res)
      :: =+  arg=[]
      ~&  135  [%|^trace [%4 %& [harg arg-hints] `%cell^[hhed htal]]~]^app
    :_  app
    :-  [%& ~ .+(u.p.arg-res)]
    [%4 %& [harg arg-hints] `%atom^u.p.arg-res]~
  ::
      [%5 a=* b=*]
    =^  oob  app  (take-bud 3)
    ?:  oob
      =^  htal  app  (hash +.f)
      [%&^~ [%5 %|^htal]~]^app
    =^  ha  app  (hash a.f)
    =^  hb  app  (hash b.f)
    =^  [=a=res =a=hints]  app
      $(f a.f)
    ?:  ?=(%| -.a-res)
      ~&  146  [%|^trace [%5 %& [ha a-hints] [hb ~]]~]^app
    ?~  p.a-res  [%&^~ [%5 %& [ha a-hints] [hb ~]]~]^app
    =^  [=b=res =b=hints]  app
      $(f b.f)
    =/  hit  [%5 %& [ha a-hints] [hb b-hints]]~
    ?:  ?=(%| -.b-res)
      ~&  150  [%|^trace hit]^app
    ?~  p.b-res  [%&^~ hit]^app
    [[%& ~ =(u.p.a-res u.p.b-res)] hit]^app
  ::
  ::  6 is special
  ::  if [subject test] returns anything but 0 1, fail
  ::  so we never have to hash yes/no in that case, hence 2
      [%6 test=* yes=* no=*]
    =^  oob  app  (take-bud 4)
    ?:  oob
      =^  htal  app  (hash +.f)
      [%&^~ [%6 %|^htal]~]^app
    =^  htest  app  (hash test.f)
    =^  hyes   app  (hash yes.f)
    =^  hno    app  (hash no.f)
    =^  [=res =hints]  app
      $(f test.f)
    =/  hit  [%6 %& [htest hints] hyes hno]
    ?:  ?=(%| -.res)
      ~&  164  [%|^trace hit ~]^app
    ?~  p.res  [%&^~ hit ~]^app
    =-  -(q.p hit^q.p.-)
    ?+  u.p.res  ~&  167  `book`[%|^trace ~]^app
      %&  $(f yes.f)
      %|  $(f no.f)
    ==
  ::
      [%7 subj=* next=*]
    =^  oob  app  (take-bud 3)
    ?:  oob
      =^  htal  app  (hash +.f)
      [%&^~ [%7 %|^htal]~]^app
    =^  hsubj  app  (hash subj.f)
    =^  hnext  app  (hash next.f)
    =^  [=sub=res =sub=hints]  app
      $(f subj.f)
    =/  hit  [%7 %& [hsubj sub-hints] hnext]
    ?:  ?=(%| -.sub-res)  ~&  179  [%|^trace hit ~]^app
    ?~  p.sub-res  [%&^~ hit ~]^app
    =-  -(q.p hit^q.p.-)
    %_  $
      s    u.p.sub-res
      f    next.f
    ==
  ::
      [%8 hed=* next=*]
    =^  oob  app  (take-bud 4)
    ?:  oob
      =^  htal  app  (hash +.f)
      [%&^~ [%8 %|^htal]~]^app
    =^  hhed  app  (hash hed.f)
    =^  hnext  app  (hash next.f)
    =^  [=hed=res =hed=hints]  app
      $(f hed.f)
    =/  hit  [%8 %& [hhed hed-hints] hnext]
    ?:  ?=(%| -.hed-res)  ~&  198  [%|^trace hit ~]^app
    ?~  p.hed-res  [%&^~ hit ~]^app
    =-  -(q.p hit^q.p.-)
    %_  $
      s    [u.p.hed-res s]
      f    next.f
    ==
  ::
      [%9 axis=@ core=*]
    =^  oob  app  (take-bud 5)
    ?:  oob
      =^  htal  app  (hash +.f)
      [%&^~ [%9 %|^htal]~]^app
    =^  hcore  app  (hash core.f)
    ?:  =(axis 0)
      ~&  256  [%|^trace [%9 %& axis.f [hcore ~] %&^0 ~]~]^app
    =/  proof-cost  (dec (met 0 axis.f))
    =^  oob  app  (take-bud proof-cost)
    ?:  oob
      [%&^~ [%9 %& axis.f [hcore ~] %&^0 ~]~]^app
    =^  [=core=res =core=hints]  app
      $(f core.f)
    ?:  ?=(%| -.core-res)
      ~&  211  [%|^trace [%9 %& axis.f [hcore core-hints] %&^0 ~]~]^app
    ?~  p.core-res  [%&^~ [%9 %& axis.f [hcore core-hints] %&^0 ~]~]^app
    =^  arm  app  (frag axis.f u.p.core-res)
    ?:  ?=(%| -.p.arm)
      ~&  269+[s axis.f]
      :_  app
      [%|^trace [%9 %& axis.f [hcore core-hints] p.arm q.arm]~]
    =^  harm  app  (hash p.p.arm) :: this will always be a cache hit. dec?
    =/  hit  [%9 %& axis.f [hcore core-hints] %&^harm q.arm]
    =-  -(q.p hit^q.p.-)
    %_  $
      s    u.p.core-res
      f    p.p.arm
    ==
  ::
  ::  ten is special, if axis is invalid
  ::  target never needs to be hashed
      [%10 [axis=@ value=*] target=*]
    =^  oob  app  (take-bud 5)
    ?:  oob
      =^  fh  app  (hash +.f)
        [%&^~ [%10 %|^fh]~]^app
    =^  hval  app  (hash value.f)
    =^  htar  app  (hash target.f)
    ?:  =(0 axis.f)
      ~&  232  [%|^trace [%10 %& axis.f [hval ~] [htar ~] %&^0 ~]~]^app
    =/  proof-cost  (mul 2 (dec (met 0 axis.f)))
    :: todo: don't take gas until the end, but check in between each val
    =^  oob  app  (take-bud proof-cost)
    ?:  oob
      [%&^~ [%10 %& axis.f [hval ~] [htar ~] %&^0 ~]~]^app
    =^  [=val=res =val=hints]  app
      $(f value.f)
    ?:  ?=(%| -.val-res)
      ~&  239  [%|^trace [%10 %& axis.f [hval val-hints] [htar ~] %&^0 ~]~]^app
    ?~  p.val-res
      [%&^~ [%10 %& axis.f [hval val-hints] [htar ~] %&^0 ~]~]^app
    =^  [=tar=res =tar=hints]  app
      $(f target.f)
    ?:  ?=(%| -.tar-res)
      ~&  235
      :_  app
      :-  %|^trace
      [%10 %& axis.f [hval val-hints] [htar tar-hints] %&^0 ~]~
    ?~  p.tar-res
      :_  app
      :-  %&^~
      [%10 %& axis.f [hval val-hints] [htar tar-hints] %&^0 ~]~
    =^  mutant  app
      (edit axis.f u.p.tar-res u.p.val-res)
    ?:  ?=(%| -.p.mutant)
      :_  app
      :-  %|^trace
      :_  ~
      :*  %10  %&  axis.f  [hval val-hints]
         [htar tar-hints]  p.mutant  q.mutant
      ==
    =^  hold  app  (hash old.p.p.mutant)
    :_  app
    :-  %&^~^mut.p.p.mutant
    :_  ~
    :*  %10  %&  axis.f  [hval val-hints]
        [htar tar-hints]  %&^hold  q.mutant
    ==
  ::
       [%11 tag=@ next=*]
    =^  oob  app  (take-bud 3)
    ?:  oob
      =^  fh  app  (hash +.f)
        [%&^~ [%11 %|^fh]~]^app
    =^  hnext  app  (hash next)
    =^  [=next=res =next=hints]  app
      $(f next.f)
    =/  hit=hints  [%11 %& %|^tag.f hnext]^next-hints
    :_  app
    ?:  ?=(%| -.next-res)  ~&  260  [%|^trace]^hit
    ?~  p.next-res  [%&^~]^~
    :_  hit
    :+  %&  ~
    .*  s
    [11 tag.f 1 u.p.next-res]
  ::
      [%11 [tag=@ clue=*] next=*]
    =^  oob  app  (take-bud 5)
    ?:  oob
      =^  fh  app  (hash +.f)
        [%&^~ [%11 %|^fh]~]^app
    =^  hclue  app  (hash clue.f)
    =^  hnext  app  (hash next.f)
    ::  look for jet with this tag and compute sample
    ~&  >  "hint: {<`@tas`tag.f>}"
    ~?  ?=(%zfast tag.f)
      ?>  ?=([[@ @] *] clue.f) :: todo: shouldn't crash here
      "jet: {<`@tas`+.-.clue.f>}"
    =^  htag  app  (hash tag.f)
    :: we can go straight to jetting in zere with this
    =/  tag-hint=@  ?:(?=(%zfast tag.f) htag tag.f)
    =^  [=clue=res =clue=hints]  app
      $(f clue.f)
    ?:  ?=(%| -.clue-res)
      ~&  269
      [%|^trace [%11 %& %&^[tag-hint [hclue clue-hints]] hnext]~]^app
    ?~  p.clue-res  [%&^~ ~]^app
    ::  if jet exists for this tag, and sample is good,
    ::  replace execution with jet
    =^  [=next=res =next=hints]  app
      ?:  =(tag.f %zfast)
        :: todo: does this safe fail in zere?
        ?.  ?=([@tas *] u.p.clue-res)
          [%|^trace [%11 %& %&^[tag-hint [hclue clue-hints]] hnext]~]^app
        (jet u.p.clue-res)
      =?    trace
          ?=(?(%hunk %hand %lose %mean %spot) tag.f)
        [[tag.f u.p.clue-res] trace]
      $(f next.f)
    =/  hit  [%11 %& %&^[tag-hint [hclue clue-hints]] hnext]^next-hints
    ?:  ?=(%| -.next-res)
      ~&  190
      [%|^trace hit]^app
    ?~  p.next-res  [%&^~ hit]^app
    :_  app
    :_  hit
    ?:  =(%fast tag.f)  %&^p.next-res
    :+  %&  ~
    .*  s
    [11 [tag.f 1 u.p.clue-res] 1 u.p.next-res]
  ::
      [%12 ref=* path=*]
    ?:  =(scrys 0) :: dunno why i can't use ?~
      ^-  book
      =^  fh  app  `[phash appendix]`(hash +.f)
      [%|^trace [%12 %|^fh]~]^app
    ::  todo: see notes for bud12 in zere
    =^  oob  app  (take-bud 5)
    ?:  oob
      =^  fh  app  (hash +.f)
        [%&^~ [%12 %|^fh]~]^app
    =^  href  app  (hash ref.f)
    =^  hpath  app  (hash path.f)
    =^  [=ref=res =ref=hints]  app
      $(f ref.f)
    ?:  ?=(%| -.ref-res)
      ~&  289  [%|^trace [%12 %& [href ref-hints] [hpath ~]]~]^app
    ?~  p.ref-res            [%&^~ [%12 %& [href ref-hints] [hpath ~]]~]^app
    =^  [=path=res =path=hints]  app
      $(f path.f)
    =/  hit  [%12 %& [href ref-hints] [hpath (flop path-hints)]]
    ?:  ?=(%| -.path-res)
      ~&  293  [%|^trace hit ~]^app
    ?~  p.path-res
      [%&^~ hit ~]^app
    ?>  ?=(^ scrys) :: see above comment
    =-  -(q.p hit^q.p.-, scrys.q scrys)
    $(s i.scrys, f [9 2 10 [6 1 [p.ref-res p.path-res]] 0 1], scrys.app t.scrys)
  ==
  ::
  ++  zink-loop  $
  ++  jet
    |=  [tag=@ sam=*]
    ^-  book
    ?:  ?=(%slog tag)
      ::  print trace printfs?
      [%&^~ ~]^app
    ?:  ?=(%mean tag)
      ::  this is a crash..
      ~&  317  [%|^trace ~]^app
    ?~  cost=(~(get by jets) tag)
      ~&  >>  "no jet found"  [%&^~ ~]^app
    ?:  ?&(?=(^ bud) (lth u.bud u.cost))  [%&^~ ~]^app
    =-  -(bud.q (bind bud.q.- |=(bud=@ (sub bud u.cost))))
    (run-jet tag sam u.cost)
  ::
  ++  take-bud
    |=  amt=@ud
    ^-  [? appendix]
    ?~  bud  %|^app
    ?:  (lth u.bud amt)  %&^app
    %|^app(u.bud (sub u.bud amt))
  ::
  ++  run-jet
    |=  [tag=@ sam=* cost=@ud]
    ^-  book
    ?:  ?=(%zock tag)
      ?.  ?=([bud=(unit @) [s=* f=*] scry=*] sam)  [%|^trace ~]^app
      =^  shash  app  (hash s.sam)
      =^  fhash  app  (hash f.sam)
      =^  hscry  app  (hash scry.sam)
      =/  inner-bud=(unit @ud)
        ?~  bud  bud.sam
        ?~  bud.sam  bud
        ?:  (lth u.bud u.bud.sam)  bud
        bud.sam
      =/  new-book
        %_    zink-loop
            s      s.sam
            f      f.sam
            scrys  scry.sam^scrys
            bud    inner-bud
        ==
      =/  diff
        ?~  inner-bud  0
        ?>  ?=(^ bud.q.new-book)
        (sub u.inner-bud u.bud.q.new-book)
      =/  outer-bud
        ?~  bud  bud
        `(sub u.bud diff)
      =/  real-inner-bud
        ?~  bud.sam  ~
        `(sub u.bud.sam diff)
      =/  =res
        ?-  p.p.new-book
            [%& ^]  %&^~^[%0 real-inner-bud u.p.p.p.new-book]
            [%& ~]
          ?:  =(outer-bud `0)  %&^~
          %&^~^[%1 real-inner-bud]
        ::
            [%| *]  %&^~^[%2 real-inner-bud]
        ==
      :-  res^[[%jet %zock [bud shash fhash hscry]] q.p.new-book]
      %_    app
          cax  cax.q.new-book
          bud  outer-bud
      ==
    =-  [- [%jet tag sam]~]^app
    ::  TODO: probably unsustainable to need to include assertions to
    ::  make all jets crash safe.
    ?+    tag  %|^trace
    ::                                                                       ::
    ::  math                                                                 ::
    ::                                                                       ::
        %add
      ?.  ?=([@ @] sam)  %|^trace
      %&^(some (add sam))
    ::
        %dec
      ?.  ?=(@ sam)  %|^trace
      %&^(some (dec sam))
    ::
        %div
      ?.  ?=([@ @] sam)  %|^trace
      ?.  (gth +.sam 0)  %|^trace
      %&^(some (div sam))
    ::
        %dvr
      ?.  ?=([@ @] sam)  %|^trace
      ?.  (gth +.sam 0)  %|^trace
      %&^(some (dvr sam))
    ::
        %gte
      ?.  ?=([@ @] sam)  %|^trace
      %&^(some (gte sam))
    ::
        %gth
      ?.  ?=([@ @] sam)  %|^trace
      %&^(some (gth sam))
    ::
        %lte
      ?.  ?=([@ @] sam)  %|^trace
      %&^(some (lte sam))
    ::
        %lth
      ?.  ?=([@ @] sam)  %|^trace
      %&^(some (lth sam))
    ::
        %max
      ?.  ?=([@ @] sam)  %|^trace
      %&^(some (max sam))
    ::
        %min
      ?.  ?=([@ @] sam)  %|^trace
      %&^(some (min sam))
    ::
        %mod
      ?.  ?=([@ @] sam)  %|^trace
      ?.  (gth +.sam 0)  %|^trace
      %&^(some (mod sam))
    ::
        %mul
      ?.  ?=([@ @] sam)  %|^trace
      %&^(some (mul sam))
    ::
        %sub
      ?.  ?=([@ @] sam)      %|^trace
      ?.  (gte -.sam +.sam)  %|^trace
      %&^(some (sub sam))
    ::                                                                       ::
    ::  bits                                                                 ::
    ::                                                                       ::
        %bex
      ?.  ?=(bloq sam)  %|^trace
      %&^(some (bex sam))
    ::
        %can
      ::  TODO validate
      %&^(some (slum can sam))
    ::
        %cat
      ?.  ?=([bloq @ @] sam)  %|^trace
      %&^(some (cat sam))
    ::
        %cut
      ?.  ?=([bloq [step step] @] sam)  %|^trace
      %&^(some (cut sam))
    ::
        %end
      ?.  ?=([bite @] sam)  %|^trace
      %&^(some (end sam))
    ::
        %fil
      ?.  ?=([bloq step @] sam)  %|^trace
      %&^(some (fil sam))
    ::
        %lsh
      ?.  ?=([bloq @] sam)  %|^trace
      %&^(some (lsh sam))
    ::
        %met
      ?.  ?=([bloq @] sam)  %|^trace
      %&^(some (met sam))
    ::
        %rap
      ::  TODO validate
      %&^(some (slum rap sam))
    ::
        %rep
      ::  TODO validate
      %&^(some (slum rep sam))
    ::
        %rev
      ?.  ?=([bloq @ud @] sam)  %|^trace
      %&^(some (rev sam))
    ::
        %rip
      ?.  ?=([bite @] sam)  %|^trace
      %&^(some (rip sam))
    ::
        %rsh
      ?.  ?=([bite @] sam)  %|^trace
      %&^(some (rsh sam))
    ::
        %run
      ::  TODO validate
      %&^(some (slum run sam))
    ::
        %rut
      ::  TODO validate
      %&^(some (slum rut sam))
    ::
        %sew
      ?.  ?=([bloq [step step @] @] sam)  %|^trace
      %&^(some (sew sam))
    ::
        %swp
      ?.  ?=([bloq @] sam)  %|^trace
      %&^(some (swp sam))
    ::
        %xeb
      ?.  ?=(@ sam)  %|^trace
      %&^(some (xeb sam))
    ::
    ::                                                                       ::
    ::  list                                                                 ::
    ::                                                                       ::
        %turn
      ::  TODO: determine how best to validate complex jet inputs
      ::  this will crash if the input is bad.
      %&^(some (slum turn sam))
    ::                                                                       ::
    ::  sha                                                                  ::
    ::                                                                       ::
        %sham
      %&^(some (sham sam))
    ::
        %shax
      ?.  ?=(@ sam)  %|^trace
      %&^(some (shax sam))
    ::
        %shay
      ?.  ?=([@u @] sam)  %|^trace
      %&^(some (shay sam))
    ::                                                                       ::
    ::  etc                                                                  ::
    ::                                                                       ::
        %need
      ?.  ?=((unit) sam)  %|^trace
      ?:  ?=(~ sam)       %|^trace
      %&^(some (need sam))
    ::
        %scot
      ?.  ?=([@ta @] sam)  %|^trace
      %&^(some (scot sam))
    ::
        %pedersen-hash
      ?.  ?=([@ @] sam)  %|^trace
      %&^(some (hash:pedersen sam))
    ==
  ::
  ++  edit
    |=  [axis=@ target=* value=*]
    ^-  [(pair (each [mut=* old=*] [atom=@ crash-axis=@]) (list phash)) appendix]
    ?~  axis  !!
    =^  frg  app  (frag axis target)
    ?.  ?=(%& -.p.frg)  frg^app
    ::  we don't use the hash, but let's get it in the hash map
    ::  we already know if we'll run out of gas by
    ::  (lth bud (mul 2 (dec (met 0 axis))))
    ::  and if it'll crash from a, so let's just run nock 10
    =/  mutant  .*(target [10 [axis 1 value] 0 1])
    =^  mhash  app  (hash mutant)
    [%&^mutant^p.p.frg q.frg]^app
  ::
  ++  hash
    |=  [n=*]
    |-  ^-  [phash appendix]
    ::  test mode disables hashing, so it won't generate valid hints.
    ::  however, computation is *much* faster since hashing is the
    ::  most expensive aspect of the process.
    ?:  test-mode  [0x1 app]
    =-  [p.-< ->]
    |-  ^-  [(pair phash @ud) appendix]
    =/  mh  (~(get by cax) n)
    ?^  mh
      [u.mh app]
    ?@  n
      =/  h  [(hash:pedersen n 0) 1]
      :-  h
      app(cax (~(put by cax) n h))
    =^  hh=(pair phash @ud)  app  $(n -.n)
    =^  ht=(pair phash @ud)  app  $(n +.n)
    =/  h  [(hash:pedersen p.hh p.ht) +((add q.hh q.ht))]
    :-  h
    app(cax (~(put by cax) n h))
  ::
  ++  frag
    |=  [axis=@ s=*]
    =|  path=(list phash)
    =/  start-axis  axis
    |^  ^-  [(pair (each * [=atom crash-axis=@]) _path) appendix]
    ?:  =(1 axis)
      [%&^s path]^app
    ?~  axis  !!
    ?@  s  [%|^s^(gep-a start-axis axis) path]^app
    =/  pick  (cap axis)
    =^  sib  app
      %-  hash
      ?-(pick %2 +.s, %3 -.s)
    =/  child  ?-(pick %2 -.s, %3 +.s)
    %=  $
      s     child
      axis  (mas axis)
      path  [sib path]
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
