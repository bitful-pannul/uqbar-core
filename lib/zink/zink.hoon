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
    =^  oob  app  (take-bud 3) :: note, i think 4 because of the *[subf-res-1 subf-res-2]
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
      ?>  ?=([[%1 jet] *] clue.f) :: todo: shouldn't crash here
      =-  "jet: {(sa:dejs:format -)}"
      (en-jet:enjs ->.clue.f)
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
        :: todo: does this safe fail in zere? no it doesnt
        ?.  ?=([jet *] u.p.clue-res)
          [%|^trace [%11 %& %&^[tag-hint [hclue clue-hints]] hnext]~]^app
        (run-jet +.clue.f u.p.clue-res)
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
  ::
  ++  take-bud
    |=  amt=@ud
    ^-  [? appendix]
    ?~  bud  %|^app
    ?:  (lth u.bud amt)  %&^app
    %|^app(u.bud (sub u.bud amt))
  ::
  ++  run-jet
    |=  [sam-clue=* =jet sam=*]
    ^-  book
    ?~  cost=(~(get by jets) jet)
      ~&  >>  "no jet found"  [%&^~ ~]^app
    ?:  ?&(?=(^ bud) (lth u.bud u.cost))  [%&^~ ~]^app
    ^-  book
    ?+    jet
      =-  [- [%jet jet (noun:enjs sam)]~]^app
      =^  oob  app  (take-bud u.cost)
      ?:  oob
        %&^~
      ?~  res=(run-zuse-jet jet sam-clue sam)  %|^trace
      %&^res
    ::
        [%$ %pedersen-hash]
      =^  oob  app  (take-bud u.cost)
      ?:  oob
        [%&^~ [%jet jet ~]~]^app
      ?.  ?=([@ @] sam)  [%|^trace ~]^app
      [%&^(some (hash:pedersen sam)) ~]^app
    ::
        [%$ %pmug]
      =^  hsam  app  (hash sam)
      [%&^~^hsam [%jet jet (noun:enjs sam)]~]^app
    ::
        [%$ %pgor]
      ?.  ?=([h=* t=*] sam)  [%|^trace ~]^app
      =/  hit  [%jet jet (noun:enjs sam)]~
      =^  res  app  (pgor sam)
      ?~  res  [%|^trace hit]^app
      [%&^res hit]^app
    ::
        [%$ %pmor]
      ?.  ?=([h=* t=*] sam)  [%|^trace ~]^app
      =/  hit  [%jet jet (noun:enjs sam)]~
      =^  res  app  (pmor sam)
      ?~  res  [%|^trace hit]^app
      [%&^res hit]^app
    ::
        [%$ %reel]
      ::  we want the hints in reverse order easier to
      ::  prove the list hash that way in zere
      ::  todo: oob early if not enough gas to hash list
      ?.  ?=([lis=* [bat=* [bunt-el=* acc=*] con=*]] sam)  [%|^trace ~]^app
      =^  hlis  app  (hash lis.sam)
      =^  hbat  app  (hash bat.sam)
      =^  hbunt-el  app  (hash bunt-el.sam)
      =^  hinit  app  (hash acc.sam)
      =^  hcon  app  (hash con.sam)
      |^
      =^  lax  app  (hash-and-flop lis.sam)
      ?.  ?=(%& -.lax)  [%|^trace [%jet jet (en-not-list p.lax)]~]^app
      =*  hax  p.p.lax
      =*  lis  q.p.lax
      =*  acc  acc.sam
      =|  hit=(list hints)
      |-  ^-  book
      ?~  lis  [%&^~^acc [%jet jet (en-hints hax (flop hit))]~]^app
      =^  [=el=res =el=hints]  app
        zink-loop(s [bat.sam [i.lis acc] con.sam], f bat.sam)
      =.  hit  el-hints^hit
      ?.  ?=(%& -.el-res)  [%|^trace [%jet jet (en-hints hax (flop hit))]~]^app
      ?~  p.el-res  [%&^~ [%jet jet (en-hints hax (flop hit))]~]^app
      $(acc u.p.el-res, lis t.lis)
      ::
      ++  en-hints
        |=  [hax=(list phash) hit=(list hints)]
        ^-  json
        %-  pairs:enjs:format
        :~  list+(num:enjs hlis)
            battery+(num:enjs hbat)
            bunt-el+(num:enjs hbunt-el)
            init+(num:enjs hinit)
            context+(num:enjs hcon)
            hashes+a+(turn hax num:enjs)
            hints+a+(turn hit hints:enjs)
        ==
      ::
      ++  en-not-list
        |=  [hax=(list phash) crash-end=@]
        ^-  json
        %-  pairs:enjs:format
        :~  list+(num:enjs hlis)
            battery+(num:enjs hbat)
            battery+(num:enjs hbat)
            bunt-el+(num:enjs hbunt-el)
            context+(num:enjs hcon)
            hashes+a+(turn hax num:enjs)
            crash-end+(num:enjs crash-end)
        ==
      ::
      --
    ::
        [%$ %roll]
      ::  we want the hints in reverse order easier to
      ::  prove the list hash that way in zere
      ::  todo: oob early if not enough gas to hash list
      ?.  ?=([lis=* [bat=* [bunt-el=* acc=*] con=*]] sam)  [%|^trace ~]^app
      =^  hlis  app  (hash lis.sam)
      =^  hbat  app  (hash bat.sam)
      =^  hbunt-el  app  (hash bunt-el.sam)
      =^  hinit  app  (hash acc.sam)
      =^  hcon  app  (hash con.sam)
      |^
      =^  lax  app  (hash-and-flop lis.sam)
      ?.  ?=(%& -.lax)  [%|^trace [%jet jet (en-not-list p.lax)]~]^app
      =*  hax  p.p.lax
      =*  lis  q.p.lax
      =*  acc  acc.sam
      =.  lis  (flop lis)  :: can probably make more efficient
      =|  hit=(list hints)
      |-  ^-  book
      ?~  lis  [%&^~^acc [%jet jet (en-hints hax (flop hit))]~]^app
      =^  [=el=res =el=hints]  app
        zink-loop(s [bat.sam [i.lis acc] con.sam], f bat.sam)
      =.  hit  el-hints^hit
      ?.  ?=(%& -.el-res)  [%|^trace [%jet jet (en-hints hax (flop hit))]~]^app
      ?~  p.el-res  [%&^~ [%jet jet (en-hints hax (flop hit))]~]^app
      $(acc u.p.el-res, lis t.lis)
      ::
      ++  en-hints
        |=  [hax=(list phash) hit=(list hints)]
        ^-  json
        %-  pairs:enjs:format
        :~  list+(num:enjs hlis)
            battery+(num:enjs hbat)
            bunt-el+(num:enjs hbunt-el)
            init+(num:enjs hinit)
            context+(num:enjs hcon)
            hashes+a+(turn hax num:enjs)
            hints+a+(turn hit hints:enjs)
        ==
      ::
      ++  en-not-list
        |=  [hax=(list phash) crash-end=@]
        ^-  json
        %-  pairs:enjs:format
        :~  list+(num:enjs hlis)
            battery+(num:enjs hbat)
            battery+(num:enjs hbat)
            bunt-el+(num:enjs hbunt-el)
            context+(num:enjs hcon)
            hashes+a+(turn hax num:enjs)
            crash-end+(num:enjs crash-end)
        ==
      ::
      --
    ::
        [%$ %turn]
      ::  we want the hints in reverse order easier to
      ::  prove the list hash that way in zere
      ::  todo: oob early if not enough gas to hash list
      ?.  ?=([lis=* [bat=* bunt=* con=*]] sam)  [%|^trace ~]^app
      =^  hlis  app  (hash lis.sam)
      =^  hbat  app  (hash bat.sam)
      =^  hbunt  app  (hash bunt.sam)
      =^  hcon  app  (hash con.sam)
      |^
      =^  lax  app  (hash-and-flop lis.sam)
      ?.  ?=(%& -.lax)  [%|^trace [%jet jet (en-not-list p.lax)]~]^app
      =*  hax  p.p.lax
      =*  lis  q.p.lax
      =|  hit=(list hints)
      =|  res=(list)
      |-  ^-  book
      ?~  lis  [%&^~^res [%jet jet (en-hints hax (flop hit))]~]^app
      =^  [=el=^res =el=hints]  app  zink-loop(s [bat.sam i.lis con.sam], f bat.sam)
      =.  hit  el-hints^hit
      ?.  ?=(%& -.el-res)  [%|^trace [%jet jet (en-hints hax (flop hit))]~]^app
      ?~  p.el-res  [%&^~ [%jet jet (en-hints hax (flop hit))]~]^app
      $(res u.p.el-res^res, lis t.lis)
      ::
      ++  en-hints
        |=  [hax=(list phash) hit=(list hints)]
        ^-  json
        %-  pairs:enjs:format
        :~  list+(num:enjs hlis)
            battery+(num:enjs hbat)
            bunt+(num:enjs hbunt)
            context+(num:enjs hcon)
            hashes+a+(turn hax num:enjs)
            hints+a+(turn hit hints:enjs)
        ==
      ::
      ++  en-not-list
        |=  [hax=(list phash) crash-end=@]
        ^-  json
        %-  pairs:enjs:format
        :~  list+(num:enjs hlis)
            battery+(num:enjs hbat)
            bunt+(num:enjs hbunt)
            context+(num:enjs hcon)
            hashes+a+(turn hax num:enjs)
            crash-end+(num:enjs crash-end)
        ==
      ::
      --
    ::
        [%$ %has %pin]
      ?~  sam=((soft ,[set=(tree) val=*]) sam)  [%|^trace ~]^app
      =>  .(sam u.sam)
      =^  [axis=@ leaf=(unit) path=(list phash)]  app
        (dig-in-set set.sam val.sam pgor test same)
      =^  hset  app  (hash set.sam)
      =^  hval  app  (hash val.sam)
      =^  hleaf  app  (hash (fall leaf ~))
      =-  [%&^~^?~(leaf %| %&) hit]^app
      ^=  hit=(hints)
      :_  ~
      :+  %jet  jet
      %-  pairs:enjs:format
      :~  set+(num:enjs hset)
          val+(num:enjs hval)
          axis+(num:enjs axis)
          path+a+(turn path num:enjs)
          leaf+(num:enjs hleaf)
      ==
    ::
        [%$ %put %pin]
      ?~  sam=((soft ,[set=(tree) val=*]) sam)  [%|^trace ~]^app
      =>  .(sam u.sam)
      =^  res  app
        (put-in-tree set.sam val.sam pgor pmor test same)
      =^  hset  app  (hash set.sam)
      =^  hval  app  (hash val.sam)
      ?.  ?=(%& +<.res)
        =-  [%&^~^a.res hit]^app
        ^=  hit=(hints)
        =/  [axis=@ path=(list phash)]  p.res
        :_  ~
        :+  %jet  jet
        %-  pairs:enjs:format
        :~  set+(num:enjs hset)
            val+(num:enjs hval)
            axis+(num:enjs axis)
            path+a+(turn path num:enjs)
        ==
      =-  [%&^~^a.res hit]^app
      ^=  hit=(hints)
      =/  [nodes=(list phash) left=(list phash) right=(list phash)]  p.res
      :_  ~
      :+  %jet  jet
      %-  pairs:enjs:format
      :~  set+(num:enjs hset)
          val+(num:enjs hval)
          nodes+a+(turn nodes num:enjs)
          left+a+(turn left num:enjs)
          right+a+(turn right num:enjs)
      ==
    ::
        [%tap %pin]
      ?~  set=((soft (tree)) sam)  [%|^trace ~]^app
      =>  .(set u.set)
      =^  hset  app  (hash set)
      =^  [res=(list) nodes=hash-tree]  app  (tap-in-tree set)
      =-  [%&^~^res hit]^app
      ^-  hit=hints
      :_  ~
      :+  %jet  jet
      %-  pairs:enjs:format
      :~  set+(num:enjs hset)
          nodes+(en-hash-tree nodes)
      ==
    ::
        [%apt %pin]
      ?~  set=((soft (tree)) sam)  [%|^trace ~]^app
      =>  .(set u.set)
      =^  hset  app  (hash set)
      =^  [res=? nodes=hash-tree]  app  (apt-in-tree set pgor pmor)
      =-  [%&^~^res hit]^app
      ^-  hit=hints
      :_  ~
      :+  %jet  jet
      %-  pairs:enjs:format
      :~  set+(num:enjs hset)
          nodes+(en-hash-tree nodes)
      ==
    ::
        [%$ %has %pby]
      ?~  sam=((soft ,[map=(tree) val=*]) sam)  [%|^trace ~]^app
      =>  .(sam u.sam)
      =^  [axis=@ leaf=(unit) path=(list phash)]  app
        (dig-by-map map.sam val.sam pgor test same)
      =^  [pkey=phash pval=phash ppkey=phash ppval=phash]  app
        (get-map-kvs map.sam axis)
      =^  hmap  app  (hash map.sam)
      =^  hval  app  (hash val.sam)
      =^  hleaf  app  (hash (fall leaf ~))
      =^  hlval  app  ?~  leaf
        (hash 0)
      ?>  ?=(^ u.leaf)
      (hash +.u.leaf)
      =-  [%&^~^?~(leaf %| %&) hit]^app
      ^=  hit=(hints)
      :_  ~
      :+  %jet  jet
      %-  pairs:enjs:format
      :~  map+(num:enjs hmap)
          val+(num:enjs hval)
          axis+(num:enjs axis)
          path+a+(turn path num:enjs)
          leaf+(num:enjs hleaf)
          leaf-val+(num:enjs hlval)
          p-key+(num:enjs pkey)
          p-val+(num:enjs pval)
          pp-key+(num:enjs ppkey)
          pp-val+(num:enjs ppval)
      ==
    ::
        [%$ %zock]
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
      :-  res^[[%jet %$^%zock (noun:enjs [bud shash fhash hscry])] q.p.new-book]
      %_    app
          cax  cax.q.new-book
          bud  outer-bud
      ==
    ==
  ::
  +$  hash-tree  (tree [n=phash l=phash r=phash])
  ++  en-hash-tree
    |=  =hash-tree
    |-  ^-  json
    ?~  hash-tree  ~
    %-  pairs:enjs:format
    :~  hn+(num:enjs n.n.hash-tree)
        hl+(num:enjs l.n.hash-tree)
        hr+(num:enjs r.n.hash-tree)
        l+$(hash-tree l.hash-tree)
        r+$(hash-tree r.hash-tree)
    ==
  ::
  ++  pgor
    |=  [a=* b=*]
    ^-  [(unit ?) appendix]
    =^  c  app  (hash a)
    =^  d  app  (hash b)
    ?:  =(c d)  ~^app
    [`(lth c d)]^app
  ::
  ++  pmor
    |=  [a=* b=*]
    ^-  [(unit ?) appendix]
    =^  c  app  (hash a)
    =^  d  app  (hash b)
    =^  c  app  (hash c)
    =^  d  app  (hash d)
    ?:  =(c d)  ~^app
    [`(lth c d)]^app
  ::
  ++  dig-in-set :: basically dig, but returns axis in ~ case, and val
    |*  [a=(tree) b=* gor=$-(^ [(unit ?) appendix]) eq=$-(^ ?) get=$-(* *)]
    ^-  [[axis=@ val=(unit _(get)) path=(list phash)] appendix]
    ?:  =(~ a)  [1 ~ ~]^app
    =/  axis  1
    =|  path=(list phash)
    |-  ^-  _^$
    ?~  a
      [axis ~ path]^app
    ?:  (eq b n.a)
      =^  htala  app  (hash +.a)
      [(peg axis 2) `(get n.a) htala^path]^app
    =^  hna  app  (hash n.a)
    =^  g  app  (gor b n.a)
    ?>  ?=(^ g)
    ?:  u.g
      =^  hra  app  (hash r.a)
      $(a l.a, axis (peg axis 6), path hra^hna^path)
    =^  hla  app  (hash l.a)
    $(a r.a, axis (peg axis 7), path hla^hna^path)
  ::
  ++  dig-by-map :: basically dig, but returns axis in ~ case, and val
    |*  [a=(tree) b=* gor=$-(^ [(unit ?) appendix]) eq=$-(^ ?) get=$-(* *)]
    ^-  [[axis=@ val=(unit _(get)) path=(list phash)] appendix]
    ?:  =(~ a)  [1 ~ ~]^app
    =/  axis  1
    =|  path=(list phash)
    |-  ^-  _^$
    ?~  a
      [axis ~ path]^app
    ?:  (eq b -.n.a)
      =^  htala  app  (hash +.a)
      [(peg axis 2) `(get n.a) htala^path]^app
    =^  hna  app  (hash n.a)
    =^  g  app  (gor b n.a)
    ?>  ?=(^ g)
    ?:  u.g
      =^  hra  app  (hash r.a)
      $(a l.a, axis (peg axis 6), path hra^hna^path)
    =^  hla  app  (hash l.a)
    $(a r.a, axis (peg axis 7), path hla^hna^path)
  ::
  ::
  ++  get-map-kvs
    |=  [map=(tree) axis=@]
    ^-  [[pkey=phash pval=phash ppkey=phash ppval=phash] appendix]
    ?~  (mod (xeb 2) 2)
      [~ ~ ~ ~]^app
    ?:  |(=(axis 6) =(axis 7))
      =^  pkey  app  (hash +4.map)
      =^  pval  app  (hash +5.map)
      [pkey pval ~ ~]^app
    =/  leaf-n-axis    (dec (div axis 2))
    =/  parent-n-axis  (dec (div axis 8))
    =^  pkey  app  (hash .*(map [0 (mul 2 leaf-n-axis)]))
    =^  pval  app  (hash .*(map [0 (succ (mul 2 leaf-n-axis))]))
    =^  ppkey  app  (hash .*(map [0 (mul 2 parent-n-axis)]))
    =^  ppval  app  (hash .*(map [0 (succ (mul 2 parent-n-axis))]))
    [pkey pval ppkey ppval]^app
  ::
  ++  put-in-tree
    |*  $:  a=(tree)  b=*
            gor=$-(^ [(unit ?) appendix])
            mor=$-(^ [(unit ?) appendix])
            eq=$-(^ ?)
            get=$-(* *)
        ==
    =|  path=(list phash)
    =/  axis  1
    |-  ^-  $:  $:  a=_a
                    %+  each  [nodes=(list phash) left=(list phash) right=(list phash)]
                    [axis=@ path=(list phash)]
                ==
                appendix
            ==
    ?~  a
      [[b ~ ~] %& ~ ~ ~]^app
    ?:  (eq (get b) (get n.a))
      =^  htala  app  (hash +.a)
      [[b l.a r.a] %| (peg axis 2) htala^path]^app
    =^  hna  app  (hash n.a)
    =^  hra  app  (hash r.a)
    =^  hla  app  (hash l.a)
    =^  g  app  (gor b n.a)
    ?>  ?=(^ g)
    ?:  u.g
      =^  c  app  $(a l.a, path hra^hna^path, axis (peg axis 6))
      ?.  ?=(%& +<.c)  [c(a a(l a.c)) app]
      ?>  ?=(^ a.c)
      =^  m  app  (mor n.a n.a.c)
      ?>  ?=(^ m)
      :_  app
      %_  c
        nodes.p  hna^nodes.p.c
        left.p   hla^left.p.c
        right.p  hra^right.p.c
        a      ?:(u.m a(l a.c) a.c(r a(l r.a.c)))
      ==
    =^  c  app  $(a r.a, path hla^hna^path, axis (peg axis 7))
    ?.  ?=(%& +<.c)  [c(a a(r a.c)) app]
    ?>  ?=(^ a.c)
    =^  m  app  (mor n.a n.a.c)
    ?>  ?=(^ m)
    :_  app
    %_  c
      nodes.p  hna^nodes.p.c
      left.p   hla^left.p.c
      right.p  hra^right.p.c
      a        ?:(u.m a(r a.c) a.c(l a(r l.a.c)))
    ==
  ::
  ++  tap-in-tree
    |=  a=(tree)
    ^-  [[res=(list) nodes=hash-tree] appendix]
    ?:  =(a ~)  [~ ~]^app
    =|  res=(list)
    |-  ^-  _^$
    ?~  a  [res ~]^app
    =^  hna  app  (hash n.a)
    =^  hla  app  (hash l.a)
    =^  hra  app  (hash r.a)
    =^  l  app  $(a l.a)
    =^  r  app  $(a r.a, res n.a^res.l)
    :_  app
    r(nodes [n=[hna hla hra] nodes.l nodes.r])
  ::
  ++  apt-in-tree
    |*  $:  a=(tree)
            gor=$-(^ [(unit ?) appendix])
            mor=$-(^ [(unit ?) appendix])
        ==
    ^-  [[? nodes=hash-tree] appendix]
    ?:  =(a ~)  [%& ~]^app
    =|  [l=(unit) r=(unit)]
    |-  ^-  $_  ^$
    ?~  a  [& ~]^app
    =^  hna  app  (hash n.a)
    =^  hla  app  (hash l.a)
    =^  hra  app  (hash r.a)
    =/  nodes  [hna hla hra]
    =^  g-na-ul   app  ?~(l [~ u=%&]^app (gor n.a u.l))
    ?>  ?=(^ g-na-ul)
    ?.  u.g-na-ul  [%| [nodes ~ ~]]^app
    =^  g-ur-na   app  ?~(r [~ u=%&]^app (gor u.r n.a))
    ?>  ?=(^ g-ur-na)
    ?.  u.g-ur-na  [%| [nodes ~ ~]]^app
    :: todo: prob more efficient to do mor first
    :: but zere jet is easier if not more efficient to write
    :: apt'ing l/r first
    =^  apt-la  app  $(a l.a, l `n.a)
    ?.  -.apt-la  [%| [nodes nodes.apt-la ~]]^app
    =^  apt-ra  app  $(a r.a, r `n.a)
    ?.  -.apt-ra  [%| [nodes nodes.apt-la nodes.apt-ra]]^app
    =^  m-na-nla  app  ?~(l.a [~ u=%&]^app (mor n.a n.l.a))
    ?>  ?=(^ m-na-nla)
    ?.  u.m-na-nla  [%| [nodes nodes.apt-la nodes.apt-ra]]^app
    =^  m-na-nra  app  ?~(r.a [~ u=%&]^app (mor n.a n.r.a))
    ?>  ?=(^ m-na-nra)
    ?.  u.m-na-nra  [%| [nodes nodes.apt-la nodes.apt-ra]]^app
    [%& [nodes nodes.apt-la nodes.apt-ra]]^app
  ::
  ++  hash-and-flop
    |=  [lis=*]
    =|  out=(list)
    =|  hax=(list phash)
    |-  ^-  [(each (pair (list phash) (list)) (pair (list phash) @)) appendix]
    ?~  lis  [%& hax out]^app
    ?@  lis  [%| hax lis]^app
    =^  hel  app  (hash -.lis)
    $(lis +.lis, out [-.lis out], hax [hel hax])
  ::
  ++  run-zuse-jet
    |=  [jet=* clue=* clue-res=*]
    ?>  ?=([@tas *] jet)
    =/  z=vase  !>(..zuse)
    =/  final-wing  -.jet
    =>  .(jet `*`+.jet)
    |^  ^-  (unit)
    =/  grabbed  (grab tail-wing)
    =/  ton
      (mink [q.z [7 q.grabbed (nock-10s-and-final p.grabbed)]] *$-(^ (unit (unit))))
    ?.(?=(%0 -.ton) ~ `product.ton)
    ::
    ++  final
      |=  =type
      q:(~(mint ut type) %noun [%wing [final-wing ~]])
    ::
    ++  nock-10s-and-final
      |=  =type
      |-  ^-  nock
      ?:  ?=([%0 axis=@] clue)
        [%7 [%10 [axis.clue %1 clue-res] %0 1] (final type)]
      ?>  ?=([i=[%0 axis=@] t=*] clue)
      ?>  ?=([i=* t=*] clue-res)
      :+    %7
        [%10 [axis.i.clue %1 i.clue-res] %0 1]
      $(clue t.clue, clue-res t.clue-res)
    ::
    ++  tail-wing
      =|  w=wing
      |-  ^-  wing
      ?@  jet  jet^w
      ?>  ?=([@ *] jet)
      $(jet `*`+.jet, w -.jet^w)
    ::
    ++  grab
      |=  wing=wing
      ^-  (pair type nock)
      =/  grab-form
        ?>  ?=(^ wing)
        %+  roll  t.wing
        |:  [limb=*limb hoon=`hoon`[%wing i.wing ~]]
        ^-  ^hoon
        [%tsgl [%wing limb ~] hoon]
      =/  z=vase  !>(..zuse)
      (~(mint ut p.z) %noun grab-form)
    --
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
    =-  [-<- ->]
    |-  ^-  [(pair phash @ud) appendix]
    =/  mh  (~(get by cax) n)
    ?^  mh
      [u.mh app]
    ?@  n
      =/  h  (hash:pedersen n 0)
      =/  v  [h 1]
      :-  v
      app(cax (~(put by cax) n v))
    =^  vh  app  $(n -.n)
    =^  vt  app  $(n +.n)
    =/  h  (hash:pedersen p.vh p.vt)
    =/  v
      [h +((add q.vh q.vt))]
    :-  v
    app(cax (~(put by cax) n v))
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
