/-  *zink
/+  *zink-pedersen, *zink-json
|%
++  zebra                                                 ::  bounded zk +mule
  |=  [bud=(unit @ud) cax=cache scry=(unit granary-scry) [s=* f=*] test-mode=?]
  ^-  book
  %.  [s f test-mode]
  %*  .  zink
    app  [~ cax ~ bud ?~(scry ~ [`*`u.scry ~])]
  ==
::
++  zink
  =|  appendix
  =*  app  -
  =|  trace=fail
  =/  ci=@ud  1
  |=  [s=* f=* test-mode=?]
  ^-  book
  =*  args  +<
  =<  [- app]
  =<  $
  |_  *  ::  retarded atrocity to keep the feel of |^
  +*  this  .  :: but have virtual arms, and not lose type information of sample
      zink-loop  $ :: yeah idiomatically this is kind of awful
  ++  $  :: but i want to keep it as short and sweet as possible
  =+  args
  ^-  [body _this]
  =<  =^  [=p=res =p=zint]  this  +
      =?  preds  ?=([%& ~ *] p-res)
        (~(put by preds) s^f u.p.p-res)
      [p-res p-zint]^this
  :-  .
  ^-  [body _this]
  =^  si  this  (index-bun s)
  ?^  u=(~(get by preds) s f)
    =^  fi  this  (index-bun f)
    =^  pi  this  (index-bun u.u)
    [%&^~^u.u [%memo si fi pi]]^this
  ?@  f
    =^  fi  this  (index-atom f)
    [%|^trace [%invalid ~]]^this
  =^  [fi=@ud hfi=@ud tfi=@ud]  this  (index-cell f)
  ?+    f
    ?>  ?=(@ -.f)
    [%|^trace [%invalid ~]]^this
  ::
      [^ *]
    =^  [=hed=res =hed=zint]  this
      $(f -.f)
    ?:  ?=(%| -.hed-res)
      ~&  61  [%|^trace [%cons [si fi 0] `hed-zint ~]]^this
    ?~  p.hed-res  [%&^~ [%cons [si fi 0] `hed-zint ~]]^this
    =^  [=tal=res =tal=zint]  this
      $(f +.f)
    ?:  ?=(%| -.tal-res)
      ~&  65  [%|^trace [%cons [si fi 0] `hed-zint `tal-zint]]^this
    ?~  p.tal-res  [%&^~ [%cons [si fi 0] `hed-zint `tal-zint]]^this
    =/  prod  [u.p.hed-res u.p.tal-res]
    =^  [pi=phash ^]  this  (index-cell prod)
    [[%& `prod] [%cons [si fi pi] `hed-zint `tal-zint]]^this
  ::
      [%0 axis=@]
    ?:  =(axis 0)  [%|^trace [%0 [si fi 0] ~]]^this
    =^  tups  this  (frag axis.f s)
    =^  *  this  (index-atom axis.f)
    :_  this
    [%& `p.tups]^[%0 [si fi pi.tups] path.tups]
  ::
      [%1 const=*]
    [[%& `const.f] [%1 [si fi hfi]]]^this
  ::
      [%2 sub=* for=*]
    =^  [=sub=res =sub=zint]  this
      $(f sub.f)
    ?:  ?=(%| -.sub-res)
      ~&  99  [%|^trace [%2 [si fi 0] `sub-zint ~ ~]]^this
    ?~  p.sub-res  [%&^~ [%2 [si fi 0] `sub-zint ~ ~]]^this
    =^  [=for=res =for=zint]  this
      $(f for.f)
    =/  hit=zint  [%2 [si fi 0] `sub-zint `for-zint ~]
    ?:  ?=(%| -.for-res)
      ~&  103  [%|^trace hit]^this
    ?~  p.for-res  [%&^~ hit]^this
    =^  [=prod=res =prod=zint]  this
      $(s u.p.sub-res, f u.p.for-res)
    ?:  ?=(%| -.prod-res)  [%|^trace [%2 [si fi 0] `sub-zint `for-zint ~]]^this
    ?~  p.prod-res  [prod-res [%2 [si fi 0] `sub-zint `for-zint ~]]^this
    =^  *  this  (index-cell +.f)
    =^  pi  this  (index-bun u.p.prod-res)
    [prod-res [%2 [si fi pi] `sub-zint `for-zint `prod-zint]]^this
  ::
      [%3 arg=*]
    :: TODO : out of gas errors
    ::        sf errors (sf itself errors or returns ~)
    =^  [=arg=res =arg=zint]  this
      $(f arg.f)
    ?:  ?=(%| -.arg-res)
      ~&  114  [%|^trace [%3 [si fi 0] ~]]^this
    ?~  p.arg-res  [%&^~ [%3 [si fi 0] `arg-zint]]^this
    =/  p  ?^(u.p.arg-res & |)
    =^  pi  this  (index-atom p)
    =^  *  this  (index-cell arg-res)
    :_  this
    :-  [%& ~ p]
    [%3 [si fi pi] `arg-zint]
    ::
      [%4 arg=*]
    :: TODO : out of gas errors
    ::        proper sf errors
    ::        error if sf returns a cell, use the hash-req struct e.g. `%cell^[-.u.p.arg-res +.u.p.arg-res]]
    ::        error if sf returns ~ (this is here but not sure if it is correct)
    =^  [=arg=res =arg=zint]  this
      $(f arg.f)
    ?:  ?=(%| -.arg-res)
      ~&  131  [%|^trace [%4 [si fi 0] ~]]^this
    ?~  p.arg-res  [%&^~ [%4 [si fi 0] `arg-zint]]^this
    ?^  u.p.arg-res
      =^  *  this  (index-cell u.p.arg-res)
      ~&  135  [%|^trace [%4 [si fi 0] ~]]^this
    =/  p  .+(u.p.arg-res)
    =^  pi  this  (index-atom p)
    :_  this
    :-  [%& ~ p]
    [%4 [si fi pi] `arg-zint]
    ::
      [%5 a=* b=*]
    =^  [=a=res =a=zint]  this
      $(f a.f)
    ?:  ?=(%| -.a-res)
      ~&  146  [%|^trace [%5 [si fi 0] ~ ~]]^this
    ?~  p.a-res  [%&^~ [%5 [si fi 0] ~ ~]]^this
    =^  [=b=res =b=zint]  this
      $(f b.f)
    ?:  ?=(%| -.b-res)
      ~&  150  [%|^trace [%5 [si fi 0] `a-zint `b-zint]]^this
    ?~  p.b-res  [%&^~ [%5 [si fi 0] `a-zint `b-zint]]^this
    =/  p   =(u.p.a-res u.p.b-res)
    =^  *  this  (index-cell +.f)
    =^  pi  this  (index-atom p)
    [[%& ~ p] [%5 [si fi pi] `a-zint `b-zint]]^this
  ::
  ::  6 is special
  ::  if [subject test] returns anything but 0 1, fail
  ::  so we never have to hash yes/no in that case, hence 2
      [%6 test=* yes=* no=*]
    =^  [=t=res =t=zint]  this
      $(f test.f)
    ?:  ?=(%| -.t-res)
      ~&  164  [%|^trace [%6 [si fi 0] `t-zint ~]]^this
    ?~  p.t-res  [%&^~ [%6 [si fi 0] `t-zint ~]]^this
    =^  [=sf2=res =sf2=zint]  this
      ?+  u.p.t-res  [%|^trace [%6 [si fi 0] `t-zint ~]]^this
        %&  $(f yes.f)
        %|  $(f no.f)
      ==
    ?:  ?=(%| -.sf2-res)
      ~&  164  [%|^trace [%6 [si fi 0] `sf2-zint ~]]^this
    ?~  p.sf2-res  [%&^~ [%6 [si fi 0] `sf2-zint ~]]^this
    =^  *  this  (index-cell +>.f) :: [yes no]
    =^  *  this  (index-bun yes.f) :: yes
    =^  *  this  (index-cell +.f)  :: [test yes no]
    =^  pi  this  (index-bun u.p.sf2-res)
    [[sf2-res [%6 [si fi pi] `t-zint `sf2-zint]]]^this
  ::
      [%7 subj=* next=*]
    =^  [=sub=res =sub=zint]  this
      $(f subj.f)
    ?:  ?=(%| -.sub-res)  ~&  179  [%|^trace [%7 [si fi 0] `sub-zint ~]]^this
    ?~  p.sub-res  [%&^~ [%7 [si fi 0] `sub-zint ~]]^this
    =^  subi  this  (index-bun u.p.sub-res)
    =^  [=nex=res =nex=zint]  this
      $(s u.p.sub-res, f next.f)
    ?:  ?=(%| -.nex-res)  [%|^trace [%7 [si fi 0] `sub-zint `nex-zint]]^this
    ?~  p.nex-res  [nex-res [%7 [si fi 0] `sub-zint `nex-zint]]^this
    =^  *  this  (index-cell +.f)
    =^  pi  this  (index-bun u.p.nex-res)
    [nex-res [%7 [si fi pi] `sub-zint `nex-zint]]^this
  ::
      [%8 hed=* next=*]
    =^  [=hed=res =hed=zint]  this
      $(f hed.f)
    ?:  ?=(%| -.hed-res)  ~&  198  [%|^trace [%8 [si fi 0] `hed-zint ~]]^this
    ?~  p.hed-res  [%&^~ [%8 [si fi 0] `hed-zint ~]]^this
    =^  [=nex=res =nex=zint]  this
      $(s [u.p.hed-res s], f next.f)
    ?:  ?=(%| -.nex-res)  ~&  198  [%|^trace [%8 [si fi 0] `hed-zint `nex-zint]]^this
    ?~  p.nex-res  [%&^~ [%8 [si fi 0] `hed-zint `nex-zint]]^this
    =^  *  this  (index-cell +.f)
    =^  pi  this  (index-bun u.p.nex-res)
    [nex-res [%8 [si fi pi] `hed-zint `nex-zint]]^this
  ::
      [%9 axis=@ core=*]
    ?:  =(axis 0)
      ~&  256  [%|^trace [%9 [si fi 0] ~ ~ 0 ~]]^this
    =^  [=core=res =core=zint]  this
      $(f core.f)
    ?:  ?=(%| -.core-res)
      ~&  211  [%|^trace [%9 [si fi 0] `core-zint ~ 0 ~]]^this
    ?~  p.core-res  [%|^trace [%9 [si fi 0] `core-zint ~ 0 ~]]^this
    =^  arm  this  (frag axis.f u.p.core-res)
    ?:  ?=(%| -.p.arm)
      ~&  269+[s axis.f]
      :_  this
      [%|^trace [%9 [si fi 0] `core-zint ~ `@ud`axis.f path.arm]]
    =^  [=res =res=zint]  this  $(s u.p.core-res, f p.arm)
    ?:  ?=(%| -.res)  [%|^trace [%9 [si fi 0] `core-zint `res-zint 0 ~]]^this
    ?~  p.res  [%&^~ [%9 [si fi 0] `core-zint `res-zint 0 ~]]^this
    =^  *  this  (index-atom axis.f)
    =^  *  this  (index-cell +.f)
    =^  *  this  (index-bun u.p.core-res)
    =^  pi  this  (index-bun u.p.res)
    [res [%9 [si fi pi] `core-zint `res-zint `@ud`axis.f path.arm]]^this
  ::
      [%10 [axis=@ value=*] target=*]
    ?:  =(0 axis.f)
      ~&  232  [%|^trace [%10 [si fi 0] ~ ~ 0 ~]]^this
    =^  [=val=res =val=zint]  this
      $(f value.f)
    ?:  ?=(%| -.val-res)
      ~&  239  [%|^trace [%10 [si fi 0] `val-zint ~ 0 ~]]^this
    ?~  p.val-res
      [%&^~ [%10 [si fi 0] `val-zint ~ 0 ~]]^this
    =^  [=tar=res =tar=zint]  this
      $(f target.f)
    ?:  ?=(%| -.tar-res)
      ~&  235
      :_  this
      :-  %|^trace
      [%10 [si fi 0] `val-zint `tar-zint 0 ~]
    ?~  p.tar-res
      :_  this
      :-  %&^~
      [%10 [si fi 0] `val-zint `tar-zint 0 ~]
    =^  ed  this
      (edit axis.f u.p.tar-res u.p.val-res)
    =^  *  this  (index-cell +.f)
    =^  *  this  (index-atom axis.f)
    =^  *  this  (index-cell +<.f) :: [axis value]
    :_  this
    :-  %&^`p.mut.ed
    :*  %10  [si fi i.mut.ed]  `val-zint
        `tar-zint  oldi.ed  path.ed
    ==
  ::
      [%11 tag=@ next=*]
    =^  itag  this  (index-bun tag.f)
    =^  [=next=res =next=zint]  this
      $(f next.f)
    ?:  ?=(%| -.next-res)  ~&  260  [%|^trace [%11 [si fi 0] `next-zint %|^itag]]^this
    ?~  p.next-res  [%&^~ [%11 [si fi 0] `next-zint %|^itag]]^this
    =/  p  .*(s [11 tag.f 1 u.p.next-res])
    =^  *  this  (index-cell +.f)
    =^  pi  this  (index-bun p)
    :_  this
    [%&^`p [%11 [si fi pi] `next-zint %|^itag]]
  ::
      [%11 [tag=@ clue=*] next=*]
    ::  look for jet with this tag and compute sample
    ~&  >  "hint: {<`@tas`tag.f>}"
    ~?  ?=(%zfast tag.f)
      ?>  ?=([[%1 jet] *] clue.f) :: todo: shouldn't crash here
      =-  "jet: {(sa:dejs:format -)}"
      (en-jet:enjs (jet-to-arm-list ->.clue.f))
    =^  itag  this  (index-bun tag.f)
    :: we can go straight to jetting in zere with this
    =^  [=clue=res =clue=zint]  this
      $(f clue.f)
    ?:  ?=(%| -.clue-res)
      ~&  269
      [%|^trace [%11 [si fi 0] ~ %&^[itag `clue-zint]]]^this
    ?~  p.clue-res  [%&^~ [%11 [si fi 0] ~ %&^[itag `clue-zint]]]^this
    ::  if jet exists for this tag, and sample is good,
    ::  replace execution with jet
    =^  [=next=res =next=zint]  this
      ?:  =(tag.f %zfast)
        :: todo: does this safe fail in zere? no it doesnt
        ?.  ?=([jet *] u.p.clue-res)
          [%|^trace [%11 [si fi 0] ~ %&^[itag `clue-zint]]]^this
        (run-jet +.clue.f u.p.clue-res)
      =?    trace
          ?=(?(%hunk %hand %lose %mean %spot) tag.f)
        [[tag.f u.p.clue-res] trace]
      $(f next.f)
    ?:  ?=(%| -.next-res)
      ~&  190
      [%|^trace [%11 [si fi 0] `next-zint %&^[itag `clue-zint]]]^this
    ?~  p.next-res  [%&^~ [%11 [si fi 0] `next-zint %&^[itag `clue-zint]]]^this
    =^  *  this  (index-cell +<.f)
    =^  *  this  (index-cell +.f)
    =^  pi  this  (index-bun u.p.next-res)
    :_  this
    :_  [%11 [si fi pi] `next-zint %&^[itag `clue-zint]]
    ?:  =(%fast tag.f)  %&^p.next-res
    :+  %&  ~
    .*  s
    [11 [tag.f 1 u.p.clue-res] 1 u.p.next-res]
  ::
  ==
  ::
  ++  index-atom
    |=  n=@
    ^-  [phash _this]
    =^  h  this  (hash n)
    =/  hit  (~(get by arena) h)
    ?:  ?=([~ [%cat *]] hit)
      [h this]
    :-  h
    this(arena (~(put by arena) n cat+n))
  ::
  ++  index-cell
    |=  n=^
    ^-  [[hn=phash hh=phash ht=phash] _this]
    =^  hn  this  (hash n)
    =/  hit  (~(get by arena) hn)
    ?:  ?=([~ [%pom *]] hit)
      [hn hh.u.hit ht.u.hit]^this
    =^  hh  this  (index-bun -.n)
    =^  ht  this  (index-bun +.n)
    :-  [hn hh ht]
    this(arena (~(put by arena) n pom+[hh ht]))
  ::
  ++  index-bun
    |=  n=*
    ^-  [phash _this]
    =^  h  this  (hash n)
    =?  arena  (~(has by arena) h)
      (~(put by arena) h %bun)
    :-  h
    this(arena (~(put by arena) n %bun))
  ::
  ++  edit
    |=  [axis=@ target=* value=*]
    ^-  [[mut=[p=* i=@ud] oldi=@ud path=(list (trel ?(%2 %3) @ud @ud))] _this]
    ?~  axis  !!
    =^  frg  this  (frag axis target)
    =/  mut  .*(target [10 [axis 1 value] 0 1])
    =^  frgmut  this  (frag axis mut)
    =/  efrg
      %+  turn  (zip path.frg path.frgmut)
      |=  [a=(pair ?(%2 %3) phash) b=(pair ?(%2 %3) phash)]
      ^-  (trel ?(%2 %3) phash phash)
      ?>  =(p.a p.b)
      [p.a q.a q.b]
    =^  muti  this  (index-bun mut)
    [mut^muti pi.frg efrg]^this
  ::
  ++  zip
    |*  [a=(list) b=(list)]
    ^-  (list _?>(?=(^ a) ?>(?=(^ b) [i.a i.b])))
    ?~  a  ~
    ?~  b  ~
    :-  [i.a i.b] 
    $(a t.a, b t.b)
  ::
  ++  hash
    |=  [n=*]
    |-  ^-  [phash _this]
    ::  test mode disables hashing, so it won't generate valid hints.
    ::  however, computation is *much* faster since hashing is the
    ::  most expensive aspect of the process.
    ::  zere needs hashes, s, and d regardless, but has a test mode
    ::  where hashes are unique, but fake. before merging into main, we
    ::  should add two test modes, one for uqbar one for zere
    =-  [h ->]
    |-  ^-  [[h=phash d=@ud s=@ud] _this]
    =/  mv  (~(get by cax) n)
    ?^  mv
      [u.mv this]
    ?@  n
      =^  h  ci  ?.(test-mode (hash:pedersen n 0)^ci ci^+(ci))
      =/  v  [h 1 1]
      :-  v
      this(cax (~(put by cax) n v))
    =^  vh  this  $(n -.n)
    =^  vt  this  $(n +.n)
    =^  h  ci  ?.(test-mode (hash:pedersen h.vh h.vt)^ci ci^+(ci))
    =/  v  [h +((add s.vh s.vt)) +((max d.vh d.vt))]
    :-  v
    this(cax (~(put by cax) n v))
  ::
  ++  frag
    |=  [axis=@ s=*]
    ::^-  [[* path] _this]
    =|  path=(list (pair ?(%2 %3) phash))
    =/  start-axis  axis
    =^  *  this  (index-bun s)
    =^  si  this  (index-bun s)
    |^  ^-  [[p=* pi=@ud path=_path] _this] :: TODO add crash axis?
    ?:  =(1 axis)
      [s si path]^this
    ?~  axis  !!
    ::?@  s  [%|^s^(gep-a start-axis axis) path]^this
    ?>  ?=(^ s)
    =/  pick  (cap axis)
    =^  pom  this  (index-cell s)
    =/  [c=* ci=@ud]  ?-(pick %2 -.s^hh.pom, %3 +.s^ht.pom)
    %_  $
      s     c
      si    ci
      axis  (mas axis)
      path  [[pick si] path]
    ==
    ::
    ::  solve for a in c = (peg a b)
    ++  gep-a
      |=  [p=@ b=@]
      =/  metb  (met 0 b)
      (rsh [0 (dec metb)] p)
    --
  ::
  ::
  ++  take-bud
    |=  amt=@ud
    ^-  [? _this]
    ?~  bud  %|^this
    ?:  (lth u.bud amt)  %&^this
    %|^this(u.bud (sub u.bud amt))
  ::
  ++  run-jet
    |=  [sam-clue=* =jet sam=*]
    ^-  [body _this]
    ?~  cost=(~(get by jets) jet)
      ~&  >>  "no jet found"  [%&^~ [%invalid ~]]^this
    ?:  ?&(?=(^ bud) (lth u.bud u.cost))  [%&^~ [%invalid ~]]^this
    ^-  [body _this]
    =^  sami  this  (index-bun sam)
    ?+    jet
      =-  [- [%jet (jet-to-arm-list jet) (num:enjs sami)]]^this
      =^  oob  this  (take-bud u.cost)
      ?:  oob
        %&^~
      ?~  res=(run-zuse-jet jet sam-clue sam)  %|^trace
      %&^res
   ::
   ::       [%$ %pedersen-hash]
   ::     =^  oob  this  (take-bud u.cost)
   ::     ?:  oob
   ::       [%&^~ [%jet jet ~]~]^this
   ::     ?.  ?=([@ @] sam)  [%|^trace ~]^this
   ::     [%&^(some (hash:pedersen sam)) ~]^this
   ::   ::
   ::       [%$ %pmug]
   ::     =^  hsam  this  (hash sam)
   ::     [%&^~^hsam [%jet jet (noun:enjs sam)]~]^this
   ::   ::
   ::       [%$ %pgor]
   ::     ?.  ?=([h=* t=*] sam)  [%|^trace ~]^this
   ::     =/  hit  [%jet jet (noun:enjs sam)]~
   ::     =^  res  this  (pgor sam)
   ::     ?~  res  [%|^trace hit]^this
   ::     [%&^res hit]^this
   ::   ::
   ::       [%$ %pmor]
   ::     ?.  ?=([h=* t=*] sam)  [%|^trace ~]^this
   ::     =/  hit  [%jet jet (noun:enjs sam)]~
   ::     =^  res  this  (pmor sam)
   ::     ?~  res  [%|^trace hit]^this
   ::     [%&^res hit]^this
   ::
        [%$ %zock]
      ?.  ?=([bud=(unit @) [s=* f=*] scry=*] sam)  [%|^trace [%invalid ~]]^this
      =^  *  this  ?~(bud.sam (index-atom bud.sam) (index-cell bud.sam))
      =^  *  this  ?~(bud.sam ~^this (index-atom u.bud.sam))
      =^  *  this  (index-bun scry.sam)
      =^  *  this  (index-cell +<.sam) :: [s f]
      =^  *  this  (index-cell +.sam)
      =/  inner-bud=(unit @ud)
        ?~  bud  bud.sam
        ?~  bud.sam  bud
        ?:  (lth u.bud u.bud.sam)  bud
        bud.sam
      =^  new-book=book  ci
        =<  [[- app] ci]
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
      =/  sub-hint  (zint:enjs q.p.new-book)
      =/  zock-hints  (pairs:enjs:format sam+(num:enjs sami) zint+sub-hint ~)
      :-  res^[%jet %$^%zock^~ zock-hints]
      %_    this
          cax  cax.q.new-book
          bud  outer-bud
      ==
    ==
  ::
  ++  jet-to-arm-list
    |=  jet=*
    ^-  (list @tas)
    =|  list=(list @tas)
    ?@  jet  jet^~
    ?>  ?=(@ -.jet)
    [`@tas`-.jet $(jet +.jet)]
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
  --
  ::
--
