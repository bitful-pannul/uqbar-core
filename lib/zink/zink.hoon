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
  |=  [s=* f=* test-mode=?]
  ^-  book
  =*  args  +<
  =<  =^(z this (memo q) [+<(q z) app])
  =<  $
  |_  *  ::  retarded atrocity to keep the feel of |^
  +*  this  .  :: but have virtual arms, and not lose type information of sample
      zink-loop  $ :: yeah idiomatically this is kind of awful
  ++  $  :: but i want to keep it as short and sweet as possible
  =+  args
  ^-  [body _this]
  :: we need to keep a separate memo for inside of zink
  :: put it in a =| at the top, only add to the appendix at the end
  :: =<  =^  [=p=res =p=zint]  this  +
  ::     =?  preds  ?=([%& ~ *] p-res)
  ::       (~(put by preds) s^f u.p.p-res)
  ::     [p-res p-zint]^this
  :: :-  .
  :: ^-  [body _this]
  :: =^  hs  this  (index-bun s)
  :: ?^  u=(~(get by preds) s f)
  ::   =^  hf  this  (index-bun f)
  ::   =^  hp  this  (index-bun u.u)
  ::   [%&^~^u.u [%memo hs hf hp]]^this
  =^  hs  this  (index-bun s)
  ?@  f
    =^  hf  this  (index-atom f)
    [%|^trace [%invalid ~]]^this
  =^  [hf=@ud hhf=@ud thf=@ud]  this  (index-cell f)
  ?+    f
    ?>  ?=(@ -.f)
    [%|^trace [%invalid ~]]^this
  ::
      [^ *]
    =^  [=hed=res =hed=zint]  this
      $(f -.f)
    ?:  ?=(%| -.hed-res)
      ~&  61  [%|^trace [%cons [hs hf 0] `hed-zint ~]]^this
    ?~  p.hed-res  [%&^~ [%cons [hs hf 0] `hed-zint ~]]^this
    =^  [=tal=res =tal=zint]  this
      $(f +.f)
    ?:  ?=(%| -.tal-res)
      ~&  65  [%|^trace [%cons [hs hf 0] `hed-zint `tal-zint]]^this
    ?~  p.tal-res  [%&^~ [%cons [hs hf 0] `hed-zint `tal-zint]]^this
    =/  prod  [u.p.hed-res u.p.tal-res]
    =^  [hp=phash ^]  this  (index-cell prod)
    [[%& `prod] [%cons [hs hf hp] `hed-zint `tal-zint]]^this
  ::
      [%0 axis=@]
    ?:  =(axis 0)  [%|^trace [%0 [hs hf 0] ~]]^this
    =^  tups  this  (frag axis.f s)
    =.  this  +:(index-atom axis.f)

    :_  this
    [%& `p.tups]^[%0 [hs hf hp.tups] path.tups]
  ::
      [%1 const=*]
    [[%& `const.f] [%1 [hs hf thf]]]^this
  ::
      [%2 sub=* for=*]
    =^  [=sub=res =sub=zint]  this
      $(f sub.f)
    ?:  ?=(%| -.sub-res)
      ~&  99  [%|^trace [%2 [hs hf 0] `sub-zint ~ ~]]^this
    ?~  p.sub-res  [%&^~ [%2 [hs hf 0] `sub-zint ~ ~]]^this
    =^  [=for=res =for=zint]  this
      $(f for.f)
    =/  hit=zint  [%2 [hs hf 0] `sub-zint `for-zint ~]
    ?:  ?=(%| -.for-res)
      ~&  103  [%|^trace hit]^this
    ?~  p.for-res  [%&^~ hit]^this
    =^  [=prod=res =prod=zint]  this
      $(s u.p.sub-res, f u.p.for-res)
    ?:  ?=(%| -.prod-res)  [%|^trace [%2 [hs hf 0] `sub-zint `for-zint ~]]^this
    ?~  p.prod-res  [prod-res [%2 [hs hf 0] `sub-zint `for-zint ~]]^this
    =.  this  +:(index-cell +.f)
    =^  hp  this  (index-bun u.p.prod-res)
    [prod-res [%2 [hs hf hp] `sub-zint `for-zint `prod-zint]]^this
  ::
      [%3 arg=*]
    :: TODO : out of gas errors
    ::        sf errors (sf itself errors or returns ~)
    =^  [=arg=res =arg=zint]  this
      $(f arg.f)
    ?:  ?=(%| -.arg-res)
      ~&  114  [%|^trace [%3 [hs hf 0] ~]]^this
    ?~  p.arg-res  [%&^~ [%3 [hs hf 0] `arg-zint]]^this
    =^  p  this  ?^(u.p.arg-res &^+:(index-cell u.p.arg-res) |^+:(index-atom u.p.arg-res))
    =^  hp  this  (index-atom p)
    :_  this
    :-  [%& ~ p]
    [%3 [hs hf hp] `arg-zint]
    ::
      [%4 arg=*]
    :: TODO : out of gas errors
    ::        proper sf errors
    ::        error if sf returns a cell, use the hash-req struct e.g. `%cell^[-.u.p.arg-res +.u.p.arg-res]]
    ::        error if sf returns ~ (this is here but not sure if it is correct)
    =^  [=arg=res =arg=zint]  this
      $(f arg.f)
    ?:  ?=(%| -.arg-res)
      ~&  131  [%|^trace [%4 [hs hf 0] ~]]^this
    ?~  p.arg-res  [%&^~ [%4 [hs hf 0] `arg-zint]]^this
    ?^  u.p.arg-res
      =.  this  +:(index-cell u.p.arg-res)
      ~&  135  [%|^trace [%4 [hs hf 0] ~]]^this
    =.  this  +:(index-atom u.p.arg-res)
    =/  p  .+(u.p.arg-res)
    =^  hp  this  (index-atom p)
    :_  this
    :-  [%& ~ p]
    [%4 [hs hf hp] `arg-zint]
    ::
      [%5 a=* b=*]
    =^  [=a=res =a=zint]  this
      $(f a.f)
    ?:  ?=(%| -.a-res)
      ~&  146  [%|^trace [%5 [hs hf 0] ~ ~]]^this
    ?~  p.a-res  [%&^~ [%5 [hs hf 0] ~ ~]]^this
    =^  [=b=res =b=zint]  this
      $(f b.f)
    ?:  ?=(%| -.b-res)
      ~&  150  [%|^trace [%5 [hs hf 0] `a-zint `b-zint]]^this
    ?~  p.b-res  [%&^~ [%5 [hs hf 0] `a-zint `b-zint]]^this
    =/  p   =(u.p.a-res u.p.b-res)
    =.  this  +:(index-cell +.f)
    =^  hp  this  (index-atom p)
    [[%& ~ p] [%5 [hs hf hp] `a-zint `b-zint]]^this
  ::
  ::  6 is special
  ::  if [subject test] returns anything but 0 1, fail
  ::  so we never have to hash yes/no in that case, hence 2
      [%6 test=* yes=* no=*]
    =^  [=t=res =t=zint]  this
      $(f test.f)
    ?:  ?=(%| -.t-res)
      ~&  164  [%|^trace [%6 [hs hf 0] `t-zint ~]]^this
    ?~  p.t-res  [%&^~ [%6 [hs hf 0] `t-zint ~]]^this
    =^  [=sf2=res =sf2=zint]  this
      ?+  u.p.t-res  [%|^trace [%6 [hs hf 0] `t-zint ~]]^this
        %&  $(f yes.f)
        %|  $(f no.f)
      ==
    ?:  ?=(%| -.sf2-res)
      ~&  164  [%|^trace [%6 [hs hf 0] `sf2-zint ~]]^this
    ?~  p.sf2-res  [%&^~ [%6 [hs hf 0] `sf2-zint ~]]^this
    =.  this  +:(index-cell +>.f) :: [yes no]
    =.  this  +:(index-bun yes.f) :: yes
    =.  this  +:(index-cell +.f)  :: [test yes no]
    =^  hp  this  (index-bun u.p.sf2-res)
    [[sf2-res [%6 [hs hf hp] `t-zint `sf2-zint]]]^this
  ::
      [%7 subj=* next=*]
    =^  [=sub=res =sub=zint]  this
      $(f subj.f)
    ?:  ?=(%| -.sub-res)  ~&  179  [%|^trace [%7 [hs hf 0] `sub-zint ~]]^this
    ?~  p.sub-res  [%&^~ [%7 [hs hf 0] `sub-zint ~]]^this
    =^  subi  this  (index-bun u.p.sub-res)
    =^  [=nex=res =nex=zint]  this
      $(s u.p.sub-res, f next.f)
    ?:  ?=(%| -.nex-res)  [%|^trace [%7 [hs hf 0] `sub-zint `nex-zint]]^this
    ?~  p.nex-res  [nex-res [%7 [hs hf 0] `sub-zint `nex-zint]]^this
    =.  this  +:(index-cell +.f)
    =^  hp  this  (index-bun u.p.nex-res)
    [nex-res [%7 [hs hf hp] `sub-zint `nex-zint]]^this
  ::
      [%8 hed=* next=*]
    =^  [=hed=res =hed=zint]  this
      $(f hed.f)
    ?:  ?=(%| -.hed-res)  ~&  198  [%|^trace [%8 [hs hf 0] `hed-zint ~]]^this
    ?~  p.hed-res  [%&^~ [%8 [hs hf 0] `hed-zint ~]]^this
    =/  next-s  [u.p.hed-res s] 
    =^  [=nex=res =nex=zint]  this
      $(s next-s, f next.f)
    ?:  ?=(%| -.nex-res)  ~&  198  [%|^trace [%8 [hs hf 0] `hed-zint `nex-zint]]^this
    ?~  p.nex-res  [%&^~ [%8 [hs hf 0] `hed-zint `nex-zint]]^this
    =.  this  +:(index-cell +.f)
    =.  this  +:(index-cell next-s)
    =^  hp  this  (index-bun u.p.nex-res)
    [nex-res [%8 [hs hf hp] `hed-zint `nex-zint]]^this
  ::
      [%9 axis=@ core=*]
    ?:  =(axis 0)
      ~&  256  [%|^trace [%9 [hs hf 0] ~ ~ 0 ~]]^this
    =^  [=core=res =core=zint]  this
      $(f core.f)
    ?:  ?=(%| -.core-res)
      ~&  211  [%|^trace [%9 [hs hf 0] `core-zint ~ 0 ~]]^this
    ?~  p.core-res  [%|^trace [%9 [hs hf 0] `core-zint ~ 0 ~]]^this
    =^  arm  this  (frag axis.f u.p.core-res)
    ?:  ?=(%| -.p.arm)
      ~&  269+[s axis.f]
      :_  this
      [%|^trace [%9 [hs hf 0] `core-zint ~ `@ud`axis.f path.arm]]
    =^  [=res =res=zint]  this  $(s u.p.core-res, f p.arm)
    ?:  ?=(%| -.res)  [%|^trace [%9 [hs hf 0] `core-zint `res-zint 0 ~]]^this
    ?~  p.res  [%&^~ [%9 [hs hf 0] `core-zint `res-zint 0 ~]]^this
    =.  this  +:(index-atom axis.f)
    =.  this  +:(index-cell +.f)
    =.  this  +:(index-bun u.p.core-res)
    =^  hp  this  (index-bun u.p.res)
    [res [%9 [hs hf hp] `core-zint `res-zint `@ud`axis.f path.arm]]^this
  ::
      [%10 [axis=@ value=*] target=*]
    ?:  =(0 axis.f)
      ~&  232  [%|^trace [%10 [hs hf 0] ~ ~ 0 ~]]^this
    =^  [=tar=res =tar=zint]  this
      $(f target.f)
    ?:  ?=(%| -.tar-res)
      ~&  239  [%|^trace [%10 [hs hf 0] ~ `tar-zint 0 ~]]^this
    ?~  p.tar-res
      [%&^~ [%10 [hs hf 0] ~ `tar-zint 0 ~]]^this
    =^  [=val=res =val=zint]  this
      $(f value.f)
    ?:  ?=(%| -.val-res)
      ~&  235
      :_  this
      :-  %|^trace
      [%10 [hs hf 0] `val-zint `tar-zint 0 ~]
    ?~  p.val-res
      :_  this
      :-  %&^~
      [%10 [hs hf 0] `val-zint `tar-zint 0 ~]
    =^  ed  this
      (edit axis.f u.p.tar-res u.p.val-res)
    =.  this  +:(index-cell +.f)
    =.  this  +:(index-atom axis.f)
    =.  this  +:(index-cell +<.f) :: [axis value]
    :_  this
    :-  %&^`p.mut.ed
    :*  %10  [hs hf i.mut.ed]  `val-zint
        `tar-zint  oldi.ed  path.ed
    ==
  ::
      [%11 tag=@ next=*]
    =^  itag  this  (index-bun tag.f)
    =^  [=next=res =next=zint]  this
      $(f next.f)
    ?:  ?=(%| -.next-res)  ~&  260  [%|^trace [%11 [hs hf 0] `next-zint %|^itag]]^this
    ?~  p.next-res  [%&^~ [%11 [hs hf 0] `next-zint %|^itag]]^this
    =/  p  .*(s [11 tag.f 1 u.p.next-res])
    =.  this  +:(index-cell +.f)
    =^  hp  this  (index-bun p)
    :_  this
    [%&^`p [%11 [hs hf hp] `next-zint %|^itag]]
  ::
      [%11 [tag=@ clue=*] next=*]
    ::  look for jet with this tag and compute sample
    ~&  >  "hint: {<`@tas`tag.f>}"
   ::  if jet exists for this tag, and sample is good,
    ::  replace execution with jet
    =.  this  +:(index-cell +.f)
    ?:  =(tag.f %zfast)
     (run-jet s f)
    =^  itag  this  (index-bun tag.f)
    =^  [=clue=res =clue=zint]  this
      $(f clue.f)
    ?:  ?=(%| -.clue-res)
      ~&  269
      [%|^trace [%11 [hs hf 0] ~ %&^[itag `clue-zint]]]^this
    ?~  p.clue-res  [%&^~ [%11 [hs hf 0] ~ %&^[itag `clue-zint]]]^this
    =^  [=next=res =next=zint]  this
      =?    trace
          ?=(?(%hunk %hand %lose %mean %spot) tag.f)
        [[tag.f u.p.clue-res] trace]
      $(f next.f)
    ?:  ?=(%| -.next-res)
      ~&  190
      [%|^trace [%11 [hs hf 0] `next-zint %&^[itag `clue-zint]]]^this
    ?~  p.next-res  [%&^~ [%11 [hs hf 0] `next-zint %&^[itag `clue-zint]]]^this
    =.  this  +:(index-cell +<.f)
    =.  this  +:(index-cell +.f)
    =^  hp  this  (index-bun u.p.next-res)
    :_  this
    :_  [%11 [hs hf hp] `next-zint %&^[itag `clue-zint]]
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
    =/  hit  (~(get by arena) n)
    ?:  ?=([~ [%cat *]] hit)
      [h this]
    :-  h
    this(arena (~(put by arena) n cat+n))
  ::
  ++  index-cell
    |=  n=^
    ^-  [[hn=phash hh=phash ht=phash] _this]
    =^  hn  this  (hash n)
    =/  hit  (~(get by arena) n)
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
    =?  arena  !(~(has by arena) n)
      (~(put by arena) n %bun)
    h^this
  :: ONLY USE FOR JETS
  :: even then, we should be more thoughtful
  :: what if we run out of gas in the middle
  :: of a turn?
  ++  index
    |=  n=*
    ^-  [phash _this]
    =^  h  this  (hash n)
    =/  hit  (~(get by arena) n)
    ?:  ?=([~ $<(%bun tnoun)] hit)  h^this
    ?@  n
      :-  h
      this(arena (~(put by arena) n cat+n))
    =^  hh  this  $(n -.n)
    =^  ht  this  $(n +.n)
    :-  h
    this(arena (~(put by arena) n pom+[hh ht]))
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
    =^  hmut  this  (index-bun mut)
    [mut^hmut hp.frg efrg]^this
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
    |^  |-  ^-  [phash _this]
    ::  test mode disables hashing, so it won't generate valid hints.
    ::  however, computation is *much* faster hsnce hashing is the
    ::  most expensive aspect of the process.
    ::  zere needs hashes, s, and d regardless, but has a test mode
    ::  where hashes are unique, but fake. before merging into master, we
    ::  should add two test modes, one for uqbar one for zere
    =-  =.  this  ->
        ?.  &(|(?=(^ n) (lth 12 n)) test-mode)  h^this
        =/  h  (prime-sham n)
        h^this(cax (~(jab by cax) n |=([* s=@ud d=@ud] h^s^d)))
    |-  ^-  [[h=phash s=@ud d=@ud] _this]
    =/  mv  (~(get by cax) n)
    ?^  mv
      [u.mv this]
    ?@  n
      =/  h  ?.(&((lth 12 n) test-mode) (hash:pedersen n 0) 0)
      =/  v  [h 1 0]
      :-  v
      this(cax (~(put by cax) n v))
    =^  vh  this  $(n -.n)
    =^  vt  this  $(n +.n)
    =/  h  ?.(test-mode (hash:pedersen h.vh h.vt) 0)
    =/  v  [h s=+((add s.vh s.vt)) d=+((max d.vh d.vt))]
    :-  v
    this(cax (~(put by cax) n v))
    ++  prime  `@`0v2.00000.00000.24000.00000.00000.00000.00000.00000.00000.00001
    ++  prime-sham
      |=  n=*
      ^-  phash
      =-  (mod - prime)
      ?@  n  (shas %atom n)
      (shas %cell (jam n))
    --
  ::
  ++  frag
    |=  [axis=@ s=*]
    ::^-  [[* path] _this]
    =|  path=(list (pair ?(%2 %3) phash))
    =/  start-axis  axis
    =.  this  +:(index-bun s)
    =^  hs  this  (index-bun s)
    |^  ^-  [[p=* hp=@ud path=_path] _this] :: TODO add crash axis?
    ?:  =(1 axis)
      [s hs path]^this
    ?~  axis  !!
    ::?@  s  [%|^s^(gep-a start-axis axis) path]^this
    ?>  ?=(^ s)
    =/  pick  (cap axis)
    =^  pom  this  (index-cell s)
    =/  [c=* hc=@ud]  ?-(pick %2 -.s^hh.pom, %3 +.s^ht.pom)
    %_  $
      s     c
      hs    hc
      axis  (mas axis)
      path  [[pick hs] path]
    ==
    ::
    ::  solve for a in c = (peg a b)
    ++  gep-a
      |=  [p=@ b=@]
      =/  metb  (met 0 b)
      (rsh [0 (dec metb)] p)
    --
  ++  memo
    |=  =zint
    =-  ?>(?=(^ -<) u.-<^->)
    =/  =uzint  `zint
    |^  ^-  [^uzint _this]
    ?~  uzint  uzint^this
    =*  zint  u.uzint
    =/  m=(unit pred)
       ?:  ?=(?(%1 %memo) -.zint)  `pred.zint
       ?:  ?=(?(%jet %invalid) -.zint)  ~
       `pred.zint
    ?~  m  down
    ?^  (~(get by preds) m)  [`[%memo u.m]]^this
    ?:  ?=(?(%1 %0 %invalid) -.u.uzint)  uzint^this
    down(preds (~(put by preds) [[s f] p]:u.m))
    ++  down
      ^-  [^uzint _this]
      ?>  ?=(^ uzint)
      =*  zint  u.uzint
      =-  [`-< this]
      ^-  [^^zint _this]
      ?+    -.zint  !!
          %jet  zint^this :: todo: add bud to pred?
          %2
        =^  sf1  this  $(uzint sf1.zint)
        =^  sf2  this  $(uzint sf2.zint)
        =^  sf3  this  $(uzint sf3.zint)
        :_  this
        zint(sf1 sf1, sf2 sf2, sf3 sf3)
          ?(%3 %4)
        =^  sf  this  $(uzint sf.zint)
        [zint(sf sf) this]
          ?(%5 %6 %7 %8 %cons)
        =^  sf1  this  $(uzint sf1.zint)
        =^  sf2  this  $(uzint sf2.zint)
        :_  this
        zint(sf1 sf1, sf2 sf2)
          ?(%9 %10)
        =^  sf1  this  $(uzint sf1.zint)
        =^  sf2  this  $(uzint sf2.zint)
        :_  this
        zint(sf1 sf1, sf2 sf2)
          %11
        =^  sf  this  $(uzint sf.zint)
        ?:  ?=(%| -.c.zint)  zint^this
        =^  clue  this  $(uzint clue.p.c.zint)
        :_  this
        zint(clue.p.c clue, sf sf)
      ==
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
    |=  [s=* f=[%11 [tag=@ clue=*] next=*]]
    ^-  [body _this]
    =^  hs  this  (index-bun s)
    =^  hf  this  (index-bun f)
    :: todo: does this safe fail in zere? no it doesnt
    ?.  ?=([[%1 =jet] *] clue.f)
      !! :: todo: dont crash
      ::  [%|^trace [%11 [hs hf 0] ~ %&^[itag `clue-zint]]]^this
    =/  =jet  jet.clue.f
    ~&  =-  "jet: {(sa:dejs:format -)}"
        (en-jet:enjs (jet-to-arm-list jet.clue.f)) 
    ?~  cost=(~(get by jets) jet)
      ~&  >>  "no jet found"  [%&^~ [%invalid ~]]^this
    ?:  ?&(?=(^ bud) (lth u.bud u.cost))  [%&^~ [%invalid ~]]^this
    ^-  [body _this]
    =^  [=clue=res *]  this  zink-loop(f clue.f)
    ?>  ?=([%& ~ *] clue-res)
    =/  sam  +:u.p.clue-res
    ?+    jet
      =/  res  (run-zuse-jet jet +.clue.f sam) 
      =/  hit  [%jet [hs hf hp=0] (jet-to-arm-list jet) ~]
      =.  this  +:(index sam)
      ?~  res  [%|^trace hit]^this
      =^  hp  this  (index u.res) :: again, dont necessarily need to alloc everything
      [%&^res hit(hp hp)]^this
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
      =.  this
        ?~  bud.sam
          +:(index-atom bud.sam)
        +:(index-cell:(index-atom u.bud.sam) bud.sam)
      =.  this  +:(index-bun scry.sam)
      =.  this  +:(index-cell +<.sam) :: [s f]
      =.  this  +:(index-cell +.sam)
      =/  inner-bud=(unit @ud)
        ?~  bud  bud.sam
        ?~  bud.sam  bud
        ?:  (lth u.bud u.bud.sam)  bud
        bud.sam
      =/  new-book=book
        =<  [- app]
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
      =.  this  this(bud outer-bud, cax cax.q.new-book, arena arena.q.new-book)
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
      =/  hit=$>(%jet zint)  [%jet [hs hf 0] %$^%zock^~ sub-hint]
      ?.  ?=([%& ^] res)
        [res hit]^this
      ~!  res
      =^  hp  this
        ?.  ?=([%0 * *] u.p.res)
          ?>  ?=(^ u.p.res)
          [hn +]:(index-cell u.p.res)
        =.  this  +:(index +<.u.p.res)
        =.  this  +:(index-cell +.u.p.res)
        [hn +]:(index-cell u.p.res) 
      :_  this
      res^hit(p.pred hp)
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
