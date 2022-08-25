/-  *zink
/+  *zink-pedersen, *zink-json
=>  |%
    +$  good      (unit *)
    +$  fail      (list [@ta *])
    +$  body      [p=(each good fail) q=(list cairo-hint)]
    +$  appendix  [cax=cache bud=@]
    +$  book      (pair body appendix)
    --
|%
++  zebra                                                 ::  bounded zk +mule
  |=  [bud=@ud cax=cache cax=bache scry=granary-scry [s=* f=*] test-mode=?]
  ^-  book
  %.  [s f scry test-mode]
  %*  .  zink
    app  [cax ~ bud]
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
      subject+s+(num:enjs hs)
      formula+s+(num:enjs hf)
  ==
::
++  order-hints
  |=  hit=hints
  =.  hit  (flop hit)
  =/  layers=(list hints)  ~[~]
  =/  depth  1
  |^  ^-  hints
  ?~  hit  (zing (flop layers))
  ?>  ?=(^ layers)
  %_  $
      hit    t.hit
      depth  depth.i.hit
      layers
    ^-  (list hints)
    ?:  =(depth.i.hit depth)
      [[i.hit i.layers] t.layers]
    ?:  =(+(depth.i.hit) depth)
      ?>  ?=(^ t.layers)
      [[i.hit (welp i.layers i.t.layers)] t.t.layers]
    (push-deeper-hint i.hit)
  ==
  ::
  ++  push-deeper-hint
    |=  hit=cairo-hint
    |-  ^-  (list hints)
    ?:  =(depth (dec depth.hit))
      [~[hit] layers]
    $(depth +(depth), layers [~ layers])
  ::
  ++  trim
    |=  [a=@ b=(list hints)]
    ^+  [p=b q=b]
    ?:  =(0 a)
      [~ b]
    ?>  ?=(^ b)
    =+  c=$(a (dec a), b t.b)
    [[i.b p.c] q.c]
  --
:: TODO: actually, might not need subf data structure
:: could check budget is at least 2 * number of subformula, because
:: in all cases will have hash(op, tail)
:: and tail will always need a hash to follow up (even w 0 and 4 an atom is hash(atom,0))
:: then check the arity of the subformula
++  zink
  =|  appendix
  =*  app  -
  =|  trace=fail
  =|  depth=@ud
  |=  [s=* f=* scry=granary-scry test-mode=?]
  ^-  book
  =-  -(q.p (flop q.p.-))
  |^
  ?+    f
    ~&  f
    [%|^trace app]
  ::
      [^ *]
    ?:  (lth 4)  [%&^~ [%cons [0 ~] [0 ~]]~]^app
    =^  hhed  app  (hash-subf -.f)
    =^  htal  app  (hash-subf +.f)
    ?:  =(bud 0)  [%&^~ [%cons [p.hhed ~] [p.htal ~]]~]^app
    =^  hed=body  app
      $(f -.f)
    ?:  ?=(%| -.p.hed)
      ~&  61  [%|^trace [%cons [p.hhed (flop q.hed)] [p.htal ~]]~]^app
    ?~  p.p.hed  [%&^~ [%cons [p.hhed (flop q.hed)] [p.htal ~]]~]^app
    =^  tal=body  app
      $(f +.f)
    =/  hit  [%cons [p.hhed (flop q.hed)] [p.htal (flop q.tal)]]~
    ?:  ?=(%| -.p.tal)
      ~&  65  [%|^trace hit]^app
    ?~  p.p.tal  [%&^~ hit]^app
    :_  app
    [%&~ u.p.p.hed^u.p.p.tal]^hit
  ::
      [%0 axis=@]
    ?:  =(axis 0)  [%|^~ [%0 0 %|^0 ~]]^app
    =^  hsibs=(pair (unit (each * atom)) path)
      app  (frag s axis.f)
    ?~  p.hsibs  [%&^~ [%0 axis.f %|^0 q.hsibs]~]
    ?:  ?=(%| -.u.p.hsibs)
      [%|^trace [%0 axis.f %|^p.u.p.hsibs q.hsibs]]
    =^  rh  app  (hash p.u.p.hsibs) :: this will always be a cache hit. dec?
    =/  hit  [%0 axis.f %&^rh q.hsibs]~
    ?:  =(bud 0)  [%&^~^ hit]^app
    :_  app
    [%& `u.u.part]^hit
  ::
      [%1 const=*]
    =^  hres  app  (hash const.f)
    ?:  =(bud 0)  [%&^~ [%1 hres] app]
    [[%& `const.f] [%1 hres]]^app
  ::
      [%2 sub=* for=*]
    ?:  (lth bud 4)  [&^~ [%2 [0 ~] [0 ~]]]^app
    =^  hsub  app  (hash-subf sub.f)
    =^  hfor  app  (hash-subf for.f)
    ?:  =(bud 0)  [&^~ [%2 [hsub ~] [hfor ~]]]^app
    =^  subject=body  app
      $(f sub.f)
    ?:  ?=(%| -.p.subject)
      ~&  99  [|^trace [%2 [hsub (flop q.subject)] [hfor ~]]~]^app
    ?~  p.subject  [&^~ [%2 [hsub (flop q.subject)] [hfor ~]]~]^app
    =^  formula=body  app
      $(f for.f)
    =/  hit  [%2 [hsub (flop q.subject)] [hfor (flop q.formula)]]
    ?:  ?=(%| -.formula)
      ~&  103  [|^trace hit ~]^app
    ?~  p.formula  [%&^~ hit ~]^app
    =-  -(q.p hit^q.p.-)
    %_  $
      s    u.p.subject
      f    u.p.formula
    ==
  ::
      [%3 arg=*]
    ?:  (lth bud 2)  [%&^~ [%3 [0 ~] ~]~]^app
    =^  harg  app  (hash-subf arg.f)
    ?:  =(bud 0)  [%&^~ [%3 [harg ~] ~]~]^app
    =^  argument=body  app
      $(f arg.f)
    ?:  ?=(%| -.argument)
      ~&  114  [%|^trace [%3 [harg (flop q.argument)] ~]~]^app
    ?~  p.argument  [%&^~ [%3 [harg (flop q.argument)] ~]~]^app
    ?@  u.p.argument
      :_  app
      :-  [%& ~ %.n]
      [%3 [harg (flop q.argument)] ~ %atom u.p.argument]~
    ::  should be cached. dec?
    =^  hhash  app  (hash -.u.p.argument)
    =^  thash  app  (hash +.u.p.argument)
    :_  app
    :-  [%& ~ %.y]
    [%3 [harg (flop q.argument)] ~ %cell u.hhash u.thash]~
  ::
      [%4 arg=*]
    ?:  (lth bud 2)  [%&^~ [%4 [0 ~] %&^0]~]^app
    =^  harg  app  (hash-subf arg.f)
    ?:  =(bud 0)  [%&^~ [%4 [harg ~] %&^0]~]^app
    =^  argument=body  app
      $(f arg.f)
    ?:  ?=(%| -.argument)
      ~&  131  [%|^trace [%4 [harg (flop q.argument)] %&^0]~]^app
    ?~  p.argument  [%&^~ [%4 [harg (flop q.argument)] %&^0]~]^app
    ?^  u.p.argument
      =+  arg=[(hash -.u.p.argument) (hash -.u.p.argument)]
      ~&  135  [%|^trace [%4 [harg (flop q.argument)] %|^arg]~]^app
    :_  app
    :-  [%& ~ .+(u.p.argument)]
    [%4 [harg (flop q.argument)] %&^u.p.argument]~
  ::
      [%5 a=* b=*]
    ?:  (lth bud 4)  [%&^~ [%5 [0 ~] [0 ~]]]^app
    =^  ha  app  (hash-subf a.f)
    =^  hb  app  (hash-subf b.f)
    ?:  =(bud 0)  [%&^~ [%5 [ha ~] [hb ~]]]^app
    =^  a=body  app
      $(f a.f)
    ?:  ?=(%| -.a)
      ~&  146  [%|^trace [%5 [ha (flop q.a)] [hb ~]]~]^app
    ?~  p.a  [%&^~ [%5 [ha (flop q.a)] [hb ~]]~]^app
    =^  b=body  app
      $(f b.f)
    =/  hit [%5 [ha (flop q.a)] [hb (flop q.b)]]~
    ?:  ?=(%| -.b)
      ~&  150  [%|^trace hit]^app
    ?~  p.b  [%&^~ hit]^app
    [[%& ~ =(u.p.a u.p.b)] hit]^app
  ::
  ::  6 is special
  ::  if [subject test] returns anything but 0 1, fail
  ::  so we never have to hash yes/no in that case, hence 2
      [%6 test=* yes=* no=*]
    ?:  (lth bud 2)  [%&^~ [%6 [0 ~] 0 0]~]^app
    =^  htest  app  (hash-subf test.f)
    =^  hyes   app  (hash-subf yes.f)
    =^  hno    app  (hash-subf no.f)
    ?:  =(bud 0)  [%&^~ [%6 [htest ~] hyes hno]~]^app
    =^  result=body  app
      $(f test.f)
    =/  hit  [%6 [htest (flop q.result)] hyes hno]
    ?:  ?=(%| -.result)
      ~&  164  [%|^trace hit ~]^app
    ?~  p.result  [%&^~ hit ~]^app
    =-  -(q.p hit^q.p.-)
    ?+  u.p.result  ~&  167  [%|^trace ~]^app
      %&  $(f yes.f)
      %|  $(f no.f)
    ==
  ::
      [%7 subj=* next=*]
    ?:  (lth bud 4)  [%&^~ [%7 [0 ~] [0 ~]]~]^app
    =^  hsubj  app  (hash-subf subj.f)
    =^  hnext  app  (hash-subf next.f)
    ?:  =(bud 0)  [%&^~ [%7 [hsubj ~] hnext]~]^app
    =^  subject=body  app
      $(f subj.f)
    =/  hit  [%7 [hsubj (flop q.subject)] hnext]
    ?:  ?=(%| -.subject)  ~&  179  [%|^trace hit ~]^app
    ?~  p.subject  [%&^~ hit ~]^app
    =-  -(q.p hit^q.p.-)
    %_  $
      s    u.p.subject
      f    next.f
    ==
  ::
      [%8 head=* next=*]
    ?:  (lth bud 4)  [%&~ [%8 [0 ~] 0]~]^app
    =^  hhead  app  (hash-subf head.f)
    =^  hnext  app  (hash-subf next.f)
    ?:  =(bud 0)  [%&~ [%8 [hhead ~] hnext]~]^app
    =^  head=body  app
      $(f head.f)
    =/  hit  [%8 [hhead (flop q.head)] hnext]
    ?:  ?=(%| -.head)  ~&  198  [%|^trace hit ~]^app
    ?~  p.head  [%&^~ hit ~]^app
    =-  -(q.p hit^q.p.-)
    %_  $
      s    [u.p.head s]
      f    next.f
      hit  [%8 u.hhead u.hnext]^hit
    ==
  ::
      [%9 axis=@ core=*]
    ?:  (lth bud 2)  [%&~ [%9 axis.f [0 ~] %|^0 ~]~]^app
    =^  hcore  app  (hash-subf core.f)
    ?:  =(bud 0)  [%&~ [%9 axis.f [hhead ~] %|^0 ~]~]^app
    =^  core=body  app
      $(f core.f)
    ?:  ?=(%| -.core)
      ~&  211  [%|^trace [%9 axis.f [hhead (flop q.core)] %|^0 ~]~]^app
    ?~  p.core  [%&^~ [%9 axis.f [hhead (flop q.core)] %|^0 ~]~]^app
    =^  arm  bud
      (frag axis.f u.p.core bud)
    ?~  p.arm  [%&^~ [%9 axis.f [hhead (flop q.core)] %|^0 q.arm]~]^app
    ?:  ?=(%| -.u.p.arm)
      :_  app
      [%|^trace [%9 axis.f [hhead (flop q.core)] %|^p.u.p.arm q.arm]~]
    =^  harm  app  (hash p.u.p.arm) :: this will always be a cache hit. dec?
    =/  hit  [%9 axis.f [hhead (flop q.core)] %&^harm q.arm]
    =-  .(p.q hit^p.q.-)
    %_  $
      s    p.u.p.arm
      f    u.u.arm
    ==
  ::
  ::  ten is special, if axis is invalid
  ::  target never needs to be hashed
      [%10 [axis=@ value=*] target=*]
    ?:  (lth bud 4)  [%&~ [%10 axis.f [0 ~] [0 ~] %|^0 ~]~]^app
    =^  hval  app  (hash-subf value.f)
    =^  htar  app  (hash-subf target.f)
    ?:  =(bud 0)  [%&~ [%10 axis.f [hval ~] [htar ~] %|^0 ~]~]^app
    ?:  =(0 axis.f)
      ~&  232  [%|^trace [%10 axis.f [hval ~] [htar ~] %|^0 ~]~]^app
    =^  value=body  app
      $(f value.f)
    ?:  ?=(%| -.value)
      ~&  239  [%|^trace [%10 axis.f [hval (flop q.value)] [htar ~] %|^0 ~]~]^app
    ?~  p.value
      [%&^~ [%10 axis.f [hval (flop q.value)] [htar ~] %|^0 ~]~]^app
    =^  target=body  app
      $(f target.f)
    ?:  ?=(%| -.target)
      ~&  235
      :_  app
      :-  %|^trace
      [%10 axis.f [hval (flop q.value)] [htar (flop q.target)] %|^0 ~]
    ?~  p.target
      :_  app
      :-  %&^~
      [%10 axis.f [hval (flop q.value)] [htar (flop q.target)] %|^0 ~]
    =^  mutant=(pair (unit (each (pair) atom)) (list phash))  app
      (edit axis.f u.p.target u.p.value)
    ?~  p.mutant
      :_  app
      :-  %&^~
      [%10 axis.f [hval (flop q.value)] [htar (flop q.target)] %|^0 q.mutant]
    ?:  ?=(%| -.p.mutant)
      :_  app
      :-  %|^trace
      :*  %10  axis.f  [hval (flop q.value)]
         [htar (flop q.target)]  %|^p.u.p.mutant q.mutant
      ==
    :_  app
    :-  %&^~^q.p.p.u.p.mutant  ::  im sorry
    :*  %10  axis.f  [hval (flop q.value)]
        [htar (flop q.target)]  %&^p.p.u.p.mutant q.mutant
    ==
  ::
       [%11 tag=@ next=*]
    =^  next=body  app
      $(f next.f)
    :_  app
    ?:  ?=(%| -.next)  ~&  260  %|^trace
    ?~  p.next  %&^~
    :+  %&  ~
    .*  s
    [11 tag.f 1 u.p.next]
  ::
      [%11 [tag=@ clue=*] next=*]
    ::  look for jet with this tag and compute sample
    ~&  >  "hint: {<`@tas`tag.f>}"
    ~?  ?=(%fast tag.f)
      ?>  ?=([@ @ [@ @] @] clue.f)
      "jet: {<`@tas`-.+.clue.f>}"
    =^  sam=body  app
      $(f clue.f)
    ?:  ?=(%| -.sam)  ~&  269  [%|^trace app]
    ?~  p.sam  [%&^~ [%0 ]]^app
    ::  if jet exists for this tag, and sample is good,
    ::  replace execution with jet
    =^  jax=body  app
      (jet tag.f u.p.sam)
    ?:  ?=(%| -.jax)  ~&  190  [%|^trace app]
    ?^  p.jax  [%& p.jax]^app
    ::  jet not found, proceed with normal computation
    =^  clue=body  app
      $(f clue.f)
    ?:  ?=(%| -.clue)  ~&  269  [%|^trace app]
    ?~  p.clue  [%&^~ [%0 ]]^app
    =^  next=body  app
      =?    trace
          ?=(?(%hunk %hand %lose %mean %spot) tag.f)
        [[tag.f u.p.clue] trace]
      $(f next.f)
    :_  app
    ?:  ?=(%| -.next)  ~&  286  %|^trace
    ?~  p.next  %&^~
    :+  %&  ~
    .*  s
    [11 [tag.f 1 u.p.clue] 1 u.p.next]
  ::
      [%12 ref=* path=*]
    ::  TODO hash ref, path and grain id parsed as last item in path
    ::       hash product and path through granary merkle tree
    ::       (similar process in nock 0)
    !!
    :: =^  href  app  (hash-subf target.f)
    :: ?:  =(bud 0)  [%&~ [%10 axis.f [hval ~] [htar ~] %|^0 ~]~]^app
    :: =^  ref=body  app
    ::   $(f ref.f)
    :: ?:  ?=(%| -.ref)     ~&  289  [%|^trace app]
    :: ?~  p.ref            [%&^~ [%0 ]]^app
    :: =^  path=body  app
    ::   $(f path.f)
    :: ?:  ?=(%| -.path)    ~&  293  [%|^trace app]
    :: ?~  p.path           [%&^~ [%0 ]]^app
    :: ?~  result=(scry p.ref p.path)
    ::   [%&^~^~ app]
    :: [%&^[~ `product.u.result] app]
  ==
  ::
  ++  jet
    |=  [tag=@ sam=*]
    ^-  book
    ?:  ?=(%slog tag)
      ::  print trace printfs?
      [%&^~ [%0 ]]^app
    ?:  ?=(%mean tag)
      ::  this is a crash..
      ~&  317  [%|^trace app]
    ?~  cost=(~(get by jets) tag)
      ~&  >>  "no jet found"  [%&^~ [%0 ]]^app
    ?:  (lth bud u.cost)  [%&^~ [%0 ]]^app
    :-  (run-jet tag sam u.cost)
    app(bud (sub bud u.cost))
  ::
  ++  run-jet
    |=  [tag=@ sam=* cost=@ud]
    ^-  body
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
    |^  ^-  [(pair (unit (each (pair) atom)) (list phash)) appendix]
    ?~  axis  !!
    =^  frg  app  (frag target)
    ?~  p.frg  [~^q.frg q.frg]^app
    ?:  ?=(%| -.p.frg)
      [%|^p.p.frg q.frg]^app
    |-
    ?~  q.frg  [%&^p.p.frg^value q.frg]^app
    ?>  ?=(^ r.frg)
    %=  $
      value  ?-(q.i.r.pick %2 [value p.i.r.frg], %3 [p.i.r.frg value])
      bud     (dec bud)
      path  t.ath
      ath  t.ath
    ==
    ::
    ++  frag
      |=  [s=* axis=@]
      =|  path=(list phash)
      =|  ath=(list (pair * ?(%2 %3)))
      |-  ^-  [(trel (unit (each * atom)) _path _ath) appendix]
      ?:  =(1 axis)
        [`%&^s ath path]^app
      ?~  axis  !!
      ?:  =(bud 0)  [~^path^ath]^app
      ?@  s  [`%|^s path ath]^app
      =/  pick  (cap axis)
      =^  sib  app
        %-  hash
        ?-(pick %2 +.s, %3 -.s)
      =/  child  ?-(pick %2 -.s, %3 +.s)
      %=  $
        s     child
        axis  (mas axis)
        path  [sib path]
        ath   [[child pick] ath]
        bud   ?:(=(bud 0) bud (dec bud))
      ==
    ::
    --
  ::
  ++  hash
    |=  [n=*]
    |-  ^-  [phash appendix]
    ::  test mode disables hashing, so it won't generate valid hints.
    ::  however, computation is *much* faster since hashing is the
    ::  most expensive aspect of the process.
    ?:  test-mode  [0x1 app]
    =-  ?~(-< [~ ->] [`p.u.-< ->])
    |-  ^-  [(pair phash @ud) appendix]
    ?^  mh
      [p.u.mh app]
    ?@  n
      =/  h  [(hash:pedersen n 0) 1]
      :-  h
      app(cax (~(put by cax) n h))
    =^  hh=(pair phash @ud)  app  $(n -.n)
    =^  ht=(pair phash @ud)  app  $(n +.n)
    =/  h  [(hash:pedersen p.p.hh p.p.ht) +((add q.u.hh q.u.ht))]
    :-  h
    app(cax (~(put by cax) n h))
  ::
      :: ?:  (lth 4)  [%&^~ [%cons [0 ~] [0 ~]]~]^app

      :: ?:  (lth 2)

      :: ?:  (lth bud 4)  [&^~ [%2 [0 ~] [0 ~]]]^app

      :: ?:  (lth bud 2)  [%&^~ [%3 [0 ~] ~]~]^app

      :: ?:  (lth bud 2)  [%&^~ [%4 [0 ~] %&^0]~]^app

      :: ?:  (lth bud 4)  [%&^~ [%5 [0 ~] [0 ~]]]^app

      :: ?:  (lth bud 2)  [%&^~ [%6 [0 ~] 0 0]~]^app

      :: ?:  (lth bud 4)  [%&^~ [%7 [0 ~] [0 ~]]~]^app

      :: ?:  (lth bud 4)  [%&^~ [%8 [0 ~] 0]~]^app

      :: ?:  (lth bud 2)  [%&^~ [%9 axis.f [0 ~] %|^0 ~]~]^app

      :: ?:  (lth bud 2)  [%&^~ [%10 axis.f [0 ~] [0 ~] %|^0 ~]~]^app

  ++  hash-form
    |=  f=*
    ^-  [=subf appendix]
    =,  hash-bud-hint
    =-  :_  app(bud
    ?~  p.-  [-.f ht]  [u.p.- ~]
    |^  ^-  (pair (unit [%unknown p]) @ud)
    ?+  f    [~ [%unknown -.f]]^1  :: todo: indirect atoms
        [^ *]
      =^  hit  app  (cell (cell hash hash) hash)
      [%cons hit]
        [%0 *]
      =^  hit  app  (atom +.f)
      [%0 hit]
        [%1 *]
      =^  h  app  (hash +.f)
      [%1 h]^app
    ::
        [%2 *]
      =^  hit  app  ((cell hash hash) +.f)
      [%2 hit]^app
    ::
        [%3 *]
      =^  app  hit  (hash +.f)
      [%3 hit]^app
    ::
        [%4 *]
      =^  app  hit  (atom +.f)
      [%4 hit]^app
    ::
        [%5 *]
      =^  app  hit  ((cell hash hash) +.f)
      [%5 hit]^app
    ::
        [%6 *]
      =^  app  hit  ((cell hash (cell hash hash)) +.f)
      [%6 hit]^app
    ::
        [%7 *]
      =^  app  hit  ((cell hash hash) +.f)
      [%7 hit]^app
    ::
        [%8 *]
      =^  app  hit  ((cell hash hash) +.f)
      [%8 hit]^app
    ::
        [%9 *]
      =^  app  hit  ((cell atom hash) +.f)
      [%9 hit]^app
    ::
        [%10 *]
      =^  app  hit  ((cell (cell atom hash) hash) +.f)
      [%10 hit]^app
    ::
        [%11 *]
      =^  app  hit  ((cell atom hash) +.f)
      [%11 hit]^app
    ::
        [%11 hit]
      =^  app  hit
        %.  -.f
        %+  cell
          |=  n=*
          ^-  (each _(atom) _((cell atom hash)))
          ?@  n  (atom n)
          ((cell atom hash) n)
        hash
      [%11 hit]^app
  ::
        [%12 *]
      =^  app  hit  ((cell hash hash) +.f)
      [%12 hit]^app
    ==
  :: builders
  ++  atom
    |=  n=*
    ^-  [^atom appendix]
    =^  h  app  (hash n)
    ?~  bud
      =^  h  app  (hash n)
      [%broke h]^app
    =.  bud  (bud (dec bud))
    ?@  n  [%atom n]^app
    =^  hh  app  (hash -.n)
    =^  ht  app  (hash +.n)
    [%cell hh ht]^app
  ::
  ++  cell
    |*  [hgat=$-(* [* appendix]) tgat=$(* [* appendix])]
    ^-  $-(* [(^cell _-:(hgat) _-:(tgat)) appendix])
    |=  n=*
    ?~  bud
      =^  h  app  (hash n)
      [%broke h]^app
    =.  app  (bud (dec bud))
    ?@  n  [%atom n]^app
    =^  hhit  app  (hgat -.n)
    =^  thit  app  (tgat +.n)
    [%cell hhit thit]^app
  ::
  --
  ++  hash-subf
    |=  f=*
    |-  ^-  subf-tree
    =^  hf  app  (hash f)
    ?+  f  !!
        [^ *]
      =^  hhed  app  $(f -.f)
      =^  htal  app  $(f +.f)
      :_  app
      :-  hf
      [%cell p.hhed p.ttal]
    ::
        [%0 axis=@]
      =^  h0  app  (hash 0)
      =^  ha  app  (hash axis)
      :_  app
      :-  hf
      [%cell [%cache h0] [%cache ha]]
    ::
        [%1 const=*]
      =^  h1  app  (hash 1)
      =^  hc  app  (hash const)
      :_  app
      :-1  hf
      [%cell [%cache h1] [%cache hc]]
    ::
        [%2 sub=* for=*]
      =^  h2    app  (hash 2)
      =^  hsub  app  (hash sub)
      =^  hfor  app  (hash for)
      :_  app
      :-  hf
      [%cell [%cache h2] [%cell [%cache hsub] [%cache hfor]]]
    ::
        [%3 arg=*]
      =^  h3  app  (hash 3)
      =^  ha  app  (hash arg)
      :_  app
      :-  hf
      [%cell [%cache h3] [%cache ha]]
    ::
        [%4 arg=*]
      =^  h4  app  (hash 4)
      =^  ha  app  (hash arg)
      :_  app
      :-  hf
      [%cell [%cache h4] [%cache ha]]
    ::
        [%5 a=* b=*]
      =^  h5  app  (hash 5)
      =^  ha  app  (hash a)
      =^  hb  app  (hash b)
      :_  app
      :-  hf
      [%cell [%cache h5] [%cell [%cache ha] [%cache hb]]]
    ::
        [%6 test=* yes=* no=*]
      =^  h6  app  (hash 6)
      :_  app
      :-  hf
      [%cell [%cache h6] [%cache ha]]
    ::
        [%7 subj=* next=*]
      =^  h7     app  (hash 7)
      =^  hsubj  app  (hash subj)
      =^  hnext  app  (hash next)
      :_  app
      :-  hf
      [%cell [%cache h7] [%cell [%cache hsubj] [%cache hnext]]]
    ::
        [%8 head=* next=*]
      =^  h8     app  (hash 8)
      =^  hhead  app  (hash head)
      =^  hnext  app  (hash next)
      :_  app
      :-  hf
      [%cell [%cache h8] [%cell [%cache hhead] [%cache hnext]]]
    ::
        [%9 axis=@ core=*]
      =^  h9     app  (hash 9)
      =^  haxis  app  (hash axis)
      =^  hcore  app  (hash core)
      :_  app
      :-  hf
      [%cell [%cache h9] [%cell [%cache haxis] [%cache hcore]]]
    ::
        [%10 [axis=@ value=*] target=*]
      =^  h10      app  (hash 10)
      =^  haxis    app  (hash axis)
      =^  hvalue   app  (hash value)
      =^  htarget  app  (hash target)
      :_  app
      :-  hf
      :+  %cell
        h10
      :+  %cell
        :+  %cell
          [%cache haxis]
        [%cache hvalue]
      [%cache htarget]
    ::
        [%11 tag=@ next=*]
      =^  h11     app  (hash 11)
      =^  htag    app  (hash tag)
      =^  hnext   app  (hash next)
      :_  app
      :-  hf
      [%cell [%cache h11] [%cell [%cache htag] [%cache hnext]]]
    ::
        [%11 [tag=@ clue=*] next=*]
      =^  h11      app  (hash 11)
      =^  htag     app  (hash tag)
      =^  hclue    app  (hash clue)
      =^  hnext    app  (hash next)
      :_  app
      :-  hf
      :+  %cell
        h11
      :+  %cell
        :+  %cell
          [%cache htag]
        [%cache hclue]
      [%cache hnext]
    ::
        [%12 ref=* path=*]
      =^  h12     app  (hash 12)
      =^  href    app  (hash ref)
      =^  hpath   app  (hash path)
      :_  app
      :-  hf
      [%cell [%cache h12] [%cell [%cache href] [%cache hpath]]]
    ::
    ==
  ::
  ++  hash-all-subf
    |=  n=*
    |^  ^-  [phash-tree appendix]
    ::  test mode disables hashing, so it won't generate valid hints.
    ::  however, computation is *much* faster since hashing is the
    ::  most expensive aspect of the process.
    ?:  test-mode  [[0x0 [%atom 0]] app]
    =-  ?~(-< [~ ->] [`p.u.-< ->])
    |-  ^-  [(pair phash-tree @ud) appendix]
    ?:  =(bud 0)  [~ app]
    ?@  n
      =/  h  [(hash:pedersen n 0) 1]
      =?  bud  ?!(=(bud 0))  (dec bud)
      :-  [h atom+val]
      app(cax (~(put by cax) n h), bud (dec bud))
    =^  hh=(pair phash-tree @ud)  app  $(n -.n)
    =^  ht=(pair phash-tree @ud)  app  $(n +.n)
    =/  h  [(hash:pedersen p.p.hh p.p.ht) +((add q.u.hh q.u.ht))]
    =?  bud  ?!(=(bud 0))  (dec bud)
    :-  [h cell+[q.hh q.ht]]
    app(cax (~(put by cax) n h), bud (dec bud))
  ::
  ++  frag
    |=  [s=* axis=@]
    =|  path=(list phash)
    |-  ^-  [(pair (unit (each * atom)) _path) appendix]
    ?:  =(1 axis)
      [`%&^s path]^app
    ?~  axis  !!
    ?:  =(bud 0)  [~^path]^app
    ?@  s  [`%|^s path]^app
    =/  pick  (cap axis)
    =^  sib  app
      %-  hash
      ?-(pick %2 +.s, %3 -.s)
    =/  child  ?-(pick %2 -.s, %3 +.s)
    %=  $
      s     child
      axis  (mas axis)
      path  [sib path]
      bud   ?:(=(bud 0) bud (dec bud))
    ==
  --
--
