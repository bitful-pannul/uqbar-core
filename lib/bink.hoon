~%  %bink-lib  ..part  ~
|%
::                                                        ::
+$  lone  $%  [%0 product=*]                              ::  +lone without scry
              [%2 trace=(list [@ta *])]
          ==
::                                                        ::
+$  loon  $%  [%0 p=*]                                    ::  success
              [%2 p=(list tank)]                          ::  stack trace
          ==
::                                                        ::
+$  bone  [$@(~ lone) rem=@ud]                            ::  bounded +lone
::                                                        ::
++  gas-cost
  |=  [a=* bud=@ud]
  =+  cost=0
  |-  ^-  (unit @ud)
  ?:  (gth cost bud)  ~
  ?@  a
    `(add cost (met 8 a))
  =/  left  $(a -.a)
  ?~  left  ~
  =.  cost  (add cost u.left)
  ?:  (gth cost bud)  ~
  =/  right  $(a +.a)
  ?~  right  ~
  =.  cost  (add cost u.right)
  ?:  (gte cost bud)  ~
  `+(cost)
::                                                        ::
++  jet-whitelist                                         ::  only these jets
  ^-  (set @tas)
  %-  ~(gas in *(set @tas))
  :~  %'a.50'                                             ::  XX tiny top-level
      %add    %apt    %bex  %by     %cad
      %can    %cat    %con  %cut    %dec
      %del    %dis    %div  %dor    %dvr
      %end    %flop   %get  %gor    %gte
      %gth    %has    %in   %lent   %lsh
      %lte    %lth    %met  %mix    %mod
      %mor    %mug    %mul  %on     %por
      %put    %reap   %rep  %rip    %rsh
      %slag   %snag   %sub  %swp    %welp
  ==
::                                                        ::
++  bink                                                  ::  bounded +mink
  ~/  %bink
  |=  $:  [subject=* formula=*]
          bud=@ud                                         ::  gas budget
      ==
  ~>  %bout                                               ::  XX remove: timing
  =|  trace=(list [@ta *])
  |^
  ?~  formula-cost=(gas-cost formula bud)
    [~ 0]
  =/  cos=@ud  u.formula-cost
  ?:  (lth bud cos)  [~ bud]
  =.  bud  (sub bud cos)
  |-
  ^-  bone
  ?+  formula  [%2 trace]^bud
      [^ *]
    =.  cos  1
    ?:  (lth bud cos)  [~ bud]
    =.  bud  (sub bud cos)
    =^  head  bud
      $(formula -.formula)
    ?~  head  [~ bud]
    ?.  ?=(%0 -.head)  head^bud
    =^  tail  bud
      $(formula +.formula)
    ?~  tail  [~ bud]
    ?.  ?=(%0 -.tail)  tail^bud
    [%0 product.head product.tail]^bud
  ::
      [%0 axis=@]
    =.  cos  1
    ?:  (lth bud cos)  [~ bud]
    =.  bud  (sub bud cos)
    =^  part  bud
      (frag axis.formula subject bud)
    ?~  part  [~ bud]
    ?~  u.part  [%2 trace]^bud
    [%0 u.u.part]^bud
  ::
      [%1 constant=*]
    =.  cos  1
    ?:  (lth bud cos)  [~ bud]
    =.  bud  (sub bud cos)
    [%0 constant.formula]^bud
  ::
      [%2 subject=* formula=*]
    =.  cos  1
    ?:  (lth bud cos)  [~ bud]
    =.  bud  (sub bud cos)
    =^  subject  bud
      $(formula subject.formula)
    ?~  subject  [~ bud]
    ?.  ?=(%0 -.subject)  subject^bud
    =^  formula  bud
      $(formula formula.formula)
    ?~  formula  [~ bud]
    ?.  ?=(%0 -.formula)  formula^bud
    %=  $
      subject  product.subject
      formula  product.formula
    ==
  ::
      [%3 argument=*]
    =.  cos  1
    ?:  (lth bud cos)  [~ bud]
    =.  bud  (sub bud cos)
    =^  argument  bud
      $(formula argument.formula)
    :_  bud
    ?~  argument  ~
    ?.  ?=(%0 -.argument)  argument
    [%0 .?(product.argument)]
  ::
      [%4 argument=*]
    =.  cos  1
    ?:  (lth bud cos)  [~ bud]
    =.  bud  (sub bud cos)
    =^  argument  bud
      $(formula argument.formula)
    ?~  argument  [~ bud]
    ?.  ?=(%0 -.argument)  argument^bud
    ?^  product.argument  [%2 trace]^bud
    ::  XX maybe we need a cache of computed gas costs
    =.  cos  %+  sub  (met 8 +(product.argument))
                      (met 8 product.argument)
    ?:  (lth bud cos)  [~ bud]
    :_  (sub bud cos)
    [%0 .+(product.argument)]
  ::
      [%5 a=* b=*]
    =.  cos  1
    ?:  (lth bud cos)  [~ bud]
    =.  bud  (sub bud cos)
    =^  a  bud
      $(formula a.formula)
    ?~  a  [~ bud]
    ?.  ?=(%0 -.a)  a^bud
    =^  b  bud
      $(formula b.formula)
    :_  bud
    ?~  b  ~
    ?.  ?=(%0 -.b)  b
    [%0 =(product.a product.b)]
  ::
      [%6 test=* yes=* no=*]
    =.  cos  1
    ?:  (lth bud cos)  [~ bud]
    =.  bud  (sub bud cos)
    =^  result  bud
      $(formula test.formula)
    ?~  result  [~ bud]
    ?.  ?=(%0 -.result)  result^bud
    ?+  product.result  [%2 trace]^bud
      %&  $(formula yes.formula)
      %|  $(formula no.formula)
    ==
  ::
      [%7 subject=* next=*]
    =.  cos  1
    ?:  (lth bud cos)  [~ bud]
    =.  bud  (sub bud cos)
    =^  subject  bud
      $(formula subject.formula)
    ?~  subject  [~ bud]
    ?.  ?=(%0 -.subject)  subject^bud
    %=  $
      subject  product.subject
      formula  next.formula
    ==
  ::
      [%8 head=* next=*]
    =.  cos  1
    ?:  (lth bud cos)  [~ bud]
    =.  bud  (sub bud cos)
    =^  head  bud
      $(formula head.formula)
    ?~  head  [~ bud]
    ?.  ?=(%0 -.head)  head^bud
    %=  $
      subject  [product.head subject]
      formula  next.formula
    ==
  ::
      [%9 axis=@ core=*]
    =.  cos  1
    ?:  (lth bud cos)  [~ bud]
    =.  bud  (sub bud cos)
    =^  core  bud
      $(formula core.formula)
    ?~  core  [~ bud]
    ?.  ?=(%0 -.core)  core^bud
    =^  arm  bud
      (frag axis.formula product.core bud)
    ?~  arm  [~ bud]
    ?~  u.arm  [%2 trace]^bud
    %=  $
      subject  product.core
      formula  u.u.arm
    ==
  ::
      [%10 [axis=@ value=*] target=*]
    =.  cos  1
    ?:  (lth bud cos)  [~ bud]
    =.  bud  (sub bud cos)
    ?:  =(0 axis.formula)  [%2 trace]^bud
    =^  target  bud
      $(formula target.formula)
    ?~  target  [~ bud]
    ?.  ?=(%0 -.target)  target^bud
    =^  value  bud
      $(formula value.formula)
    ?~  value  [~ bud]
    ?.  ?=(%0 -.value)  value^bud
    =^  mutant=(unit (unit *))  bud
      (edit axis.formula product.target product.value bud)
    :_  bud
    ?~  mutant  ~
    ?~  u.mutant  [%2 trace]
    [%0 u.u.mutant]
  ::
      [%11 [tag=@ clue=*] next=*]
    ::  XX change gas cost if jet changes atom size
    =.  cos  1
    ?:  (lth bud cos)  [~ bud]
    =.  bud  (sub bud cos)
    ?.  (~(has in (silt `(list @)`~[%spot %mean %fast])) tag.formula)
      ::  TODO: put something in the trace
      [%2 trace]^bud
    =^  clue  bud
      $(formula clue.formula)
    ?~  clue  [~ bud]
    ?.  ?=(%0 -.clue)  clue^bud
    =?    trace
        ?=(?(%hunk %hand %lose %mean %spot) tag.formula)
      [[tag.formula product.clue] trace]
    =^  next  bud
      $(formula next.formula)
    :_  bud
    ?~  next  ~
    ?.  ?=(%0 -.next)  next
    ?.  ?|  !=(%fast tag.formula)
            ?&  ?=([@ *] product.clue)
                (~(has in jet-whitelist) -.product.clue)
        ==  ==
      [%2 trace]
    :-  %0
    .*  subject
    [11 [tag.formula 1 product.clue] 1 product.next]
  ::
  ::  [%11 tag=@ next=*]
  ::
  ::  [%12 ref=* path=*]
  ::
  ==
  ::
  ++  frag
    |=  [axis=@ noun=* bud=@ud]
    ^-  [(unit (unit)) @ud]
    ?:  =(0 axis)  [`~ bud]
    |-  ^-  [(unit (unit)) @ud]
    ?:  =(0 bud)  [~ bud]
    ?:  =(1 axis)  [``noun (dec bud)]
    ?@  noun  [`~ (dec bud)]
    =/  pick  (cap axis)
    %=  $
      axis  (mas axis)
      noun  ?-(pick %2 -.noun, %3 +.noun)
      bud   (dec bud)
    ==
  ::
  ++  edit
    |=  [axis=@ target=* value=* bud=@ud]
    ^-  [(unit (unit)) @ud]
    ?:  =(1 axis)  [``value bud]
    ?@  target  [`~ bud]
    ?:  =(0 bud)  [~ bud]
    =/  pick  (cap axis)
    =^  mutant  bud
      %=  $
        axis    (mas axis)
        target  ?-(pick %2 -.target, %3 +.target)
        bud     (dec bud)
      ==
    ?~  mutant  [~ bud]
    ?~  u.mutant  [`~ bud]
    ?-  pick
      %2  [``[u.u.mutant +.target] bud]
      %3  [``[-.target u.u.mutant] bud]
    ==
  --
::                                                        ::
++  book                                                  ::  bounded +mook
  |=  ton=lone
  ^-  loon
  ?.  ?=([%2 *] ton)
    ton
  |^  [%2 (turn skip rend)]
  ::
  ::  TODO: run +bink on a read call
  ++  skip
    ^+  trace.ton
    =/  yel  (lent trace.ton)
    ?.  (gth yel 1.024)  trace.ton
    %+  weld
      (scag 512 trace.ton)
    ^+  trace.ton
    :_  (slag (sub yel 512) trace.ton)
    :-  %lose
    (crip "[skipped {(scow %ud (sub yel 1.024))} frames]")
  ::
  ++  rend
    |=  [tag=@ta dat=*]
    ^-  tank
    ?+    tag
    ::
      leaf+"mook.{(rip 3 tag)}"
    ::
        %hunk
      ?@  dat  leaf+"mook.hunk"
      =/  sof=(unit path)  ((soft path) +.dat)
      ?~  sof  leaf+"mook.hunk"
      (smyt u.sof)
    ::
        %lose
      ?^  dat  leaf+"mook.lose"
      leaf+(rip 3 dat)
    ::
        %hand
      leaf+(scow %p (mug dat))
    ::
        %mean
      ?@  dat  leaf+(rip 3 dat)
      =/  mac  (mack dat -.dat)
      ?~  mac  leaf+"####"
      =/  sof  ((soft tank) u.mac)
      ?~  sof  leaf+"mook.mean"
      u.sof
    ::
        %spot
      =/  sof=(unit spot)  ((soft spot) dat)
      ?~  sof  leaf+"mook.spot"
      :+  %rose  [":" ~ ~]
      :~  (smyt p.u.sof)
          =*  l   p.q.u.sof
          =*  r   q.q.u.sof
          =/  ud  |=(a=@u (scow %ud a))
          leaf+"<[{(ud p.l)} {(ud q.l)}].[{(ud p.r)} {(ud q.r)}]>"
      ==
    ==
  --
::                                                        ::
++  bock                                                  ::  raw virtual nock
  |=  [[sub=* fol=*] bud=@ud]
  ^-  [(unit loon) @ud]
  =/  =bone  (bink [sub fol] bud)
  :_  rem.bone
  ?~  -.bone  ~
  `(book -.bone)
::                                                        ::
++  brute                                                 ::  untyped +bock
  |=  [tap=(trap) bud=@ud]
  ^-  [(unit (each * (list tank))) @ud]
  =+  [ton rem]=(bock [tap %9 2 %0 1] bud)
  :_  rem
  ?~  ton  ~
  :-  ~
  ?-  -.u.ton
    %0  [%& p.u.ton]
    %2  [%| p.u.ton]
  ==
::                                                        ::
++  blue                                                  ::  typed +bock
  |*  [tap=(trap) bud=@ud]
  ::^-  [(unit (each $:tap (list tank))) @ud]
  =+  [mud rem]=(brute tap bud)
  :_  rem
  ?~  mud  ~
  ?-  -.u.mud
    %&  [%& p=$:tap]
    %|  [%| p=p.u.mud]
  ==
--
