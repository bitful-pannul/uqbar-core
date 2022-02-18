/+  *bink, *zig-sys-smart
|_  [validator-id=@ux town-id=@ud] :: now=time]
::
::  +mill-all: mills all calls in mempool
::
++  mill-all
  |=  [=town mempool=(list egg) blocknum=@ud]
  =/  pending
    %+  sort  mempool
    |=  [a=egg b=egg]
    (gth rate.p.a rate.p.b)
  =|  result=(list [@ux egg])
          ::  'chunk' def
  |-  ^-  [(list [@ux egg]) ^town]
  ?~  pending
    [result town]
  %_  $
    pending  t.pending
    result   [[`@ux`(shax (jam i.pending)) i.pending] result]
    town  (mill town i.pending blocknum)
  ==
::  +mill: processes a single call and returns updated town
::
++  mill
  |=  [=town =egg blocknum=@ud]
  ^-  ^town
  ?.  ?=(user from.p.egg)  town
  ?~  curr-nonce=(~(get by q.town) id.from.p.egg)
    town  ::  missing user
  ?.  =(nonce.from.p.egg +(u.curr-nonce))
    town  ::  bad nonce
  ?.  (~(audit tax p.town) egg)
    town  ::  can't afford gas
  =+  [gan rem]=(~(work farm p.town blocknum) egg)
  =/  fee=@ud   (sub budget.p.egg rem)
  :-  ?~  gan  (~(pay tax p.town) id.from.p.egg fee)
      (~(pay tax u.gan) id.from.p.egg fee)
  (~(put by q.town) id.from.p.egg nonce.from.p.egg)
::
::  +tax: manage payment for contract execution in zigs
::
++  tax
  |_  =granary
  +$  zigs-mold
    $:  total=@ud
        balances=(map id @ud)
        allowances=(map [owner=id sender=id] @ud)
        coinbase-rate=@ud
    ==
  ::  +audit: evaluate whether a caller can afford gas
  ++  audit
    |=  =egg
    ^-  ?
    ~&  >  "auditing"
    ?~  zigs=(~(get by granary) zigs-rice-id)        %.n
    ?.  ?=(%& -.germ.u.zigs)                         %.n
    =/  data  (hole zigs-mold data.p.germ.u.zigs)
    ~&  >>  balances.data
    ?.  ?=(user from.p.egg)                          %.n
    ?~  bal=(~(get by balances.data) id.from.p.egg)  %.n
    (gth u.bal (mul rate.p.egg budget.p.egg))
  ::  +pay: extract gas fee from caller's zigs balance
  ++  pay
    |=  [payee=id fee=@ud]
    ^-  ^granary
    ~&  >  "paying taxes"
    ?~  zigs=(~(get by granary) zigs-rice-id)  granary
    ?.  ?=(%& -.germ.u.zigs)                   granary
    =/  data  (hole zigs-mold data.p.germ.u.zigs)
    =.  balances.data
      %+  %~  jab  by
          ?.  (~(has by balances.data) validator-id)
            ::  make account if none in balances
            (~(put by balances.data) validator-id fee)
          ::  otherwise add to existing balance
          %+  ~(jab by balances.data)
            validator-id
          |=(bal=@ud (add bal fee))
        payee
      |=(bal=@ud (sub bal fee))
    =.  data.p.germ.u.zigs  data
    ~&  >>  balances.data
    (~(put by granary) zigs-rice-id u.zigs)
  --
::
::  +farm: execute a call to a contract within a wheat
::
++  farm
  |_  [=granary block=@ud]
  ::
  ++  work
    |=  =egg
    ^-  [(unit ^granary) @ud]
    =/  crop  (incubate egg(budget.p (div budget.p.egg rate.p.egg)))
    :_  +.crop
    ?~  -.crop  ~
    (harvest u.-.crop to.p.egg from.p.egg)
  ::
  ++  incubate
    |=  =egg
    ^-  [(unit male) @ud]
    |^
    =/  args  (cook q.egg)
    ?~  stalk=(germinate to.p.egg)
      `budget.p.egg
    (grow u.stalk args egg)
    ::  might move these out of farm to be used everywhere
    ::  also TODO fix mixed metaphor here
    ++  cook
      |=  =yolk
      ^-  scramble
      ?.  ?=(user caller.yolk)  !!
      :+  caller.yolk
        args.yolk
      %-  ~(gas by *(map id grain))
      %+  murn  ~(tap in grain-ids.yolk)
      |=  =id
      ?~  res=(~(get by granary) id)  ~
      ?.  ?=(%& -.germ.u.res)  ~
      ::  check that caller holds all input grain
      ?.  =(holder.u.res id.caller.yolk)  ~
      `[id u.res]
    ::
    ++  germinate
      |=  find=id
      ^-  (unit crop)
      ?~  gra=(~(get by granary) find)  ~
      ?.  ?=(%| -.germ.u.gra)  ~
      ?~  cont.p.germ.u.gra  ~
      `[(hole contract u.cont.p.germ.u.gra) owns.p.germ.u.gra]
    --
  ::
  ++  grow
    |=  [=crop =scramble =egg]
    ^-  [(unit male) @ud]
    |^
    =+  [chick rem]=(weed crop to.p.egg [%& scramble] ~ budget.p.egg)
    ~&  >  "1st weeding successful"
    ?~  chick  `rem
    ?:  ?=(%& -.u.chick)
      ::  male result, finished growing
      [`p.u.chick rem]
    ::  female result, continuation
    |-
    =*  next  next.p.u.chick
    =*  mem   mem.p.u.chick
    =^  child  rem
      (incubate egg(from.p to.p.egg, to.p to.next, budget.p rem, q args.next))
    ?~  child  `rem
    =/  gan  (harvest u.child to.p.egg from.p.egg)
    ?~  gan  `rem
    =.  granary  u.gan
    =^  eve  rem
      (weed crop to.p.egg [%| u.child] mem rem)
    ?~  eve  `rem
    ?:  ?=(%& -.u.eve)
      [`p.u.eve rem]
    %_  $
      next.p.u.chick  next.p.u.eve
      mem.p.u.chick   mem.p.u.eve
    ==
    ::
    ++  weed
      |=  [=^crop to=id inp=maybe-hatched mem=(unit vase) budget=@ud]
      ^-  [(unit chick) @ud]
      =/  owned
        %-  ~(gas by *(map id grain))
        %+  murn  ~(tap in owns.crop)
        |=  =id
        ?~  res=(~(get by granary) id)   ~
        ?.  =(holder.u.res to)  ~
        `[id u.res]
      =/  cart  [mem to block town-id -]
      =+  [res bud]=(barn contract.crop inp cart budget)
      ?~  res               `bud
      ?:  ?=(%| -.u.res)    `bud
      ?:  ?=(%& -.p.u.res)  `bud
      ::  write or event result
      [`p.p.u.res bud]
    ::
    ::  +barn: run contract formula with arguments and memory, bounded by bud
    ::  (takes yolk and runs write)
    ++  barn
      |=  [=contract inp=maybe-hatched =cart bud=@ud]
      ^-  [(unit (each (each * chick) (list tank))) @ud]
      |^
      ::  hellaciously ugly
      ?:  ?=(%| -.inp)
        ::  event
        =/  res  (event p.inp)
        ?~  -.res  `+.res
        ?:  ?=(%& -.u.-.res)
          [`[%& %| p.u.-.res] +.res]
        [`[%| p.u.-.res] +.res]
      ::  write
      =/  res  (write p.inp)
      ?~  -.res  `+.res
      ?:  ?=(%& -.u.-.res)
        [`[%& %| p.u.-.res] +.res]
      [`[%| p.u.-.res] +.res]
      ::  TODO read (scry)
      ::  =/  res  (read ;;(path +.args.p.inp))
      ::  ?~  -.res  `+.res
      ::  ?:  ?=(%& -.u.-.res)
      ::    [`[%& %& p.u.-.res] +.res]
      ::  [`[%| p.u.-.res] +.res]
      ::
      ::  note:  i believe the way we're using ;; here destroys
      ::  any trace data we may get out of the contract. the
      ::  output trace ends up resolving at the ;; rather than
      ::  wherever in the contract caused a stack trace.
      ::
      ::  using +mule here and charging no gas until jet dashboard for +bink
      ++  write
        |=  =^scramble
        ^-  [(unit (each chick (list tank))) @ud]
        ~&  >  "barn performing %write call"
        :_  bud
        `(mule |.(;;(chick (~(write contract cart) scramble))))
      ++  read
        |=  =path
        ^-  [(unit (each * (list tank))) @ud]
        ~&  >  "barn performing %read call"
        (bull |.((~(read contract cart) path)) bud)
      ++  event
        |=  =male
        ^-  [(unit (each chick (list tank))) @ud]
        ~&  >  "barn performing %event call"
        (bull |.(;;(chick (~(event contract cart) male))) bud)
      --
    --
  ::
  ++  harvest
    |=  [res=male lord=id from=caller]
    ^-  (unit ^granary)
    =-  ?.  -  ~
        ~&  >  "passed harvest checks"
        `(~(uni by granary) (~(uni by changed.res) issued.res))
    ?&  %-  ~(all in changed.res)
        |=  [=id =grain]
        ::  id in changed map must be equal to id in grain AND
        ::  all changed grains must already exist AND
        ::  no changed grains may also have been issued at same time AND
        ::  only grains that proclaim us lord may be changed
        ?&  =(id id.grain)
            (~(has by granary) id.grain)
            !(~(has by issued.res) id.grain)
            =(lord lord:(~(got by granary) id))
        ==
      ::
        %-  ~(all in issued.res)
        |=  [=id =grain]
        ::  id in issued map must be equal to id in grain AND
        ::  all newly issued grains must have properly-hashed id AND
        ::  lord of grain must be contract issuing it
        ?&  =(id id.grain)
            =((fry lord.grain town-id.grain germ.grain) id.grain)
            =(lord lord.grain)
    ==  ==
  --
--