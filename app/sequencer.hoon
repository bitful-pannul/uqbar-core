::  sequencer [UQ| DAO]
::
::  Agent for managing a single UQ| town. Publishes diffs to rollup.hoon
::  Accepts transactions and batches them periodically as moves to town.
::
/-  uqbar=zig-uqbar, w=zig-wallet
/+  default-agent, dbug, io=agentio,
    *zig-sequencer, *zig-rollup,
    zink=zink-zink, sig=zig-sig,
    engine=zig-sys-engine
::  Choose which library smart contracts are executed against here
::
/*  smart-lib-noun  %noun  /lib/zig/sys/smart-lib/noun
::  /*  zink-cax-noun   %noun  /lib/zig/sys/hash-cache/noun
|%
+$  card  card:agent:gall
+$  proposed-batch  [num=@ud =processed-txs =chain diff-hash=@ux root=@ux]
+$  state-1
  $:  %1
      rollup=(unit ship)      ::  replace in future with ETH contract address
      private-key=(unit @ux)  ::  our signing key
      town=(unit town)        ::  chain-state
      peer-roots=(map town=@ux root=@ux)  ::  track updates from rollup
      pending=mempool         ::  unexecuted transactions
      =memlist                ::  executed transactions in working state
      proposed-batch=(unit proposed-batch)   ::  stores working state
      status=?(%available %off)
      block-height-api-key=(unit @t)
  ==
+$  inflated-state-1  [state-1 =eng smart-lib-vase=vase]
+$  eng  $_  ~(engine engine !>(0) *(map * @) %.y %.n)  ::  sigs on, hints off
--
::
=|  inflated-state-1
=*  state  -
%-  agent:dbug
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  =/  smart-lib=vase  ;;(vase (cue +.+:;;([* * @] smart-lib-noun)))
  =-  `this(state [[%1 ~ ~ ~ ~ ~ ~ ~ %off ~] - smart-lib])
  %~  engine  engine
    ::  sigs on, hints off
  [smart-lib *(map * @) %.y %.n]
::
++  on-save  !>(-.state)
::
++  on-load
  |=  =old=vase
  ^-  (quip card _this)
  ::  on-load: pre-cue our compiled smart contract library
  =/  smart-lib=vase  ;;(vase (cue +.+:;;([* * @] smart-lib-noun)))
  =/  eng
    %~  engine  engine
      ::  sigs on, hints off
    [smart-lib *(map * @) %.y %.n]
  ?+  -.q.old-vase  `this(state [*state-1 eng smart-lib])
    %1  `this(state [!<(state-1 old-vase) eng smart-lib])
  ==
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?.  ?=([%indexer %updates ~] path)
    ~|("%sequencer: rejecting %watch on bad path" !!)
  ::  handle indexer watches here -- send nothing
  `this
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?.  ?=(%sequencer-town-action mark)
    ~|("%sequencer: error: got erroneous %poke" !!)
  =^  cards  state
    (handle-poke !<(town-action vase))
  [cards this]
  ::
  ++  handle-poke
    |=  act=town-action
    ^-  (quip card _state)
    ?-    -.act
    ::
    ::  town administration
    ::
        %init
      ?>  =(src.bowl our.bowl)
      ?.  =(%off status.state)
        ~|("%sequencer: already active" !!)
      ::  poke rollup ship with params of new town
      ::  (will be rejected if id is taken)
      =/  =chain  ?~(starting-state.act [~ ~] u.starting-state.act)
      =/  new-root  `@ux`(sham chain)
      =/  =^town
        :-  chain
        :*  town-id.act
            batch-num=0
            [address.act our.bowl]
            mode.act
            0x0  ~  ~
        ==
      =/  sig
        (ecdsa-raw-sign:secp256k1:secp:crypto `@uvI`new-root private-key.act)
      :_  %=  state
            rollup       `rollup-host.act
            private-key  `private-key.act
            town         `town
            status        %available
            proposed-batch  `[0 ~ chain.town 0x0 new-root]
          ==
      :~  %+  ~(watch pass:io /sub-rollup)
            [rollup-host.act %rollup]
          /peer-root-updates
        ::
          %+  ~(poke pass:io /batch-submit/(scot %ux new-root))
            [rollup-host.act %rollup]
          rollup-action+!>([%launch-town address.act sig town])
      ==
    ::
        %set-block-height-api-key
      ?>  =(src.bowl our.bowl)
      `state(block-height-api-key `key.act)
    ::
        %del-block-height-api-key
      ?>  =(src.bowl our.bowl)
      `state(block-height-api-key ~)
    ::
        %clear-state
      ?>  =(src.bowl our.bowl)
      ~&  >>>  "sequencer: wiping state"
      `[[%1 ~ ~ ~ ~ ~ ~ ~ %off ~] eng.state smart-lib-vase.state]
    ::
    ::  handle bridged assets from rollup
    ::
        %deposit
      ::  handle a transaction generated by rollup contract to bridge ETH
      ::  assets onto the town we're running. (TODO)
      ?>  =(src.bowl (need rollup.state))
      ?:  ?|  !=(%available status.state)
              ?=(~ town.state)
          ==
        ~|("%sequencer: error: got asset while not active" !!)
      ::  assert we haven't used this transaction hash as a deposit before
      ?<  (~(has in deposits.hall.u.town) hash.act)
      ~&  >>  "%sequencer: received assets from rollup: {<data.act>}"
      !!  ::  TODO -- this will be a special transaction type
    ::
    ::  transactions
    ::
        %receive
      ?.  =(%available status.state)
        ~|("%sequencer: error: got transaction while not active" !!)
      ::  configurable rate minimum
      ?.  (gte rate.gas.transaction.act 1)
        ~|("%sequencer: rejected transaction, gas rate too low" !!)
      ::  fetch latest ETH block height and perform batch
      ::  TODO inline thread
      =/  tid  `@ta`(cat 3 'run-single_' (scot %uv (sham eny.bowl)))
      =/  ta-now  `@ta`(scot %da now.bowl)
      =+  [`@ux`(sham +.transaction.act) src.bowl transaction.act]
      :_  state(pending (~(put by pending) -))
      :~  %+  ~(watch pass:io /run-single/[ta-now])
            [our.bowl %spider]
          /thread-result/[tid]
        ::
          %+  ~(poke pass:io /run-single/[ta-now])
            [our.bowl %spider]
          :-  %spider-start
          !>  :^  ~  `tid
                byk.bowl(r da+now.bowl)
              sequencer-get-block-height-etherscan+!>(block-height-api-key)
      ==
    ::
        %run-pending
      ?>  =(src.bowl our.bowl)
      ?:  =(~ pending)
        ~&  >  "%sequencer: no pending txns to run"
        `state
      ?~  town.state  ~|("%sequencer: error: no state" !!)
      =*  town  u.town.state
      =/  addr  p.sequencer.hall.town
      =/  =wallet-update:w
        .^  wallet-update:w  %gx
            %+  weld  /(scot %p our.bowl)/wallet/(scot %da now.bowl)
            /account/(scot %ux addr)/(scot %ux town-id.hall.town)/noun
        ==
      ?>  ?=(%account -.wallet-update)
      =/  new=state-transition
        %+  %~  run  eng
            :^    caller.wallet-update
                town-id.hall.town
              batch-num.hall.town
            eth-block-height.act
          ?~  proposed-batch.state
            chain.town
          chain.u.proposed-batch.state
        (sort-mempool:eng pending)
      =/  processed
        %+  turn  processed.new
        |=  [a=@ux b=transaction:smart c=output]
        [a b `c]
      =/  memlist-lent  (lent memlist)
      :_  %=    state
              pending         ~
              memlist         (weld memlist `^memlist`processed)
              proposed-batch  `[0 ~ chain.new 0x0 0x0]
          ==
      ^-  (list card)
      =<  p
      %^  spin  processed.new  0
      |=  [[=hash:smart t=transaction:smart op=output] i=@]
      :_  +(i)
      =/  signed-stuff  `@ux`(sham [t op])
      =/  usig
        %+  ecdsa-raw-sign:secp256k1:secp:crypto
          `@uvI`signed-stuff
        (need private-key.state)
      %+  ~(poke pass:io /receipt)
        [from:(~(got by pending) hash) %uqbar]
      :-  %uqbar-write
      !>  ^-  write:uqbar
      :+  %receipt  hash
      :+  (sign:sig our.bowl now.bowl signed-stuff)
        usig
      [(add memlist-lent i) t op]
    ::
    ::  batching
    ::
        %trigger-batch
      ?>  =(src.bowl our.bowl)
      ::  fetch latest ETH block height and perform batch
      ::  TODO inline thread
      =/  tid  `@ta`(cat 3 'make-batch_' (scot %uv (sham eny.bowl)))
      =/  ta-now  `@ta`(scot %da now.bowl)
      :_  state
      :~  %+  ~(watch pass:io /make-batch/[ta-now])
            [our.bowl %spider]
          /thread-result/[tid]
        ::
          %+  ~(poke pass:io /make-batch/[ta-now])
            [our.bowl %spider]
          :-  %spider-start
          !>  :^  ~  `tid
                byk.bowl(r da+now.bowl)
              sequencer-get-block-height-etherscan+!>(block-height-api-key)
      ==
    ::
        %perform-batch
      ?>  =(src.bowl our.bowl)
      ~&  %perform-batch
      ~>  %bout
      ?.  =(%available status.state)
        ~|("%sequencer: error: got poke while not active" !!)
      ?~  town.state
        ~|("%sequencer: error: no state" !!)
      ?~  rollup.state
        ~|("%sequencer: error: no known rollup host" !!)
      ?:  =(~ memlist)
        ~&  >  "%sequencer: ignoring batch trigger, no transactions"
        `state
      =*  town  u.town.state
      ?>  ?=(%full-publish -.mode.hall.town)
      ::  publish full diff data
      ::
      ::  produce diff and new state with engine
      ::
      =/  addr  p.sequencer.hall.town
      =/  =wallet-update:w
        .^  wallet-update:w  %gx
            %+  weld  /(scot %p our.bowl)/wallet/(scot %da now.bowl)
            /account/(scot %ux addr)/(scot %ux town-id.hall.town)/noun
        ==
      ?>  ?=(%account -.wallet-update)
      =/  new=state-transition
        %+  %~  run  eng
            :^    caller.wallet-update
                town-id.hall.town
              batch-num.hall.town
            eth-block-height.act
          chain.town
        memlist
      =/  new-root       `@ux`(sham chain.new)
      =/  diff-hash      `@ux`(sham ~[modified.new])
      =/  new-batch-num  +(batch-num.hall.town)
      ::  poke rollup
      ::
      :_  %=    state
              memlist  ~
              proposed-batch
            `[new-batch-num processed.new chain.new diff-hash new-root]
          ==
      :_  ~
      %+  ~(poke pass:io /batch-submit/(scot %ux new-root))
        [u.rollup.state %rollup]
      :-  %rollup-action
      !>  :-  %receive-batch
          :-  addr
          ^-  batch
          :*  town-id.hall.town
              new-batch-num
              mode.hall.town
              ~[modified.new]
              diff-hash
              new-root
              chain.new
              peer-roots.state
              ?~  private-key.state
                ~|("%sequencer: error: no signing key found" !!)
              %+  ecdsa-raw-sign:secp256k1:secp:crypto
                `@uvI`new-root
              u.private-key.state
    ==    ==
  --
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?+    wire  (on-agent:def wire sign)
      [%batch-submit @ ~]
    ?:  ?=(%poke-ack -.sign)
      ?~  p.sign
        ~&  >>  "%sequencer: batch approved by rollup"
        ?~  proposed-batch
          ~|("error: received batch approval without proposed batch" !!)
        =/  new-town=(unit ^town)
          (transition-state town u.proposed-batch)
        :_  this(town new-town, proposed-batch ~, memlist ~)
        :_  ~
        :^  %give  %fact
          ~[/indexer/updates]
        :-  %sequencer-indexer-update
        !>  ^-  indexer-update
        :^  %update  root.u.proposed-batch
          processed-txs.u.proposed-batch
        (need new-town)
      ::  TODO manage rejected moves here
      ~&  >>>  "%sequencer: our move was rejected by rollup!"
      ~&  u.p.sign
      `this(proposed-batch ~)
    `this
  ::
      [%sub-rollup ~]
    ?:  ?=(%kick -.sign)
      :_  this  ::  attempt to re-sub
      [%pass wire %agent [src.bowl %rollup] %watch (snip `path`wire)]~
    ?.  ?=(%fact -.sign)  `this
    =^  cards  state
      =/  upd  !<(town-update q.cage.sign)
      ?-    -.upd
          %new-peer-root
        ::  update our local map
        `state(peer-roots (~(put by peer-roots.state) town.upd root.upd))
      ::
          %new-sequencer
        ::  check if we have been kicked off our town
        ::  this is in place for later..  TODO expand this functionality
        ?~  town.state                          `state
        ?.  =(town.upd town-id.hall.u.town)     `state
        ?:  =(who.upd our.bowl)                 `state
        ~&  >>>  "%sequencer: we've been kicked out of town!"
        `state
      ==
    [cards this]
  ::
      [%make-batch @ ~]
    ?+    -.sign  (on-agent:def wire sign)
        %fact
      ?+    p.cage.sign  (on-agent:def wire sign)
          %thread-fail
        =/  err  !<  (pair term tang)  q.cage.sign
        %-  %+  slog
              leaf+"%sequencer: get-eth-block thread failed: {(trip p.err)}"
            q.err
        `this
          %thread-done
        =/  height=@ud  !<(@ud q.cage.sign)
        ~&  >  "eth-block-height: {<height>}"
        :_  this
        ::  get any last pending transactions, then make a batch.
        :~  %+  ~(poke pass:io /perform)
              [our.bowl %sequencer]
            sequencer-town-action+!>(`town-action`[%run-pending height])
            %+  ~(poke pass:io /perform)
              [our.bowl %sequencer]
            sequencer-town-action+!>(`town-action`[%perform-batch height])
        ==
      ==
    ==
  ::
      [%run-single @ ~]
    ?+    -.sign  (on-agent:def wire sign)
        %fact
      ?+    p.cage.sign  (on-agent:def wire sign)
          %thread-fail
        =/  err  !<  (pair term tang)  q.cage.sign
        %-  %+  slog
              leaf+"%sequencer: get-eth-block thread failed: {(trip p.err)}"
            q.err
        ::  NOW TRY AGAIN
        `this
          %thread-done
        =/  height=@ud  !<(@ud q.cage.sign)
        ~&  >  "eth-block-height: {<height>}"
        :_  this  :_  ~
        %+  ~(poke pass:io /perform)
          [our.bowl %sequencer]
        sequencer-town-action+!>(`town-action`[%run-pending height])
      ==
    ==
  ==
::
++  on-arvo
  |=  [=wire =sign-arvo:agent:gall]
  ^-  (quip card _this)
  (on-arvo:def wire sign-arvo)
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?.  =(%x -.path)  ~
  ?+    +.path  (on-peek:def path)
      [%status ~]
    ``noun+!>(status)
  ::
      [%town-id ~]
    ?~  town  ``noun+!>(~)
    ``noun+!>(`town-id.hall.u.town)
  ::
  ::  state reads fail if sequencer not active
  ::
      [%has @ ~]
    ::  see if grain exists in state
    =/  id  (slav %ux i.t.t.path)
    ?~  town  [~ ~]
    ``noun+!>((~(has by p.chain.u.town) id))
  ::
      [%all-data ~]
    ?~  town  [~ ~]
    =-  ``noun+!>(-)
    %+  murn  ~(tap in p.chain.u.town)
    |=  [=id:smart @ =item:smart]
    ?.  ?=(%& -.item)  ~
    `item
  ::
      [%grain @ ~]
    ?~  town  [~ ~]
    (read-grain t.path p.chain.u.town)
  ==
::
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
