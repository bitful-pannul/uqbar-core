::  sequencer [UQ| DAO]
::
::  Agent for managing a single UQ| town. Publishes diffs to rollup.hoon
::  Accepts transactions and batches them periodically as moves to town.
::
/+  *sequencer, *rollup, default-agent, dbug, verb
::  Choose which library smart contracts are executed against here
::
/*  smart-lib-noun  %noun  /lib/zig/compiled/smart-lib/noun
|%
+$  card  card:agent:gall
+$  state-0
  $:  %0
      rollup=(unit ship)  ::  replace in future with ETH/starknet contract address
      private-key=(unit @ux)
      town=(unit town)                ::  state
      =basket                         ::  mempool
      peer-roots=(map id:smart root=@ux)  ::  track updates from rollup
      proposed-batch=(unit [=basket =land diff-hash=@ux root=@ux])
      status=?(%available %off)
  ==
+$  inflated-state-0  [state-0 smart-lib-vase=vase]
--
::
=|  inflated-state-0
=*  state  -
%-  agent:dbug
%+  verb  |
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  `this(state [[%0 ~ ~ ~ ~ ~ ~ %off] ;;(vase (cue q.q.smart-lib-noun))])
++  on-save  !>(-.state)
++  on-load
  |=  =old=vase
  ^-  (quip card _this)
  ::  on-load: pre-cue our compiled smart contract library
  ::
  `this(state [!<(state-0 old-vase) ;;(vase (cue q.q.smart-lib-noun))])
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?.  =(%available status.state)
    ~|("%sequencer: error: got watch while not active" !!)
  ?>  (allowed-participant [src our now]:bowl)
  ?.  ?=([%indexer %updates ~] path)
    ~|("%sequencer: rejecting %watch on bad path" !!)
  ::  handle indexer watches here -- send latest state
  ?~  town  `this
  :_  this
  =-  [%give %fact ~ %indexer-update -]~
  !>(`indexer-update`[%update ~ u.town (rear roots.hall.u.town)])
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?.  ?=(%sequencer-town-action mark)
    ~|("%sequencer: error: got erroneous %poke" !!)
  ?>  (allowed-participant [src our now]:bowl)
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
      =/  =land  ?~(starting-state.act [~ ~] u.starting-state.act)
      =/  new-root  (shax (jam land))
      =/  =^town
        :-  land
        :*  town-id.act
            [address.act our.bowl]
            mode.act
            0x0
            [new-root]~
        ==
      =/  sig
        (ecdsa-raw-sign:secp256k1:secp:crypto new-root private-key.act)
      :_  %=  state
            rollup       `rollup-host.act
            private-key  `private-key.act
            town         `town
            status        %available
            proposed-batch  `[~ land.town 0x0 new-root]
          ==
      :~  [%pass /sub-rollup %agent [rollup-host.act %rollup] %watch /peer-root-updates]
          =+  [%rollup-action !>([%launch-town address.act sig town])]
          [%pass /batch-submit/(scot %ux new-root) %agent [rollup-host.act %rollup] %poke -]
      ==
    ::
        %clear-state
      ?>  =(src.bowl our.bowl)
      ~&  >>  "sequencer: wiping state"
      `state(rollup ~, private-key ~, town ~, basket ~, peer-roots ~, status %off)
    ::
    ::  handle transactions
    ::
        %receive-assets
      ::  uncritically absorb assets bridged from rollup
      ?>  =(src.bowl (need rollup.state))
      ?.  =(%available status.state)
        ~|("%sequencer: error: got asset while not active" !!)
      ?~  town.state  !!
      ~&  >>  "%sequencer: received assets from rollup: {<assets.act>}"
      `state(town `u.town(p.land (~(uni by p.land.u.town.state) assets.act)))

    ::
        %receive
      ?.  =(%available status.state)
        ~|("%sequencer: error: got egg while not active" !!)
      =-  `state(basket (~(uni in basket) -))
      ^+  basket
      %-  ~(run in eggs.act)
      |=  =egg:smart
      [`@ux`(shax (jam q.egg)) egg]
    ::
    ::  batching
    ::
        %trigger-batch
      ?>  =(src.bowl our.bowl)
      ?.  =(%available status.state)
        ~|("%sequencer: error: got poke while not active" !!)
      ?~  town.state
        ~|("%sequencer: error: no state" !!)
      ?~  rollup.state
        ~|("%sequencer: error: no known rollup host" !!)
      ?~  basket.state
        ~|("%sequencer: error: no transactions to include in batch" !!)
      =*  town  u.town.state
      ?:  ?=(%committee -.mode.hall.town)
        ::  TODO data-availability committee
        ::
        ~|("%sequencer: error: DAC not implemented" !!)
      ::  publish full diff data
      ::
      ::  1. produce diff and new state with mill
      ::  TODO: adjust what mill returns to get diff for submission
      =/  addr  p.sequencer.hall.town
      =/  mil  ~(mill mill smart-lib-vase)
      =+  /(scot %p our.bowl)/wallet/(scot %da now.bowl)/account/(scot %ux addr)/(scot %ud id.hall.town)/noun
      =+  .^(account:smart %gx -)
      =/  new-state=[(list [@ux egg:smart]) =land]
        %+  ~(mill-all mil - `@ud`id.hall.town 0)
          land.town
        (turn ~(tap in `^basket`basket.state) tail)
      =/  new-root      (shax (jam land.new-state))
      =/  state-diffs  *(list diff)
      =/  diff-hash     (shax (jam state-diffs))
      ::  2. generate our signature
      ::  (address sig, that is)
      ?~  private-key.state
        ~|("%sequencer: error: no signing key found" !!)
      =/  sig
        (ecdsa-raw-sign:secp256k1:secp:crypto new-root u.private-key.state)
      ::  3. poke rollup
      :_  state(proposed-batch `[basket.state +.new-state diff-hash new-root], basket ~)
      =-  [%pass /batch-submit/(scot %ux new-root) %agent [u.rollup.state %rollup] %poke -]~
      :-  %rollup-action
      !>  :-  %receive-batch
          :*  addr
              id.hall.town
              mode.hall.town
              state-diffs
              diff-hash
              new-root
              +.new-state
              peer-roots.state
              sig
          ==
    ==
  --
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  ?+    wire  (on-agent:def wire sign)
      [%batch-submit @ ~]
    ?:  ?=(%poke-ack -.sign)
      ?~  p.sign
        ~&  >>  "%sequencer: batch approved by rollup"
        ?~  proposed-batch
          ~|("%sequencer: error: received batch approval without proposed batch" !!)
        =/  new-town=(unit ^town)
          (transition-state town u.proposed-batch)
        :_  this(town new-town, proposed-batch ~, basket ~)
        =-  [%give %fact ~[/indexer/updates] %indexer-update -]~
        !>(`indexer-update`[%update ~(tap in basket.u.proposed-batch) (need new-town) root.u.proposed-batch])
      ::  TODO manage rejected moves here
      ~&  >>>  "%sequencer: our move was rejected by rollup!"
      `this(proposed-batch ~)
    `this
  ::
      [%sub-rollup ~]
    ?:  ?=(%kick -.sign)
      :_  this  ::  attempt to re-sub
      [%pass wire %agent [src.bowl %rollup] %watch (snip `path`wire)]~
    ?.  ?=(%fact -.sign)  `this
    =^  cards  state
      (update-fact !<(rollup-update q.cage.sign))
    [cards this]
  ==
  ::
  ++  update-fact
    |=  upd=rollup-update
    ^-  (quip card _state)
    ?-    -.upd
        %new-peer-root
      ::  update our local map
      `state(peer-roots (~(put by peer-roots.state) town-id.upd root.upd))
    ::
        %new-sequencer
      ::  check if we have been kicked off our town
      ::  this is in place for later..  TODO expand this functionality
      ?~  town.state                  `state
      ?.  =(town-id.upd id.hall.u.town)  `state
      ?:  =(who.upd our.bowl)         `state
      ~&  >>>  "%sequencer: we've been kicked out of town!"
      `state
    ==
  --
::
++  on-arvo
  |=  [=wire =sign-arvo:agent:gall]
  ^-  (quip card _this)
  (on-arvo:def wire sign-arvo)
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ::  all scrys return a unit
  ::
  ?.  =(%x -.path)  ~
  ?+    +.path  (on-peek:def path)
      [%status ~]
    ``noun+!>(status)
  ::
      [%town-id ~]
    ?~  town  ``noun+!>(~)
    ``noun+!>(`id.hall.u.town)
  ::
  ::  state reads fail if sequencer not active
  ::
      [%has @ ~]  ::  see if grain exists in state
    =/  id  (slav %ux i.t.t.path)
    ?~  town  [~ ~]
    ``noun+!>((~(has by p.land.u.town) id))
  ::
      [%grain @ ~]
    ?~  town  [~ ~]
    (read-grain t.path p.land.u.town)
  ::
      [%read @ @tas @ta @ ^]  :: grain id, %noun/%json, argument @ta, other +jam'd data, like tx args, then any associated rice IDs
    ?~  town  [~ ~]
    ::  TODO pre-;; library
    (read-wheat t.path now.bowl id.hall.u.town p.land.u.town smart-lib-vase)
  ==
::
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
