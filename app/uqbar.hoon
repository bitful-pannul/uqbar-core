::  uqbar [UQ| DAO]
::
::  The agent for interacting with Uqbar. Provides read/write layer for userspace agents.
::
::  TODO: when remote scry releases, take another look at remote indexer
::  and fallback logic, e.g. at
::  https://github.com/uqbar-dao/uqbar-core/blob/master/app/uqbar.hoon#L636-L637
::
/-  f=zig-faucet,
    u=zig-uqbar,
    ui=zig-indexer,
    w=zig-wallet
/+  agentio,
    default-agent,
    dbug,
    verb,
    s=zig-sequencer,
    sig=zig-sig,
    ethereum,
    smart=zig-sys-smart
|%
+$  card  card:agent:gall
+$  state-1
  $:  %1
      indexers=(map id:smart dock)           ::  single indexer for each town
      sequencers=(map id:smart sequencer:s)  ::  single sequencer for each town
      wallet-source=term  ::  track any wallet ship is using
  ==
--
::
=|  state-1
=*  state  -
::
%-  agent:dbug
::  %+  verb  &
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this  .
      def   ~(. (default-agent this %|) bowl)
      io    ~(. agentio bowl)
      uc    ~(. +> bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this(wallet-source %wallet)  ::  name of locally-installed wallet app
    :_  ~
    %-  ~(poke-self pass:io /self-wire)
    :-  %uqbar-action
    !>  ^-  action:u
    [%set-sources [0x0 [our.bowl %indexer]]~]
  ::
  ++  on-save  !>(state)
  ++  on-load
    |=  =old=vase
    ^-  (quip card _this)
    ?+    -.q.old-vase  on-init
        %1
      `this(state !<(state-1 old-vase))
    ==
  ::
  ++  on-watch
    |=  =path
    |^  ^-  (quip card _this)
    ?>  =(src.bowl our.bowl)
    :_  this
    ?+    -.path  !!
        %wallet
      ::  must be of the form, e.g.,
      ::   /wallet/[requesting-app-name]/[*-updates]
      ?.  ?=([%wallet @ @ ~] path)  ~  ::  TODO: kick
      (watch-wallet /[i.path]/[i.t.path] t.t.path)
    ::
        %indexer
      ::  must be of the form, e.g.,
      ::   /indexer/[requesting-app-name]/batch-order/[town-id]
      ::   or
      ::   /indexer/[requesting-app-name]/json/batch-order/[town-id]
      ?.  ?=([%indexer @ @ *] path)  ~  ::  TODO: kick
      =/  town-id=id:smart              ::  TODO: generalize?
        ?:  ?=([%indexer @ @ @ ~] path)
          (slav %ux i.t.t.t.path)
        ?>  ?=([%indexer @ %json @ @ ~] path)
        (slav %ux i.t.t.t.t.path)
      (watch-indexer town-id /[i.path]/[i.t.path] t.t.path)
    ==
    ::
    ++  watch-wallet
      |=  [wire-prefix=wire sub-path=^path]
      ^-  (list card)
      :_  ~
      %+  ~(watch-our pass:io (weld wire-prefix sub-path))
      wallet-source  sub-path
    ::
    ++  watch-indexer
      |=  [town=id:smart wire-prefix=wire sub-path=^path]
      ^-  (list card)
      ?~  source=(~(get by indexers) town)
        ~&  >>>  "%uqbar: subscription failed:"
        ~&  >>>  " do not have indexer source for town {<town>}."
        ~&  >>>  " Add indexer source for town and try again."
        ~
      ~&  %uqbar^%on-watch^%indexer^src.bowl^u.source
      :_  ~
      %+  ~(watch pass:io (weld wire-prefix sub-path))
      u.source  sub-path
    --
  ::
  ++  on-poke
    |=  [m=mark v=vase]
    ^-  (quip card _this)
    |^
    =^  cards  state
      ?+    m  ~|("%uqbar: rejecting erroneous poke {<m>}" !!)
          %uqbar-action  (handle-action !<(action:u v))
          %uqbar-write   (handle-write !<(write:u v))
          %wallet-poke          handle-wallet-poke
          %uqbar-share-address  handle-wallet-poke
      ==
    [cards this]
    ::
    ++  handle-wallet-poke
      ^-  (quip card _state)
      :_  state
      :_  ~
      (~(poke-our pass:io /wallet-poke) wallet-source [m v])
    ::
    ++  handle-action
      |=  act=action:u
      ^-  (quip card _state)
      ?>  =(src.bowl our.bowl)
      ?-    -.act
          %set-wallet-source
        `state(wallet-source app-name.act)
      ::
          %remove-source
        :-  ~
        %=  state
            indexers
          (~(del by indexers) town.act source.act)
        ==
      ::
          %set-sources
        :_  state(indexers (~(gas by *(map id:smart dock)) towns.act))
        %+  turn  towns.act
        |=  [town=id:smart indexer=dock]
        (~(watch pass:io /rollup-updates) indexer /rollup-updates)
      ::
          %open-faucet
        ::  poke known sequencer for this town, will fail if they don't
        ::  host a faucet.
        :_  state
        :_  ~
        %+  ~(poke pass:io /poke-faucet)
          [q:(~(got by sequencers) town.act) %faucet]
        :-  %faucet-action
        !>  ^-  action:f
        [%open town.act send-to.act]
      ==
    ::
    ++  handle-write
      |=  =write:u
      ^-  (quip card _state)
      ?-    -.write
          %submit
        ::  forward a transaction to sequencer we're tracking
        ::  for the specified town
        ?>  =(src.bowl our.bowl)
        ?~  seq=(~(get by sequencers.state) `@ux`town.transaction.write)
          ~|("%uqbar: no known sequencer for that town" !!)
        =/  tx-hash  `@ux`(sham +.transaction.write)
        :_  state
        :~  %+  ~(poke pass:io /write-result)
              [our.bowl wallet-source]
            uqbar-write-result+!>(`write-result:u`[tx-hash %sent ~])
          ::
            %+  %~  poke  pass:io
                /submit-transaction/(scot %ux tx-hash)
              [q.u.seq %sequencer]
            :-  %sequencer-town-action
            !>(`town-action:s`[%receive transaction.write])
        ==
      ::
          %receipt
        ::  forward to connected wallet app after
        ::  verifying the sequencer's signatures
        ?~  seq=(~(get by sequencers.state) `@ux`town.transaction.write)
          ~|("%uqbar: got receipt from a stranger" !!)
        =/  signed-stuff  (sham [transaction output]:write)
        ?>  =(q.u.seq q.ship-sig.write)
        ?>  (validate:sig our.bowl ship-sig.write signed-stuff now.bowl)
        ?>  (uqbar-validate:sig p.u.seq signed-stuff uqbar-sig.write)
        :_  state  :_  ~
        %+  ~(poke pass:io /write-result)
          [our.bowl wallet-source]
        :-  %uqbar-write-result
        !>(`write-result:u`[tx-hash.write %receipt +.+.write])
      ==
    --
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    |^
    ^-  (quip card _this)
    ?+    -.wire  (on-agent:def wire sign)
        %rollup-updates
      ::  set sequencers based on rollup state, given by indexer
      ?+    -.sign  (on-agent:def wire sign)
          %kick
        [rejoin this]
      ::
          %fact
        =+  !<(upd=rollup-update:s q.cage.sign)
        ?>  ?=(%new-peer-root -.upd)
        `this(sequencers (~(put by sequencers.state) [town sequencer]:upd))
      ==
    ::
        %indexer
      ?+  -.sign  (on-agent:def wire sign)
        %kick  [rejoin this]
        %fact  [[(pass-through cage.sign)]~ this]
      ==
    ::
        %submit-transaction
      ::  get poke-ack from sequencer
      ?.  ?=([@ ~] t.wire)      `this
      ?.  ?=(%poke-ack -.sign)  `this
      =/  tx-hash=hash:smart  (slav %ux i.t.wire)
      :_  this  :_  ~
      ?~  p.sign
        ::  ack
        %+  ~(poke pass:io /write-result)
          [our.bowl wallet-source]
        uqbar-write-result+!>(`write-result:u`[tx-hash %delivered ~])
      ::  nack
      %+  ~(poke pass:io /write-result)
        [our.bowl wallet-source]
      uqbar-write-result+!>(`write-result:u`[tx-hash %rejected ~])
    ==
    ::
    ++  pass-through
      |=  =cage
      ^-  card
      (fact:io cage ~[wire])
    ::
    ++  rejoin  ::  TODO: ping indexers and find responsive one?
      ^-  (list card)
      =/  old-source=(unit [dock path])
        (get-wex-dock-by-wire:uc wire)
      ?~  old-source  ~
      ~[(~(watch pass:io wire) u.old-source)]
    --
  ::
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ::  all scrys should return a unit
    ::
    ::  TODO: revisit this when remote scry is a thing..
    ::
    ?.  =(%x -.path)  ~
    ?+    +.path  (on-peek:def path)
        [%indexer @ *]
      =/  is-json=?  ?=(%json i.t.t.path)
      ?.  is-json
        :^  ~  ~  %indexer-update
        !>  ^-  update:ui
        .^(update:ui %gx (scry:io %indexer (snoc t.t.path %noun)))
      :^  ~  ~  %json
      !>  ^-  json
      .^(json %gx (scry:io %indexer (snoc t.t.path %json)))
    ::
        [%wallet *]
      :^  ~  ~  %wallet-update
      !>  ^-  wallet-update:w
      .^(wallet-update:w %gx (scry:io %wallet (snoc t.t.path %noun)))
    ::
    ::  return the @p and uqbar address for a sequencer on given town,
    ::  if we know of one. form: (unit (pair address ship))
    ::
        [%sequencer-on-town @ ~]
      ``noun+!>((~(get by sequencers.state) (slav %ux i.t.t.path)))
    ==
  ::
  ++  on-leave
    |=  =path
    |^  ^-  (quip card _this)
    ?>  =(src.bowl our.bowl)
    :_  this
    ?+    -.path  !!
        %track  ~
        %wallet
      ::  must be of the form, e.g.,
      ::   /wallet/[requesting-app-name]/[*-updates]
      ?.  ?=([%wallet @ @ ~] path)  ~
      leave-wallet
    ::
        %indexer
      ::  must be of the form, e.g.,
      ::   /indexer/[requesting-app-name]/grain/[town-id]/[grain-id]
      ?.  ?=([%indexer @ @ @ @ ~] path)  ~
      leave-indexer
    ==
    ::
    ++  leave-wallet
      ^-  (list card)
      [(~(leave-our pass:io path) wallet-source)]~
    ::
    ++  leave-indexer
      ^-  (list card)
      ?~  dock-path=(get-wex-dock-by-wire:uc path)  ~
      [(~(leave pass:io path) p.u.dock-path)]~
    --
  ::
  ++  on-arvo  on-arvo:def
  ++  on-fail   on-fail:def
  --
::
::  uqbar-core
::
|_  =bowl:gall
+*  io  ~(. agentio bowl)
::
++  get-wex-dock-by-wire
  |=  =wire
  ^-  (unit (pair dock path))
  ?:  =(0 ~(wyt by wex.bowl))  ~
  =/  wexs=(list [[w=^wire s=ship t=term] a=? p=path])
    ~(tap by wex.bowl)
  |-
  ?~  wexs  ~
  =*  wex  i.wexs
  ?.  =(wire w.wex)  $(wexs t.wexs)
  `[[s.wex t.wex] p.wex]
--
