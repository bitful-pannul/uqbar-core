::  uqbar [UQ| DAO]
::
::  The "vane" for interacting with UQ|. Provides read/write layer for userspace agents.
::
/-  ui=indexer
/+  *uqbar, *sequencer, default-agent, dbug, verb, agentio
|%
+$  card  card:agent:gall
+$  state-0
  $:  %0
      min-ping-time=@dr
      max-ping-time=@dr
      next-ping-time=@da
      ping-tids=(map @ta (pair id:smart dock))
      ping-time-fast-delay=@dr
      ping-timeout=@dr
      pings-timedout=(unit @da)
      sources=(jug id:smart dock)  ::  set of indexers for each town
      =sources-ping-results
      sequencers=(map id:smart sequencer)  ::  single sequencer for each town
  ==
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this        .
      def         ~(. (default-agent this %|) bowl)
      io          ~(. agentio bowl)
      uqbar-core  +>
      uc          ~(. uqbar-core bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    =.  min-ping-time         ~m15  ::  TODO: allow user to set?
    =.  max-ping-time         ~h2   ::   Spam risk?
    =.  ping-time-fast-delay  ~s5
    =.  ping-timeout          ~s30
    `this
  ::
  ++  on-save  !>(state)
  ++  on-load
    |=  =old=vase
    ^-  (quip card _this)
    =.  state  !<(state-0 old-vase)
    =.  next-ping-time
      ?:  (gth next-ping-time now.bowl)  next-ping-time
      (make-next-ping-time:uc min-ping-time max-ping-time)
    :_  this
    ?:  (gth next-ping-time now.bowl)  ~
    [(make-ping-wait-card:uc next-ping-time)]~
  ::
  ++  on-watch
    |=  =path
    |^  ^-  (quip card _this)
    ?>  =(src.bowl our.bowl)
    :_  this
    ?+    -.path  !!
        %track
      ~
    ::
        %indexer
      ::  must be of the form, e.g.,
      ::   /indexer/grain/[town-id]/[grain-id]
      ?.  ?=([%indexer @ @ @ ~] path)  ~
      =/  town-id=id:smart  (slav %ux i.t.t.path)
      (watch-indexer town-id /[i.path] t.path)
    ==
    ::
    ++  watch-indexer  ::  TODO: use fallback better?
      |=  [town-id=id:smart wire-prefix=wire sub-path=^path]
      ^-  (list card)
      ?~  source=(get-best-source town-id ~ %nu)
        ~&  >>>  "%uqbar: subscription failed:"
        ~&  >>>  " do not have indexer source for town {<town-id>}."
        ~&  >>>  " Add indexer source for town and try again."
        ~
      :_  ~
      %+  ~(watch pass:io (weld wire-prefix sub-path))
      p.u.source  sub-path
    --
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    |^
    ?>  =(src.bowl our.bowl)
    ?.  ?=(?(%uqbar-action %uqbar-write) mark)
      ~|("%uqbar: rejecting erroneous poke" !!)
    =^  cards  state
      ?-  mark
        %uqbar-action  (handle-action !<(action vase))
        %uqbar-write   (handle-write !<(write vase))
      ==
    [cards this]
    ::
    ++  make-ping-rest-card
      |=  old-next-ping-time=@da
      ^-  card
      %^  %~  arvo  pass:io
          /start-indexer-ping/(scot %da old-next-ping-time)
      %b  %rest  old-next-ping-time
      :: %.  old-next-ping-time
      :: %~  rest  pass:io
      :: /start-indexer-ping/(scot %da old-next-ping-time)
    ::
    ++  handle-action
      |=  act=action
      ^-  (quip card _state)
      ?-    -.act
          %add-source
        =/  faster-next-ping-time=@da
          %+  add  ping-time-fast-delay
          ?~(pings-timedout now.bowl u.pings-timedout)
        :-  :+  (make-ping-rest-card:uc next-ping-time)
              (make-ping-wait-card:uc faster-next-ping-time)
            ~
        %=  state
            next-ping-time  faster-next-ping-time
        ::
            sources
          (~(put ju sources) town-id.act source.act)
        ==
      ::
          %remove-source
        =/  faster-next-ping-time=@da
          %+  add  ping-time-fast-delay
          ?~(pings-timedout now.bowl u.pings-timedout)
        :-  :+  (make-ping-rest-card:uc next-ping-time)
              (make-ping-wait-card:uc faster-next-ping-time)
            ~
        %=  state
            next-ping-time  faster-next-ping-time
        ::
            sources-ping-results
          ?^(pings-timedout ~ sources-ping-results)  :: TODO: can do better?
        ::
            sources
          (~(del ju sources) town-id.act source.act)
        ==
      ::
          %set-sources
        =/  p=path  /capitol-updates
        =/  faster-next-ping-time=@da
          %+  add  ping-time-fast-delay
          ?~(pings-timedout now.bowl u.pings-timedout)
        :-  :+  (make-ping-rest-card:uc next-ping-time)
              (make-ping-wait-card:uc faster-next-ping-time)
            %+  murn  towns.act
            |=  [town=id:smart indexers=(set dock)]
            ^-  (unit card)
            ?~  indexers  ~
            `(~(watch pass:io p) -.indexers p)  ::  TODO: do better here
        %=  state
            next-ping-time  faster-next-ping-time
        ::
            sources-ping-results
          ?^(pings-timedout ~ sources-ping-results)  :: TODO: can do better?
        ::
            sources
          (~(gas by *(map id:smart (set dock))) towns.act)
        ==
      ==
    ::
    ++  handle-write
      |=  =write
      ^-  (quip card _state)
      ?-    -.write
      ::
      ::  Each write can optionally create a subscription, which will forward these things:
      ::
      ::  - a "receipt" from sequencer, which contains a signed hash of the egg
      ::    (signed by both urbit ID and uqbar address -- enforcing that reputational link)
      ::
      ::  - once the egg gets submitted in batch to rollup, a card with the status/errorcode
      ::
      ::  - a card containing the new nonce of the address submitting the egg
      ::    (apps can ignore and track on their own, or use this)
      ::
      ::  To enable status update, uqbar.hoon should subscribe to indexer for that egg
      ::  and unsub when either status is received, or batch is rejected. (TODO how to determine latter?)
      ::
          %submit
        =/  town-id  `@ux`town-id.shell.egg.write
        ?~  seq=(~(get by sequencers.state) town-id)
          ~|("%uqbar: no known sequencer for that town" !!)
        =/  egg-hash  (scot %ux `@ux`(sham [shell yolk]:egg.write))
        :_  state
        =+  [%sequencer-town-action !>([%receive (silt ~[egg.write])])]
        :~  [%pass /submit-transaction/egg-hash %agent [q.u.seq %sequencer] %poke -]
            [%give %fact ~[/track/egg-hash] %write-result !>([%sent ~])]
        ==
      ::
          %receipt
        ::  forward to local watchers
        :_  state
        ~[[%give %fact ~[/track/(scot %ux egg-hash.write)] %write-result !>(write)]]
      ==
    --
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    |^  ^-  (quip card _this)
    ?+    -.wire  (on-agent:def wire sign)
        %capitol-updates
      ::  set sequencers based on rollup state, given by indexer
      ?+    -.sign  (on-agent:def wire sign)
          %kick
        [rejoin this]
      ::
          %fact
        =^  cards  state
          (update-sequencers !<(capitol-update q.cage.sign))
        [cards this]
      ==
    ::
        %indexer
      ?+  -.sign  (on-agent:def wire sign)
        %kick  [rejoin this]
        %fact  [[(pass-through cage.sign)]~ this]
      ==
    ::
        %submit-transaction
      ::  get receipt from sequencer
      ?.  ?=([@ ~] t.wire)      `this
      ?.  ?=(%poke-ack -.sign)  `this
      =/  path  ~[/track/[i.t.wire]]
      :_  this
      ?~  p.sign  ~
      [%give %fact path %write-result !>(`write-result`[%rejected src.bowl])]~
    ::
        %pinger
      ?.  ?=([@ ~] t.wire)
        ~&  >  "1 {<wire>}"
        `this
      =/  tid=@ta  i.t.wire
      ?~  source=(~(get by ping-tids) tid)
        ~&  >  "2 {<tid>} {<wire>} {<ping-tids>} {<sign>}"
        `this
      ?+    -.sign  (on-agent:def wire sign)
          :: %kick      `this
          :: %poke-ack  `this
          %kick
        ~&  >  "%uqbar: %pinger %kick"
        `this
      ::
          %poke-ack
        ~&  >  "%uqbar: %pinger %poke-ack"
        `this
      ::
          %fact
        =.  ping-tids  (~(del by ping-tids) tid)
        ?+    p.cage.sign  (on-agent:def wire sign)
            %thread-fail
          ~&  >>>  "%uqbar: pinger failed tid: {<tid>}; source: {<u.source>}"
          `this
        ::
            %thread-done
          ~&  >  "%uqbar: %pinger %thread-done"
          ?:  =(*vase q.cage.sign)  `this  ::  thread canceled
          =*  town-id  p.u.source
          =*  d        q.u.source
          =.  sources-ping-results
            %+  ~(put by sources-ping-results)  town-id
            =/  [pu=(set dock) pd=(set dock) nu=(set dock) nd=(set dock)]
              %+  ~(gut by sources-ping-results)  town-id
              [~ ~ ~ ~]
            :+  pu  pd
            ?:  !<(? q.cage.sign)  [(~(put in nu) d) nd]
            [nu (~(put in nd) d)]
          ?.  =(0 ~(wyt by ping-tids))  `this
          :_  this(pings-timedout ~)
          ?~  pings-timedout  ~
          :_  ~
          %^  %~  arvo  pass:io
              /ping-timeout/(scot %da u.pings-timedout)
          %b  %rest  u.pings-timedout
          :: :-  %.  u.pings-timedout
          ::     %~  rest  pass:io
          ::     /ping-timeout/(scot %da u.pings-timedout)
          :: TODO: move current subscriptions if on non-replying indexer?
        ==
      ==
    ==
    ::
    ++  pass-through
      |=  =cage
      ^-  card
      (fact:io cage ~[wire])
    ::
    ++  rejoin  ::  TODO: ping indexers and find responsive one?
      ^-  (list card)
      =/  old-source=(unit [dock path])  get-wex-dock-by-wire
      ?~  old-source  ~
      ~[(~(watch pass:io wire) u.old-source)]
    ::
    ++  get-wex-dock-by-wire
      ^-  (unit [dock path])
      ?:  =(0 ~(wyt by wex.bowl))  ~
      =/  wexs=(list [[w=^wire s=ship t=term] a=? p=path])
        ~(tap by wex.bowl)
      |-
      ?~  wexs  ~
      =*  wex  i.wexs
      ?.  =(wire w.wex)  $(wexs t.wexs)
      `[[s.wex t.wex] p.wex]
    ::
    ++  update-sequencers
      |=  upd=capitol-update
      ^-  (quip card _state)
      :-  ~
      %=    state
          sequencers
        %-  ~(run by capitol.upd)
        |=(=hall sequencer.hall)
      ==
    --
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo:agent:gall]
    |^  ^-  (quip card _this)
    ?+    wire  (on-arvo:def wire sign-arvo)
        [%start-indexer-ping @ ~]
      ?+    sign-arvo  (on-arvo:def wire sign-arvo)
          [%behn %wake *]
        =/  until=@da  (slav %da i.t.wire)
        ?:  (gth until now.bowl)  `this
        ?^  error.sign-arvo
          ~&  >>>  "%uqbar: error from ping timer: {<u.error.sign-arvo>}"
          `this
        ~&  >  "%uqbar: +on-arvo: %start-indexer-ping %behn %wake"
        :: =.  next-ping-time  make-next-ping-time:uc
        =.  next-ping-time
          %+  make-next-ping-time:uc  min-ping-time
          max-ping-time
        =.  sources-ping-results
          %-  ~(urn by sources-ping-results)
          |=  $:  town-id=id:smart
                  previous-up=(set dock)
                  previous-down=(set dock)
                  newest-up=(set dock)
                  newest-down=(set dock)
              ==
          [newest-up newest-down ~ ~]
        =^  cards  state  make-ping-indexer-cards
        :_  this
        [(make-ping-wait-card:uc next-ping-time) cards]
      ==
    ::
        [%ping-timeout @ ~]
      ?+    sign-arvo  (on-arvo:def wire sign-arvo)
          [%behn %wake *]
        =/  until=@da  (slav %da i.t.wire)
        ?:  (gth until now.bowl)  `this
        ~&  >  "%uqbar: %ping-timeout for remaining: {<ping-tids>}"
        :-  %-  zing
            %+  turn  ~(tap by ping-tids)
            |=  [tid=@ta id:smart dock]
            :+    %+  ~(poke-our pass:io /pinger/[tid])
                  %spider  [%spider-stop !>([tid %.y])]
              (~(leave-our pass:io /pinger/[tid]) %spider)
            ~
        %=  this
            ping-tids       ~
            pings-timedout  ~
            sources-ping-results
          %-  ~(gas by sources-ping-results)
          %+  turn  ~(tap by ping-tids)
          |=  [@ta town-id=id:smart d=dock]
          =/  [pu=(set dock) pd=(set dock) nu=(set dock) nd=(set dock)]
            %+  ~(gut by sources-ping-results)  town-id
            [~ ~ ~ ~]
          [town-id pu pd nu (~(put in nd) d)]
        ==
      ==
    ==
    ::
    ++  make-ping-indexer-cards
      ^-  (quip card _state)
      =|  cards=(list card)
      =|  tids=(list [@ta (pair id:smart dock)])
      =/  all-sources=(list (pair id:smart dock))
        %-  zing
        %+  turn  ~(tap by sources)
        |=  [town-id=id:smart docks=(set dock)]
        %+  turn  ~(tap in docks)
        |=(d=dock [town-id d])
      |-
      ?~  all-sources
        =.  pings-timedout  `(add now.bowl ping-timeout)
        :_  state(ping-tids (~(gas by *_ping-tids) tids))
        ?>  ?=(^ pings-timedout)
        :-  %.  u.pings-timedout
            %~  wait  pass:io
            /ping-timeout/(scot %da u.pings-timedout)
        cards
      =*  town-id  p.i.all-sources
      =*  d        q.i.all-sources
      =/  tid=@ta
        %+  rap  3
        :~  'ted-'
            (scot %uw (sham eny.bowl))
            '-'
            (scot %ux town-id)
            '-'
            (scot %p p.d)
        ==
      =/  start-args
        :-  ~
        :^  `tid  byk.bowl(r da+now.bowl)
        %uqbar-pinger  !>(`dock`d)
      %=  $
          all-sources  t.all-sources
          tids         [[tid i.all-sources] tids]
      ::
          cards
        :+  %+  ~(watch-our pass:io /pinger/[tid])
            %spider  /thread-result/[tid]
          %+  ~(poke-our pass:io /pinger/[tid])
          %spider  [%spider-start !>(start-args)]
        cards
      ==
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
        ::  TODO: scry contract interface/data from sequencer?
        ::  TODO: scry wallet?
        [%indexer *]
      :^  ~  ~  %noun
      !>  ^-  update:ui
      .^(update:ui %gx (scry:io %indexer (snoc t.t.path %noun)))
    ==
  ::
  ++  on-leave  on-leave:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
+*  io  ~(. agentio bowl)
::
++  make-ping-wait-card
  |=  next-ping-time=@da
  ^-  card
  %.  next-ping-time
  %~  wait  pass:io
  /start-indexer-ping/(scot %da next-ping-time)
::
++  make-next-ping-time
  |=  [min-time=@dr max-time=@dr]
  ^-  @da
  %+  add  now.bowl
  %+  add  min-time
  %-  ~(rad og eny.bowl)
  (sub max-time min-time)
::
++  roll-without-replacement
  |=  [max=@ud seen=(list @ud)]
  ^-  [@ud (list @ud)]
  =/  number-seen=@ud  (lent seen)
  =/  roll=@ud  (~(rad og eny.bowl) (sub max number-seen))
  =|  index=@ud
  |-
  ?:  |(=(number-seen index) (lth roll (snag index seen)))
    =.  roll  (add roll index)
    [roll (into seen index roll)]
  $(index +(index))
::
++  get-best-source
  |=  [town-id=id:smart seen=(list @ud) level=?(%nu %nd %pu %pd %~)]
  ^-  (unit [p=dock q=(list @ud) r=?(%nu %nd %pu %pd %~)])
  =+  town-spr=(~(get by sources-ping-results) town-id)
  =+  town-s=(~(get ju sources) town-id)
  ?~  town-spr
    =/  size-town-s=@ud  ~(wyt in town-s)
    ?:  =(0 size-town-s)  ~
    =^  index  seen
      (roll-without-replacement size-town-s seen)
    `[(snag index ~(tap in town-s)) seen %~]
  =/  [level-town-spr=(set dock) next-level=?(%nu %nd %pu %pd %~)]
    =*  newest-up    newest-up.u.town-spr
    =*  newest-down  newest-down.u.town-spr
    =/  newest-seen-so-far=(set dock)
      (~(uni in newest-up) newest-down)
    ?+    level  !!  ::  TODO: handle better?
        %nu
      :-  newest-up
      ?.  =(~(wyt in newest-up) +((lent seen)))  level
      ?:(=(town-s newest-seen-so-far) %nd %pu)
    ::
        %pu
      =*  previous-up  previous-up.u.town-spr
      :-  (~(dif in previous-up) newest-seen-so-far)
      ?.(=(~(wyt in previous-up) +((lent seen))) level %pd)
    ::
        %pd
      =*  previous-down  previous-down.u.town-spr
      :-  (~(dif in previous-down) newest-seen-so-far)
      ?.(=(~(wyt in previous-down) +((lent seen))) level %nd)
    ::
        %nd
      :-  newest-down
      ?.(=(~(wyt in newest-down) +((lent seen))) level %~)
    ==
  =^  index  seen
    (roll-without-replacement ~(wyt in level-town-spr) seen)
  :^  ~  (snag index ~(tap in level-town-spr))
  ?.(=(level next-level) ~ seen)  next-level
--
