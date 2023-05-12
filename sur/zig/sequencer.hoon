/-  *zig-engine
/+  smart=zig-sys-smart
|%
+$  ship-sig   [p=@ux q=ship r=life]
+$  sequencer  (pair address:smart ship)
::
+$  town  [=chain =hall]
::
+$  hall
  $:  town-id=@ux
      batch-num=@ud
      =sequencer
      mode=availability-method  ::  *always* %full-publish for now
      latest-diff-hash=@ux
      roots=(list @ux)
      ::  deposits from the rollup contract are tracked, such that each
      ::  can only be used *once*. a sequencer handles deposits by linking
      ::  a transaction hash to the rollup contract and directly inserting
      ::  the associated assets into the town state.
      deposits=(set @ux)
  ==
::
::  working state tracked here
::
+$  proposed-batch
  $:  num=@ud
      =processed-txs
      =chain
      diff-hash=@ux
      root=@ux
      deposits=(set @ux)
  ==
::
::  capitol: tracks sequencer and state roots / diffs for all towns
::
+$  capitol  (map @ux hall)
::
::  town state transition
::
+$  batch
  $:  town-id=id:smart
      num=@ud
      mode=availability-method  ::  *always* %full-publish for now
      state-diffs=(list state)
      diff-hash=@ux
      new-root=@ux
      new-state=chain
      peer-roots=(map id:smart @ux)  ::  roots for all other towns
      =sig:smart                     ::  sequencer signs new state root
  ==
::
+$  availability-method
  $%  [%full-publish ~]
      [%committee members=(map address:smart [ship (unit sig:smart)])]
  ==
::
+$  town-action
  $%  ::  administration
      $:  %init
          rollup-host=ship
          =address:smart
          private-key=@ux
          town-id=@ux
          starting-state=(unit chain)
          mode=availability-method
      ==
      [%set-block-height-api-key key=@t]
      [%del-block-height-api-key ~]
      [%clear-state ~]
      ::  transactions
      [%deposit deposit-bytes=tape]  ::  from sidecar
      [%receive =transaction:smart]
      [%run-pending eth-block-height=@ud]
      ::  batching
      [%trigger-batch ~]
      [%perform-batch eth-block-height=@ud]
  ==
::
+$  rollup-update
  $%  [%new-sequencer town=id:smart who=ship]
      $:  %new-peer-root
          =sequencer
          town=id:smart
          root=@ux
          batch-num=@ud
          timestamp=@da
      ==
  ==
::
+$  sidecar-action
  $%  [%batch-posted town-root=@ux]
      [%batch-rejected town-root=@ux]
  ==
::
::  indexer must verify root is posted to rollup before verifying new state
::  pair of [transactions town] is batch from sur/indexer.hoon
+$  indexer-update
  $%  [%notify town=id:smart root=@ux]
      [%update root=@ux transactions=processed-txs town]
  ==
::
::  historical states
::
+$  old-proposed-batch
  [num=@ud =processed-txs =chain diff-hash=@ux root=@ux]
+$  state-1
  $:  %1
      rollup=(unit ship)      ::  replace in future with ETH contract address
      private-key=(unit @ux)  ::  our signing key
      town=(unit town)        ::  chain-state
      peer-roots=(map town=@ux root=@ux)  ::  track updates from rollup
      pending=mempool         ::  unexecuted transactions
      =memlist                ::  executed transactions in working state
      proposed-batch=(unit old-proposed-batch)   ::  stores working state
      status=?(%available %off)
      block-height-api-key=(unit @t)
  ==
::
+$  state-2
  $:  %2
      last-batch-time=@da      ::  saved to compare against indexer acks
      indexers=(map dock @da)  ::  indexers receiving batch updates
      rollup=(unit ship)       ::  replace in future with ETH contract address
      private-key=(unit @ux)   ::  our signing key
      town=(unit town)         ::  chain-state
      peer-roots=(map town=@ux root=@ux)  ::  track updates from rollup
      pending=mempool          ::  unexecuted transactions
      =memlist                 ::  executed transactions in working state
      proposed-batch=(unit old-proposed-batch)   ::  stores working state
      status=?(%available %off)
      block-height-api-key=(unit @t)
  ==
--
