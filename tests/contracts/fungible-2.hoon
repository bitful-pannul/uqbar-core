::  fungible.hoon tests
/-  zink
/+  *test, smart=zig-sys-smart, *sequencer, merk, ethereum
/+  fun=zig-contracts-lib-fungible
/*  smart-lib-noun  %noun  /lib/zig/compiled/smart-lib/noun
/*  zink-cax-noun   %noun  /lib/zig/compiled/hash-cache/noun
/*  zigs-contract   %noun  /lib/zig/compiled/zigs/noun
/*  fung-contract   %noun  /lib/zig/compiled/fungible/noun
|%
::
::  constants / dummy info for mill
::
++  big  (bi:merk id:smart grain:smart)  ::  merkle engine for granary
++  pig  (bi:merk id:smart @ud)          ::                for populace
++  batch-num  1
++  town-id    0x2
++  rate    1
++  budget  10
++  fake-sig   [0 0 0]
++  mil
  %~  mill  mill
  :+    ;;(vase (cue q.q.smart-lib-noun))
    ;;((map * @) (cue q.q.zink-cax-noun))
  %.y
::
+$  mill-result
  [fee=@ud =land burned=granary =errorcode:smart hits=(list hints:zink) =crow:smart]
::
::  fake data
::
++  miller    ^-  caller:smart  [0x1512.3341 1 0x1.1512.3341]
++  caller-1  ^-  caller:smart  [0xbeef 1 0x1.beef]
++  caller-2  ^-  caller:smart  [0xdead 1 0x1.dead]
++  caller-3  ^-  caller:smart  [0xcafe 1 0x1.cafe]
::
++  zigs
  |%
  ++  zig-account
    |=  [holder=id:smart amt=@ud]
    ^-  [id:smart grain:smart]
    =/  sal  `@`'zigsalt'
    =/  id  (fry-rice:smart zigs-wheat-id:smart holder town-id sal)
    :-  id
    :*  %&  sal  %account
        [amt ~ `@ux`'zigs']
        id
        zigs-wheat-id:smart
        holder
        town-id
    ==
  ++  miller-account
    ^-  grain:smart
    :*  %&
        `@`'zigs'
        %account
        [1.000.000 ~ `@ux`'zigs-metadata']
        0x1.1512.3341
        zigs-wheat-id:smart
        0x1512.3341
        town-id
    ==
  ++  wheat
    :: todo does mill:mill depend on this existing
    ^-  grain:smart
    =/  cont  ;;([bat=* pay=*] (cue q.q.zigs-contract))
    =/  interface=lumps:smart  ~
    =/  types=lumps:smart  ~
    :*  %|
        `cont
        interface
        types
        zigs-wheat-id:smart  ::  id
        zigs-wheat-id:smart  ::  lord
        zigs-wheat-id:smart  ::  holder
        town-id
    ==
  --
::
::
::
::  N.B. owner zigs ids must match the ones generated in `+zig-account`
++  priv-1  0xbeef.beef.beef.beef.beef.beef.beef.beef.beef.beef
++  pub-1   (address-from-prv:key:ethereum priv-1)
++  owner-1
  ^-  caller:smart
  [pub-1 0 (fry-rice:smart zigs-wheat-id:smart pub-1 town-id `@`'zigsalt')]
++  account-1
  ^-  grain:smart
  :*  %&  `@`'salt'  %account
      `account:sur:fun`[50 ~ `@ux`'simple' 0]
      0x1.beef
      id.p:fungible-wheat  ::  lord
      pub-1              ::  holder
      town-id
  ==
::
++  priv-2  0xdead.dead.dead.dead.dead.dead.dead.dead.dead.dead
++  pub-2   (address-from-prv:key:ethereum priv-2)
++  owner-2
  ^-  caller:smart
  [pub-2 0 (fry-rice:smart zigs-wheat-id:smart pub-2 town-id `@`'zigsalt')]
++  account-2  ^-  grain:smart
  :*  %&  `@`'salt'  %account
      `account:sur:fun`[30 ~ `@ux`'simple' 0]
      0x1.dead
      id.p:fungible-wheat
      pub-2
      town-id
  ==
::
++  priv-3  0xcafe.cafe.cafe.cafe.cafe.cafe.cafe.cafe.cafe.cafe
++  pub-3   (address-from-prv:key:ethereum priv-3)
++  owner-3
  ^-  caller:smart
  [pub-3 0 (fry-rice:smart zigs-wheat-id:smart pub-3 town-id `@`'zigsalt')]
++  account-3
  ^-  grain:smart
  :*  %&  `@`'salt'  %account
      `account:sur:fun`[20 (malt ~[[0xffff 100]]) `@ux`'simple' 0]
      0x1.cafe
      id.p:fungible-wheat
      pub-3
      town-id
  ==
::
++  fungible-wheat
  ^-  grain:smart
  =/  cont  ;;([bat=* pay=*] (cue q.q.fung-contract))
  =/  interface=lumps:smart  ~
  =/  types=lumps:smart      ~
  :*  %|
      `cont
      interface
      types
      id=`@ux`'fungible'
      lord=`@ux`'fungible'
      holder=`@ux`'fungible'
      town-id
  ==
++  metadata-1
  ^-  grain:smart
  :*  %&  `@`'salt'  %metadata
      ^-  token-metadata:sur:fun
      :*  name='Simple Token'
          symbol='ST'
          decimals=0
          supply=100
          cap=~
          mintable=%.n
          minters=~
          deployer=0x0
          salt=`@`'salt'
      ==
      `@ux`'simple-metadata-1'
      id.p:fungible-wheat
      `@ux`'holder'
      town-id
  ==
++  metadata-mintable
  ^-  grain:smart
  :*  %&  `@`'salt'  %metadata
      ^-  token-metadata:sur:fun
      :*  name='Simple Token'
          symbol='ST'
          decimals=0
          supply=100
          cap=`1.000
          mintable=%.y
          minters=(silt ~[pub-1])
          deployer=0x0
          salt=`@`'salt'
      ==
      `@ux`'simple-mintable'
      id.p:fungible-wheat
      `@ux`'holder'
      town-id
  ==
::
::
::
++  fake-granary
  ^-  granary
  %+  gas:big  *(merk:merk id:smart grain:smart)
  :~  [id.p:fungible-wheat fungible-wheat]
      [id.p:metadata-1 metadata-1]
      [id.p:metadata-mintable metadata-mintable]
      [id.p:account-1 account-1]
      [id.p:account-2 account-2]
      [id.p:account-3 account-3]
      (zig-account:zigs holder.p:account-1 999.999)
      (zig-account:zigs holder.p:account-2 999.999)
      (zig-account:zigs holder.p:account-3 999.999)
      ::[id.p:miller-account:zigs miller-account:zigs]
      ::[id.p:wheat:zigs wheat:zigs]
  ==
++  fake-populace
  ^-  populace
  %+  gas:pig  *(merk:merk id:smart @ud)
  :~  [pub-1 0]
      [pub-2 0]
      [pub-3 0]
  ==
++  fake-land
  ^-  land
  [fake-granary fake-populace]
++  test-set-allowance
  ^-  tang
  =/  =action:sur:fun  [%set-allowance 0xcafe 10]
  ::~&  "zigs owner 1 {<zigs:owner-1>}"
  =/  shel=shell:smart
    [[id +(nonce) zigs]:owner-1 ~ id.p:fungible-wheat rate budget town-id 0]
  =/  updated-1=grain:smart
    :*  %&  `@`'salt'  %account
        `account:sur:fun`[50 (malt ~[[0xcafe 10]]) `@ux`'simple' 0]
        id.p:account-1
        id.p:fungible-wheat
        pub-1
        town-id
    ==
  =/  correct=chick:smart  (result:smart ~[updated-1] ~ ~ ~)
  ~&  >  (key:big fake-granary)
  =/  milled=mill-result
    %+  ~(mill mil miller town-id 1)
    fake-land  `egg:smart`[fake-sig shel action]
  ::~&  >>  (key:big p.land.milled)
  ::~&  >  p.land.milled
  ~&  milled
  =/  res=grain:smart  (got:big p.land.milled id.p:account-1)
  (expect-eq !>(res) !>(correct))
--