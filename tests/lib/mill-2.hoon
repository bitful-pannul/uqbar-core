::
::  Tests for lib/zig/mill.hoon
::  Basic goal: construct a simple town / helix state
::  and manipulate it with some calls to our zigs contract.
::  Mill should handle clearing a mempool populated by
::  calls and return an updated town. The zigs contract
::  should manage transactions properly so this is testing
::  the arms of that contract as well.
::
::  Tests here should cover:
::  (all calls to exclusively zigs contract)
::
::  * executing a single call with +mill
::  * executing same call unsuccessfully -- not enough gas
::  * unsuccessfully -- some constraint in contract unfulfilled
::  * (test all constraints in contract: balance, gas, +give, etc)
::  * executing multiple calls with +mill-all
::
/-  zink
/+  *test, smart=zig-sys-smart, *sequencer, merk
/*  smart-lib-noun  %noun  /lib/zig/compiled/smart-lib/noun
/*  zink-cax-noun   %noun  /lib/zig/compiled/hash-cache/noun
/*  triv-contract   %noun  /lib/zig/compiled/trivial/noun
/*  scry-contract   %noun  /lib/zig/compiled/trivial-scry/noun
/*  zigs-contract   %noun  /lib/zig/compiled/zigs/noun
/*  temp-contract   %noun  /lib/zig/compiled/tester/noun
|%
::
::  constants / dummy info for mill
::
++  big  (bi:merk id:smart grain:smart)  ::  merkle engine for granary
++  pig  (bi:merk id:smart @ud)          ::                for populace
++  town-id    0x0
++  set-fee    7
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
  ++  holder-1  0xbeef
  ++  holder-2  0xdead
  ++  holder-3  0xcafe
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
  ++  beef-account
    ^-  grain:smart
    :*  %&
        `@`'zigs'
        %account
        [300.000.000 ~ `@ux`'zigs-metadata']
        0x1.beef
        zigs-wheat-id:smart
        holder-1
        town-id
    ==
  ++  dead-account
    ^-  grain:smart
    :*  %&
        `@`'zigs'
        %account
        [200.000 ~ `@ux`'zigs-metadata']
        0x1.dead
        zigs-wheat-id:smart
        holder-2
        town-id
    ==
  ++  cafe-account
    ^-  grain:smart
    :*  %&
        `@`'zigs'
        %account
        [100.000 ~ `@ux`'zigs-metadata']
        0x1.cafe
        zigs-wheat-id:smart
        holder-3
        town-id
    ==
  ++  wheat
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
++  scry-wheat
  ^-  grain:smart
  =/  cont  ;;([bat=* pay=*] (cue q.q.scry-contract))
  =/  interface=lumps:smart  ~
  =/  types=lumps:smart  ~
  :*  %|
      `cont
      interface
      types
      0xdada.dada  ::  id
      0xdada.dada  ::  lord
      0xdada.dada  ::  holder
      town-id
  ==
::
++  temp-wheat
  ^-  grain:smart
  =/  cont  ;;([bat=* pay=*] (cue q.q.temp-contract))
  =/  interface=lumps:smart  ~
  =/  types=lumps:smart      ~
  :*  %|
      `cont
      interface
      types
      0xcafe.cafe  ::  id
      0xcafe.cafe  ::  lord
      0xcafe.cafe  ::  holder
      town-id
  ==
++  temp-grain
  ^-  grain:smart
  :*  %&
      `@`'loach'
      %account
      [300.000.000 ~ `@ux`'custom-token']
      0x1111.2222.3333
      0xcafe.cafe
      `@ux`123.456.789
      town-id
  ==
::
++  fake-granary
  ^-  granary
  %+  gas:big  *(merk:merk id:smart grain:smart)
  :~  :: [id.p:scry-wheat scry-wheat]
      :: [id.p:wheat:zigs wheat:zigs]
      [id.p:temp-wheat temp-wheat]
      [id.p:temp-grain temp-grain]
      [id.p:beef-account:zigs beef-account:zigs]
      :: [id.p:dead-account:zigs dead-account:zigs]
      :: [id.p:miller-account:zigs miller-account:zigs]
  ==
++  fake-populace
  ^-  populace
  %+  gas:pig  *(merk:merk id:smart @ud)
  ~[[holder-1:zigs 0]] :: [holder-2:zigs 0] [holder-3:zigs 0]]
++  fake-land
  ^-  land
  [fake-granary fake-populace]
::
::  begin tests
::
++  test-mill-tester
  =/  =yolk:smart  [%look 0x1111.2222.3333]
  =/  shel=shell:smart
    [caller-1 ~ id.p:temp-wheat 1 1.000.000 town-id 0]
  =/  res=[fee=@ud =land burned=granary =errorcode:smart hits=(list) =crow:smart]
    %+  ~(mill mil miller town-id 1)
    fake-land  `egg:smart`[fake-sig shel yolk]
  ~&  >>  "output: {<crow.res>}"
  ~&  >>  "fee: {<fee.res>}"
  ~&  >>  "diff:"
  ~&  p.land.res
  ::  assert that our call went through
  %+  expect-eq
    !>(%0)
  !>(errorcode.res)
::  ++  test-mill-zigs-give
::    =/  =yolk:smart  [%give holder-2:zigs 69 0x1.beef `0x1.dead]
::    =/  shel=shell:smart
::      [caller-1 ~ id.p:wheat:zigs 1 1.000.000 town-id 0]
::    =/  res=[fee=@ud =land burned=granary =errorcode:smart hits=(list) =crow:smart]
::      %+  ~(mill mil miller town-id 1)
::      fake-land  `egg:smart`[fake-sig shel yolk]
::    ~&  >  "output: {<crow.res>}"
::    ~&  >  "fee: {<fee.res>}"
::    ::  assert that our call went through
::    %+  expect-eq
::      !>(%0)
::    !>(errorcode.res)
::
::  ++  test-mill-trivial-scry
::    =/  =yolk:smart  [%find 0x1.dead]
::    =/  shel=shell:smart
::      [caller-1 ~ id.p:scry-wheat 1 1.000.000 town-id 0]
::    =/  res=[fee=@ud =land burned=granary =errorcode:smart hits=(list) =crow:smart]
::      %+  ~(mill mil miller town-id 1)
::      fake-land  `egg:smart`[fake-sig shel yolk]
::    ~&  >  "output: {<crow.res>}"
::    ~&  >  "fee: {<fee.res>}"
::    ::  assert that our call went through
::    %+  expect-eq
::      !>(%0)
::    !>(errorcode.res)
--