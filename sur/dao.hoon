/-  r=resource
/+  smart=zig-sys-smart
::
|%
+$  role         @tas  ::  E.g. %marketing, %development
+$  address      ?(id:smart resource:r)  ::  [chain=@tas id:smart] for other chains?
::  name might be, e.g., %read or %write for a graph;
::  %spend for treasury/rice
+$  permissions  (map name=@tas (jug address role))
+$  members      (jug ship role)
+$  id-to-ship   (map id:smart ship)
+$  ship-to-id   (map ship id:smart)
+$  dao
  $:  name=@t
      =permissions
      =members
      =id-to-ship
      =ship-to-id
      subdaos=(set id:smart)
      :: owners=(set id:smart)  ::  ? or have this in permissions?
      threshold=@ud
      proposals=(map @ux [update=on-chain-update votes=(set id:smart)])
  ==
::
+$  on-chain-update
  $%  [%add-dao =dao]
      [%remove-dao ~]
      [%add-member roles=(set role) =id:smart him=ship]
      [%remove-member him=ship]
      [%add-permissions name=@tas =address roles=(set role)]
      [%remove-permissions name=@tas =address roles=(set role)]
      [%add-subdao subdao-id=id:smart]
      [%remove-subdao subdao-id=id:smart]
      [%add-roles roles=(set role) him=ship]
      [%remove-roles roles=(set role) him=ship]
  ==
::  off-chain
::
+$  off-chain-update
  $%  [%on-chain dao-id=id:smart update=on-chain-update]
      [%add-comms dao-id=id:smart rid=resource:r]
      [%remove-comms dao-id=id:smart]
  ==
::
+$  daos  (map id:smart dao)
+$  dao-id-to-rid  (map id:smart resource:r)
+$  dao-rid-to-id  (map resource:r id:smart)
--
