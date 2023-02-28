::  subscription.hoon [UQ| DAO]
/+  *zig-sys-smart
/=  lib  /con/lib/subscription
|_  =context
++  write
  |=  =action:lib
  ^-  (quip call diff)
  ?-    -.action
      %launch
    ::  create a new subscription item with the specified params,
    ::  and the caller as holder (so they can edit)
    =/  salt=@
      (cat 3 [name owner]:action)
    =/  =id
      (hash-data this.context address.caller.context town.context salt)
    =-  `(result ~ [%&^- ~] ~ [[%new-bond id.-]]^~)
    :*  id
        this.context
        address.caller.context
        town.context
        salt
        %subscription
        ^-  subscription:lib
        :_  ~
        [name owner token]:action
    ==
  ::
      %edit
    ::  edit an existing subscription item, caller must be holder
    =/  sub
      =+  (need (scry-state subscription.act))
      (husk subscription:lib - `this.context `address.caller.context)
    =?    amount.sub
        ?=(^ new-amount.action)
      u.new-amount.action
    =?    sub-length.sub
        ?=(^ new-sub-length.action)
      u.new-sub-length.action
    !!
  ::
      %sub
    !!
  ==
::
++  read
  |=  =pith
  ~
--
