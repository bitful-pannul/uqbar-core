/+  *zig-sys-smart
|%
+$  token
  $:  contract=id
      metadata=id
      amount=@ud
      account=id  ::  owner puts their token account here to get paid
  ==
::
+$  subscription
  $:  name=@t
      owner=ship
      =token
      subs=(pmap ship @ud)  ::  map of ship to ETH block expiration
  ==
::
+$  action
  $%  $:  %launch
          name=@t
          owner=ship
          =token
          sub-length=@ud  ::  # of ETH blocks of access that price buys you
      ==
  ::
      $:  %edit
          subscription=id
          new-amount=(unit @ud)
          new-sub-length=(unit @ud)
      ==
  ::
      $:  %sub
          subscription=id
          =ship
          amount=@ud  ::  must be an exact multiple of amount in price
          token-account=id
      ==
  ==
--
