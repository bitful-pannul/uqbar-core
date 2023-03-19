/+  *zig-sys-smart
|%
+$  feed
  $:  name=@t
      price=@ud        ::  gas fee for scrying out data
      threshold=@ud    ::  number of ships that must contribute to update
      last-update=@ud  ::  eth block when data was last updated
      feed-data=@      ::  averaged out from votes
  ==
::
::  typed message: domain  = (cat 3 [this contract address] [feed-name])
::                 type    = eth-block vote is valid until
::                 message = feed-data vote
::
+$  vote  [typed-message =sig]
::
+$  action
  $%  [%make-new-feed name=@t price=@ud threshold=@ud]
      [%contribute-data feed-id=id votes=(map address vote)]
  ==
--