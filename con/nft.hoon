::  nft.hoon [UQ| DAO]
::
::  NFT standard. Provides abilities similar to ERC-721 tokens, also ability
::  to deploy and mint new sets of tokens.
::
/+  *zig-sys-smart
/=  nft  /con/lib/nft
=,  nft
|_  =context
++  write
  |=  act=action:sur
  ^-  (quip call diff)
  ?-  -.act
    %give           (give:lib:nft context act)
    %take           (take:lib:nft context act)
    %set-allowance  (set-allowance:lib:nft context act)
    %mint           (mint:lib:nft context act)
    %deploy         (deploy:lib:nft context act)
  ==
::
++  read
  |=  =pith
  ~
--
