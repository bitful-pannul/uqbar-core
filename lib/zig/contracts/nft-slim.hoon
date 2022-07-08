::  nft-slim.hoon [UQ| DAO]
::
::  Slimmed down version of the NFT standard.
::  Allows deploying and mint new sets of tokens.
::
  /+  *zig-sys-smart
/=  nft  /lib/zig/contracts/lib/nft-slim
=,  nft
::  TODO we are definitely removing accounts for simplicity
|_  =cart
++  write
  |=  inp=embryo
  ^-  chick
  |^
  ?~  args.inp  !!
  (process ;;(action u.args.inp) (pin caller.inp))
  ::
  ++  process
    |=  [args=action caller-id=id]
    ?-    -.args
        %give
      =/  item=grain  -:~(val by grains.inp)
      ?>  &(=(lord.giv me.cart) ?=(%& -.germ.giv))
      ?>  =(caller-id holder.item)
      ?>  transferrable.item
      =.  holder.item  to.args
      [%& (malt ~[[id.item item]]) ~ ~]
    ::
        %mint
      ::  expects token metadata in owns.cart
      =/  tok=grain  (~(got by owns.cart) meta.args)
      ?>  &(=(lord.tok me.cart) ?=(%& -.germ.tok))
      =/  meta  ;;(collection-metadata data.p.germ.tok)
      ::  first, check if token is mintable
      ?>  &(mintable.meta ?=(^ cap.meta) ?=(^ minters.meta))
      ?>  (~(has in minters.meta) caller-id)
      ::  check if mint will surpass supply cap
      ?>  (gth u.cap.meta (add supply.meta ~(wyt in mints.args)))
      ::  cleared to execute!
      =/  next-item-id  supply.meta
      ::  for accounts which we know rice of, find in owns.cart
      ::  and alter. for others, generate id and add to c-call
      =/  changed-rice  (malt ~[[id.tok tok]])
      =/  issued-rice   *(map id grain)
      =/  mints         ~(tap in mints.args)
      =/  next-mints    *(set mint)
      |-
      ?~  mints
        *chick
      *chick
      ::  basically, we need to recurse over mints and
      ::  a) create item grains for all items in items.i.mints
      ::  b) make sure their holder is set to to.i.mints
      ::  c) update the supply/cap/mintable as per old logic
    ::
        %deploy
      ::  no rice expected as input, only arguments
      ::  if mintable, enforce minter set not empty
      ?>  ?:(mintable.args ?=(^ minters.args) %.y)
      ::  if !mintable, enforce distribution adds up to cap
      ::  otherwise, enforce distribution < cap
      =/  distribution-total=@ud 
        ~(wyt in items.distribution.args)
      ?>  ?:  mintable.args
            (gth cap.args distribution-total)
          =(cap.args distribution-total)
      ::  generate salt
      =/  salt  (sham (cat 3 caller-id symbol.args))
      ::  create metadata
      =/  metadata-grain  ^-  grain
        :*  (fry-rice me.cart me.cart town-id.cart salt)
            me.cart
            me.cart
            town-id.cart
            :+  %&  salt
            ^-  collection-metadata
            :*  name.args
                symbol.args
                supply=distribution-total
                ?:(mintable.args `cap.args ~)
                mintable.args
                minters.args
                deployer=caller-id
                salt
        ==  ==
      [%& ~ (malt ~[[id.metadata-grain metadata-grain]]) ~]
    ==
  --
::
::  TODO 
++  read
  |_  args=path
  ++  json
    ~
  ::
  ++  noun
    ?+    args  !!
        [%collection-data ~]
      ?>  =(1 ~(wyt by owns.cart))
      =/  g=grain  -:~(val by owns.cart)
      ?>  ?=(%& -.germ.g)
      ;;(collection-metadata data.p.germ.g)
    ::
        [%account-data ~]
      ?>  =(1 ~(wyt by owns.cart))
      =/  g=grain  -:~(val by owns.cart)
      ?>  ?=(%& -.germ.g)
      ;;(nft-account data.p.germ.g)
    ==
  --
--
