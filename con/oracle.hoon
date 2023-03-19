::  oracle.hoon [UQ| DAO]
::
::  contract for producing data feeds.
::
/+  *zig-sys-smart
/=  lib  /con/lib/oracle
=,  lib
|_  =context
++  write
  |=  act=action
  ^-  (quip call diff)
  ?-    -.act
      %make-new-feed
    ::  feed name acts as salt so that scry path can be locked to one feed
    =+  id=(hash-data this.context this.context town.context name.act)
    =/  =item
      =+  [name.act price.act threshold.act 0 ~]
      [%& id this.context this.context town.context name.act %feed -]
    `(result ~ [item ~] ~ ~)
  ::
      %contribute-data
    ::  must have number of votes greater than or equal to threshold,
    ::  each vote must be valid: domain is correct, sig is valid, eth-block
    ::  value stored in `type` must be higher than current eth-block
    ::
    ::  votes are *averaged*, bottom 25% and top 75% rejected
    ::  TODO reward good votes and punish bad votes
    =+  (need (scry-state feed-id.act))
    =/  feed  (husk feed - [`this `this]:context)
    =/  votes=(list [=address vote])  ~(tap by votes.act)
    =/  domain  (cat 3 this.context name.noun.feed)
    =/  new-feed-data=@
      =+  i=0
      =+  avg=feed-data.noun.feed
      |-
      ?~  votes
        ?>  (gth i threshold.noun.feed)
        (div avg +(i))
      ?.  ?&  =(domain domain.i.votes)
              (gth type.i.votes eth-block.context)
              =(address.i.votes (recover +.i.votes))
              ?=(@ message.i.votes)
          ==
        $(votes t.votes)
      $(avg (add avg message.i.votes), i +(i), votes t.votes)
    `(result [%&^feed(feed-data.noun new-feed-data) ~] ~ ~ ~)
  ==
::
++  read
  |=  =pith
  ?+    pith  !!
      ::  /oracle/[price]/[feed-name]
      ::  always returns a (unit atom)
      [%oracle [%ud @ud] [%t @t] ~]
    =/  price=@ud     +.i.t.pith
    =/  feed-name=@t  +.i.t.t.pith
    =+  id=(hash-data this.context this.context town.context feed-name)
    ?~  item=(scry-state id)  ~
    `feed-data.noun:(husk feed u.item [`this `this]:context)
  ==
--
