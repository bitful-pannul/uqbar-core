::  orgs.hoon [UQ| DAO]
::
::  manage an organization on-chain
::
::  this contract is designed to emit updates to a %social-graph.
::  NOT FOR DAOs! DAOs should be self-governing. orgs.hoon allows
::  users to create user-controlled organizations. control of the
::  org is delegated to an id, which can be a multisig contract,
::  some person's address, or something else entirely...
::
::  why not generic? one likely wants to subscribe to events from
::  one contract per organization. in the future, watching events
::  associated with one item in particular could be easy, in which
::  case, can genericize this easily.
::
/+  *zig-sys-smart
/=  lib  /con/lib/orgs
|_  =context
++  write
  |=  act=action:lib
  ^-  (quip call diff)
  ?:  ?=(%create -.act)
    ::  called by publish contract: %deploy-and-init
    ?<  ?=(%deleted org.act)
    =/  =item
      :*  %&
          %:  hash-data
              this.context
              controller.org.act
              town.context
              name.org.act
          ==
          this.context
          controller.org.act
          town.context
          name.org.act
          %org
          org.act
      ==
    =-  `(result ~ [item ~] ~ -)
    (produce-org-events:lib / org.act)
  ::
  =/  org
    =+  (need (scry-state org-id.act))
    (husk org:lib - `this.context ~)
  ::  if org has been deleted, crash
  ?<  ?=(%deleted noun.org)
  ::  caller must control identified org
  ?>  =(id.caller.context controller.noun.org)
  =^  events  noun.org
    =-  ?<  ?=(%deleted +.-)  -
    ?-    -.act
        %edit-org
      :-  ~
      %^  modify-org:lib
        noun.org  where.act
      |=  =org:lib
      ?<  ?=(%deleted org)
      %=    org
          desc
        ?~(desc.act desc.org desc.act)
          controller
        ?~(controller.act controller.org u.controller.act)
      ==
    ::
        %add-sub-org
      ?<  ?=(%deleted org.act)
      :-  (produce-org-events:lib where.act org.act)
      %^  modify-org:lib
        noun.org  where.act
      |=  =org:lib
      ?<  ?=(%deleted org)
      =-  org(sub-orgs -)
      (~(put py sub-orgs.org) [name.org org]:act)
    ::
        %delete-org
      :-  (nuke-tag:lib where.act)
      %^  modify-org:lib
        noun.org  where.act
      |=(=org:lib %deleted)
    ::
        %replace-members
      :-  %+  weld  (nuke-tag:lib where.act)
          (make-tag:lib where.act name.noun.org new.act)
      %^  modify-org:lib
        noun.org  where.act
      |=  =org:lib
      ?<  ?=(%deleted org)
      org(members new.act)
    ::
        %add-member
      :-  (add-tag:lib where.act name.noun.org address.act)
      %^  modify-org:lib
        noun.org  where.act
      |=  =org:lib
      ?<  ?=(%deleted org)
      org(members (~(put pn members.org) address.act))
    ::
        %del-member
      :-  (del-tag:lib where.act name.noun.org address.act)
      %^  modify-org:lib
        noun.org  where.act
      |=  =org:lib
      ?<  ?=(%deleted org)
      org(members (~(del pn members.org) address.act))
    ==
  `(result [&+org ~] ~ ~ events)
::
++  read
  |=  =pith
  ~  ::  TODO
--
