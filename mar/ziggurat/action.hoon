/-  *zig-ziggurat
=,  dejs:format
|_  act=action
++  grab
  |%
  ++  noun  action
  ++  json
    |=  jon=^json
    ^-  action
    %-  action
    |^
    (guh jon)
    ++  guh
      %-  ot
      :~  [%project so]
          [%action process]
      ==
    ++  process
      %-  of
      :~  [%new-project ul]
          [%delete-project ul]
      ::
          [%save-file (ot ~[[%file pa] [%text so]])]
          [%delete-file (ot ~[[%file pa]])]
      ::
          [%register-contract-for-compilation (ot ~[[%file pa]])]
          [%deploy-contract parse-deploy]
      ::
          [%compile-contracts ul]
          [%compile-contract (ot ~[[%path pa]])]
          [%read-desk ul]
      ::
          [%add-test parse-add-test]
          [%delete-test (ot ~[[%id (se %ux)]])]
          [%run-test (ot ~[[%id (se %ux)]])]
          [%add-and-run-test parse-add-test]
          [%run-queue ul]
          [%clear-queue ul]
          [%queue-test (ot ~[[%id (se %ux)]])]
          [%add-and-queue-test parse-add-test]
      ::
          [%add-custom-step parse-add-custom-step]
          [%delete-custom-step (ot ~[[%test-id (se %ux)] [%tag (se %tas)]])]
      ::
          [%add-app-to-dashboard parse-add-app-to-dashboard]
          [%delete-app-from-dashboard (ot ~[[%app (se %tas)]])]
      ::
          [%add-town-sequencer (ot ~[[%town-id (se %ux)] [%who (se %p)]])]
          [%delete-town-sequencer (ot ~[[%town-id (se %ux)]])]
      ::
          [%stop-pyro-ships ul]
          [%start-pyro-ships (ot ~[[%ships (ar (se %p))]])]
      ::
          [%publish-app parse-docket]
          [%add-user-file (ot ~[[%file pa]])]
          [%delete-user-file (ot ~[[%file pa]])]
      ==
    ::
    ++  parse-docket
      %-  ot
      :~  [%title so]
          [%info so]
          [%color (se %ux)]
          [%image so]
          [%version (at ~[ni ni ni])]
          [%website so]
          [%license so]
      ==
    ::
    ++  parse-test
      %-  ot
      :~  [%id (se %ux)]
          [%rate ni]
          [%bud ni]
      ==
    ::
    ++  parse-deploy
      %-  ot
      :~  [%town-id (se %ux)]
          [%path pa]
      ==
    ::
    ++  parse-add-test
      %-  ot
      :+  [%name so:dejs-soft:format]
        [%path pa]
      ~
    ::
    ++  parse-test-step
      %-  of
      %+  welp  parse-test-read-step-inner
      parse-test-write-step-inner
    ::
    ++  parse-test-read-step
      (of parse-test-read-step-inner)
    ::
    ++  parse-test-read-step-inner
      :~  [%scry (ot ~[[%payload parse-scry-payload] [%expected so]])]
          [%dbug (ot ~[[%payload parse-dbug-payload] [%expected so]])]
          [%read-subscription (ot ~[[%payload parse-read-sub-payload] [%expected so]])]
          [%wait (ot ~[[%until (se %dr)]])]
          [%custom-read (ot ~[[%tag (se %tas)] [%payload so] [%expected so]])]
      ==
    ::
    ++  parse-scry-payload
      %-  ot
      :~  [%who (se %p)]
          [%mold-name so]
          [%care (se %tas)]
          [%app (se %tas)]
          [%path pa]
      ==
    ::
    ++  parse-dbug-payload
      %-  ot
      :^    [%who (se %p)]
          [%mold-name so]
        [%app (se %tas)]
      ~
    ::
    ++  parse-read-sub-payload
      %-  ot
      :~  [%who (se %p)]
          [%to (se %p)]
          [%app (se %tas)]
          [%path pa]
      ==
    ::
    ++  parse-test-write-step
      (of parse-test-write-step-inner)
    ::
    ++  parse-test-write-step-inner
      :~  [%dojo (ot ~[[%payload parse-dojo-payload] [%expected (ar parse-test-read-step)]])]
          [%poke (ot ~[[%payload parse-poke-payload] [%expected (ar parse-test-read-step)]])]
          [%subscribe (ot ~[[%payload parse-subscribe-payload] [%expected (ar parse-test-read-step)]])]
          [%custom-write (ot ~[[%tag (se %tas)] [%payload so] [%expected (ar parse-test-read-step)]])]
      ==
    ::
    ++  parse-dojo-payload
      %-  ot
      :+  [%who (se %p)]
        [%payload so]
      ~
    ::
    ++  parse-poke-payload
      :~  [%who (se %p)]
          [%app (se %tas)]
          [%mark (se %tas)]
          [%payload so]
      ==
    ::
    ++  parse-subscribe-payload
      :~  [%who (se %p)]
          [%to (se %p)]
          [%app (se %tas)]
          [%path pa]
      ==
    ::
    ++  parse-add-custom-step
      :^    [%test-id (se %ux)]
          [%tag (se %tas)]
        [%path pa]
      ~
    ::
    ++  parse-add-app-to-dashboard
      :~  [%app (se %tas)]
          [%sur pa]
          [%mold-name so]
          [%mar pa]
      ==
    --
  --
++  grow
  |%
  ++  noun  act
  --
++  grad  %noun
--
