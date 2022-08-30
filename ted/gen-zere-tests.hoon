/-  spider, test=zere-test
/+  zink-tests, strandio
::
=*  strand     strand:spider
|%
+$  card  card:agent:gall
++  put-fils
  |=  fils=test-fils:test
  =/  m  (strand ,~)
  ^-  form:m
  ;<  our=@p  bind:m  get-our:strandio
  %-  send-raw-cards:strandio
  %+  turn  fils
  |=  test-fil:test
  ^-  card
  [%pass /put %agent [our %hood] %poke %drum-put !>([%zere-tests^fil (crip (en-json:html jon))])]
--
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
;<  ~  bind:m  (put-fils zink-tests)
(pure:m !>(~))