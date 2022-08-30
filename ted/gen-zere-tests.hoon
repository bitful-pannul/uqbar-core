/-  spider, test=zere-test
/+  zere-tests, strandio
::
=*  strand     strand:spider
|%
+$  card  card:agent:gall
++  put-json
  |=  [=path =json]
  =/  m  (strand ,~)
  ^-  form:m
  %^  poke-our:strandio
      %hood
    %drum-put
  !>([path (crip (en-json:html json))])
--
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
;<  ~  bind:m  (put-json /zere-tests/json zere-tests)
(pure:m !>(~))