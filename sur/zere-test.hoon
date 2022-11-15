/-  zink
|%
+$  test-args  [bud=(unit @ud) scrys=(list *) s=* f=*]
+$  test-hints  $~  [%& *json]  (each json (list json))
+$  named-test  $-([cache:zink (unit @t)] [json cache:zink]) 
--
