/-  ui=indexer,
    zig=ziggurat
/+  ui-lib=indexer
::
|_  headers=(list [epoch-num=@ud =block-header:zig])
+$  headers-mold  (list [epoch-num=@ud =block-header:zig])
++  grab
  |%
  ++  noun  headers-mold
  --
::
++  grow
  |%
  ++  noun  headers
  ++  json  (headers:enjs:ui-lib headers)
  --
::
++  grad  %noun
::
--
