|%
+$  field
  $:  size=@ud
      exp=@
      log=@
  ==
::
++  generate-field
  ~/  %generate-field
  |=  size=@
  ^-  field
  =+  [i=0 exp=*@ log=*@ x=1]
  |-  ^+  [size exp log]
  ?:  =(i (dec size))
    [size (cat 3 exp exp) log]
  ?:  =(i 0)
    %=  $
        i  +(i)
        exp  (cat 3 1 exp)
    ==
  =.  x
    =/  x  (lsh 0 x)
    ?.  =((dis x size) 0)
      (mix x (con size 0x1D))
    x
  %=  $
      i  +(i)
      exp  (cat 3 exp x)
      log  (sew 3 [x 1 i] log)
  ==
::
::  Galois math
::
++  galois
  |_  f=field
  ++  add
    mix
  ::
  ++  mul
    |=  [x=@ y=@]
    ^-  @
    ?:  ?|  =(x 0)
            =(y 0)
        ==
      0
    %^  cut  3
      :_  1
      %+  ^add
        (cut 3 [x 1] log.f)
      (cut 3 [y 1] log.f)
    exp.f
  ::
  ++  mod-2
    |=  x=@
    ^-  @
    (rsh [0 (dec (met 0 x))] x)
  ::
  ++  div
    |=  [x=@ y=@]
    ^-  @
    ::  reject attempt to divide by 0
    ?<  =(y 0)
    ?:  =(x 0)
      0
    %^  cut  3
      :_  1
      %+  mod
        %+  sub
          (^add (cut 3 [x 1] log.f) 255)
        (cut 3 [y 1] log.f)
      255
    exp.f
  ::
  ++  pow
    |=  [x=@ power=@]
    ^-  @
    %^  cut  3
      :_  1
      %+  mod
        (^mul (cut 3 [x 1] log.f) power)
      255
    exp.f
  ::
  ++  inv
    |=  [x=@]
    ^-  @
    %^  cut  3
      :_  1
      (sub 255 (cut 3 [x 1] log.f))
    exp.f
  ::
  ::  Reed-Solomon decoding utils
  ::  Only built to handle erasures, not detect errors
  ::
  ++  poly-add
    ~/  %gf-poly-add
    |=  [p=(list @) q=(list @)]
    ^-  (list @)
    =/  [longer=(list @) shorter=(list @)]
      ?:  %+  gth  (lent p)  (lent q)
        [p q]
      [q p]
    =/  diff
      %+  sub
        (lent longer)
      (lent shorter)
    %+  welp
      %+  scag
        diff
      longer
    =<  p
    %^    spin
        %+  slag
          diff
        longer
      shorter
    |=  [i=@ud r=(list @)]
    [(mix i -.r) +.r]
  ::
  ++  poly-add-bytes
    mix
  ::
  ++  poly-mul
    ~/  %gf-poly-mul
    |=  [p=(list @) q=(list @)]
    ^-  (list @)
    =<  q
    %^    spin
        (gulf 0 (sub (lent q) 1))
      (reap (sub (^add (lent p) (lent q)) 1) 0)
    |=  [j=@ r=(list @)]
    =<  q
    %^    spin
        (gulf 0 (sub (lent p) 1))
      [j r]
    |=  [i=@ [j=@ r=(list @)]]
    =/  a
      %+  add
        %+  snag
          (^add i j)
        r
      %+  mul
        (snag i p)
      (snag j q)
    :+  i
      j
    (snap r (^add i j) a)
  ::
  ++  poly-mul-bytes
    |=  [p=@ q=@]
    ^-  @
    =/  el-p  (met 3 p)
    =/  el-q  (met 3 q)
    =+  [j=0 i=0 result=*@]
    |-  ^-  @
    ?:  =(j el-q)
      result
    =.  result
      |-  ^-  @
      ?:  =(i el-p)
        result
      =.  result
        %+  mix
          result
        %+  ^mul
          (bex (^mul (^add i j) 8))
        %+  mul
          (cut 3 [i 1] p)
        (cut 3 [j 1] q)
      $(i +(i))
    $(j +(j))
  ::
  ++  poly-eval
    ~/  %gf-poly-eval
    |=  [p=(list @) x=@]
    ^-  @
    =<  q
    %^    spin
        `(list @)`+.p
      -.p
    |=  [i=@ y=@]
    [i (mix (mul y x) i)]
  ::
  ++  poly-eval-bytes
    |=  [p=@ x=@]
    ^-  @
    =+  [i=1 y=(end [3 1] p)]
    |-  ^-  @
    ?:  =(i (met 3 p))
      y
    %=    $
      i  +(i)
    ::
        y
      %+  mix
        (mul y x)
      (cut 3 [i 1] p)
    ==
  --
--
