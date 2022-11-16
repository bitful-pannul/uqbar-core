/-  *zink
|%
++  enjs
  =,  enjs:format
  |%
  ++  book
    |=  ^book
    =+  (arena arena.q cax.q)
    %-  pairs
    :~  arena+arena
        nindex+nindex
        zint+(zint q.p)
    ==
  ::
  ++  arena
    |=  [ar=^arena cax=cache]
    |^  ^-  [nindex=json arena=json]
    =<  [a+nin o+ar]
    %+  roll  sorted
    |=  [[n=* t=^tnoun] nin=(list json) ar=(map @t json)]
    =/  v  (~(got by cax) n)
    =/  jv  (num h.v)
    :*  [jv^nin]
        (~(put by ar) p.jv (tnoun t))
    ==
    ::
    ++  sorted
      ^-  (list [n=* t=^tnoun])
      %+  sort  ~(tap by ar)
      |=  [[a=* *] [b=* *]]
      =/  va  (~(got by cax) a)
      =/  vb  (~(got by cax) b)
      (lth d.va d.vb)
   ::
   --
  ++  tnoun
    |=  ^tnoun
    ^-  json
    ?-  +<
        [%cat *]
      (pairs cat+(num a) ~)
        [%pom *]
      (pairs pom+a+~[(num hh) (num ht)] ~)
        %bun
      ~
    ==
  ::
  ++  uzint
    |=  z=^uzint
    ^-  json
    ?~  z  ~
    (zint u.z)
  ::
  ++  zints
    |=  h=(list ^zint)
    ^-  json
    a+(turn h zint)
  ::
  ++  zint
    |=  hin=^zint
    ^-  json
    :: ?:  &(?=(_-:*$<(?(%1 %cons %invalid %jet) zint) -.hin) ?=(%| +<.hin))
    ::    (frond %broke (num p.hin))
    ::  for some reason previous conditional doesn't assert %& case here
    ::  it should mint-vain, but doesn't
    ?+  hin  !!
        [%0 * *]
      %-  pairs
      :~  'jmp_dest'^(num 0)
          pred+(en-pred pred.hin)
          path+(en-path path.hin)
      ==
        [%1 *]  (pairs 'jmp_dest'^(num 1) pred+(en-pred +.hin) ~)
    ::
        [%2 * * * *]
      %-  pairs
      :~  'jmp_dest'^(num 2)
          pred+(en-pred pred.hin)
          sf1+(uzint sf1.hin)
          sf2+(uzint sf2.hin)
          sf3+(uzint sf3.hin)
      ==
    ::     
        [?(%3 %4) * *]
      %-  pairs
      :~  'jmp_dest'^(num -.hin)
          pred+(en-pred pred.hin)
          sf+(uzint sf.hin)
      ==
    ::
        [?(%5 %6 %7 %8) * * *]
      %-  pairs
      :~  'jmp_dest'^(num -.hin)
          pred+(en-pred pred.hin)
          sf1+(uzint sf1.hin)
          sf2+(uzint sf2.hin)
      ==
    ::
        [%9 * * * *]
      %-  pairs
      :~  'jmp_dest'^(num 9)
          pred+(en-pred pred.hin)
          sf1+(uzint sf1.hin)
          sf2+(uzint sf2.hin)
          leaf+(num leaf.hin)
          path+(en-path path.hin)
      ==
    ::
        [%10 * * * * *]
      %-  pairs
      :~  'jmp_dest'^(num 10)
          pred+(en-pred pred.hin)
          sf1+(uzint sf1.hin)
          sf2+(uzint sf2.hin)
          oldleaf+(num old-leaf.hin)
          path+(en-path-10 path.hin)
      ==
    ::
        [%11 * * %| @]
      %-  pairs
      :~  'jmp_dest'^(num 11)
          pred+(en-pred pred.hin)
          sf+(uzint sf.hin)
          tag+(num +>+>:hin)
      ==
    ::
        [%11 * * %& *]
      %-  pairs
      :~  pred+(en-pred pred.hin)
          sf+(uzint sf.hin)
          tag+(num +>+>-:hin)
          clue+(uzint +>+>+:hin)
      ==
    ::
        [%cons *]
      %-  pairs
      :~  'jmp_dest'^s+%cons
          pred+(en-pred pred.hin)
          sf1+(uzint sf1.hin)
          sf2+(uzint sf2.hin)
      ==
        [%memo *]
      %-  pairs
      :~  'jmp_dest'^s+%memo
          pred+(en-pred pred.hin)
      ==
    ==
    ::
  ++  en-jet
    |=  n=(list @tas)
    ^-  json
    :-  %s
    |-  ^-  cord
    ?~  n  !!
    ?:  ?=([* ~] n)  ?~(i.n '' (scot %tas i.n))
    ?>  ?=(@ i.n)
    ?~  i.n  $(n t.n)
    :((cury cat 3) (scot %tas i.n) ':' $(n t.n))
  ::
  ++  raw-noun
    |=  n=*
    |-  ^-  json
    ?@  n  (num n)
    a+[$(n -.n) $(n +.n) ~]
  ::
  ++  en-11
    |=  [=pred sf=^uzint a=(each [tag=@ clue=^uzint] @)]
    ^-  json
    ?-  -.a
      %&  (pairs pred+(en-pred pred) tag+(num tag.p.a) clue+(uzint clue.p.a) sf+(uzint sf) ~)
      %|  (pairs pred+(en-pred pred) tag+(num p.a) sf+(uzint sf) ~)
    ==
  ::
  ++  en-invalid
    |=  hin=(each @ [@ phash])
    ^-  json
    ?-  -.hin
      %&  (pairs is-atom+b+%& head+(num p.hin) ~)
      %|  (pairs is-atom+b+%| head+(num -.p.hin) tail+(num +.p.hin) ~)
    ==
  ::
  :: ++  en-subf
  ::   |=  subf
  ::   ^-  json
  ::   (pairs hash+(num h) hints+(hints hit) ~)
  ::
  :: ++  en-hash-req
  ::   |=  hash-req
  ::   ^-  json
  ::   ?-  +<-
  ::     %cell  (pairs head+(num head) tail+(num tail) ~)
  ::     %atom  (frond %atom (num val))
  ::   ==
  ::
  ++  en-leaf-or-atom
    |=  hin=(each phash [=atom crash-axis=@])
    ^-  json
    =,  p.hin
    ?-  -.hin
      %&  (frond %leaf (num p.hin))
      %|  (pairs atom+(num atom) crash-axis+(num crash-axis) ~)
    ==
  ::
  ++  en-path
    |=  path=(list (pair ?(%2 %3) phash))
    a+(turn path en-path-elem)
  ::
  ++  en-path-elem
    |=  a=(pair @ud phash)
    ^-  json
    a+[(num p.a) (num q.a) ~]
  ::
  ::  todo: less dumb
  ++  en-path-10
    |=  path=(list (trel ?(%2 %3) phash phash))
    a+(turn path en-path-elem-10)
  ::
  ++  en-path-elem-10
    |=  a=(trel ?(%2 %3) phash phash)
    ^-  json
    a+[(num p.a) (num q.a) (num r.a) ~]
 
  ::
  ++  en-pred
    |=  =pred
    ^-  json
    a+[(num s.pred) (num f.pred) (num p.pred) ~]
  ::
  ++  num
    |=  n=@ud
    ^-  [%s p=@t]
    [%s `cord`(rsh [3 2] (scot %ui n))]
  --
--
