/-  *zink
|%
++  enjs
  |%
  ++  hints
    |=  h=^hints
    ^-  json
    a+(turn h en-hint)
  ::
  ++  en-hint
    |=  hin=cairo-hint
    ^-  json
    =,  enjs:format
    :-  %a
    ^-  (list json)
    :-  ?:(?=(?(%cons %invalid %jet) -.hin) [%s -.hin] (num -.hin))
    :_  ~
    ?:  &(?=(_-:*$<(?(%1 %cons %invalid %jet) cairo-hint) -.hin) ?=(%| +<.hin))
        (frond %broke (num p.hin))
    ::  for some reason previous conditional doesn't assert %& case here
    ::  it should mint-vain, but doesn't
    ?+  hin  !!
        [%0 %& *]
      %-  pairs
      :~  axis+(num axis.p.hin)
          leaf-or-atom+(en-leaf-or-atom leaf-or-atom.p.hin)
          path+(en-path path.p.hin)
      ==
    ::
        [%1 *]  (num +.hin)
        [?(%2 %5 %12) %& *]   (pairs sf1+(en-subf sf1.p.hin) sf2+(en-subf sf2.p.hin) ~)
        [?(%3 %4) %& *]
      ?~  sf-res.p.hin  (frond %sf (en-subf sf.p.hin))
      %-  pairs
      :~  sf+(en-subf sf.p.hin)
          sf-res+(en-hash-req u.sf-res.p.hin)
      ==
    ::
        [%6 %& *]  (pairs sf1+(en-subf sf1.p.hin) sf2+(num sf2.p.hin) sf3+(num sf3.p.hin) ~)
        [?(%7 %8) %& *]  (pairs sf1+(en-subf sf1.p.hin) sf2+(num sf2.p.hin) ~)
        [%9 %& *]
      %-  pairs
      :~  axis+(num axis.p.hin)
          sf+(en-subf sf.p.hin)
          leaf-or-atom+(en-leaf-or-atom leaf-or-atom.p.hin)
          path+(en-path path.p.hin)
      ==
    ::
        [%10 %& *]
      %-  pairs
      :~  axis+(num axis.p.hin)
          sf1+(en-subf sf1.p.hin)
          sf2+(en-subf sf2.p.hin)
          old-leaf-or-atom+(en-leaf-or-atom old-leaf-or-atom.p.hin)
          path+(en-path path.p.hin)
      ==
    ::
        [%11 %& *]  (en-11 p.hin)
    ::
        [%jet *]
        %-  pairs
        :~  jet+(en-jet jet.hin)
            arg+(en-noun data.hin)
        ==
    ::
        [%cons *]  (pairs sf1+(en-subf sf1.hin) sf2+(en-subf sf2.hin) ~)
        [%invalid *]  (en-invalid +.hin)
    ==
    ::
  ++  en-jet
    |=  n=*
    ^-  json
    :-  %s
    |-  ^-  cord
    ?@  n  ?~(n '' (scot %tas n))
    ?>  ?=(@ -.n)
    ?~  -.n  $(n +.n)
    :((cury cat 3) (scot %tas -.n) ':' $(n +.n))
  ::
  ++  en-noun
    |=  n=*
    |-  ^-  json
    ?@  n  (num n)
    a+[$(n -.n) $(n +.n) ~]
  ++  en-11
    |=  [a=(each [tag=@ clue=subf] @) next=phash]
    ^-  json
    =,  enjs:format
    ?-  -.a
      %&  (pairs tag+(num tag.p.a) clue+(en-subf clue.p.a) sf+(num next) ~)
      %|  (pairs tag+(num p.a) sf+(num next) ~)
    ==

  ++  en-invalid
    |=  hin=(each @ [@ phash])
    ^-  json
    =,  enjs:format
    ?-  -.hin
      %&  (pairs is-atom+b+%& head+(num p.hin) ~)
      %|  (pairs is-atom+b+%| head+(num -.p.hin) tail+(num +.p.hin) ~)
    ==
  ++  en-subf
    |=  subf
    ^-  json
    =,  enjs:format
    (pairs hash+(num h) hints+(hints hit) ~)
  ::
  ++  en-hash-req
    |=  hash-req
    ^-  json
    =,  enjs:format
    ?-  +<-
      %cell  (pairs head+(num head) tail+(num tail) ~)
      %atom  (frond %atom (num val))
    ==
  ::
  ++  en-leaf-or-atom
    |=  hin=(each phash [=atom crash-axis=@])
    ^-  json
    =,  enjs:format
    =,  p.hin
    ?-  -.hin
      %&  (frond %leaf (num p.hin))
      %|  (pairs atom+(num atom) crash-axis+(num crash-axis) ~)
    ==
  ::
  ++  en-path
    |=  path=(list phash)
    a+(turn path num)
  ::
  ++  num
    |=  n=@ud
    ^-  json
    [%s `cord`(rsh [3 2] (scot %ui n))]
  --
--
