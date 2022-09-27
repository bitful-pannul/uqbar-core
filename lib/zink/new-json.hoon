/-  *new-zink
|%
++  enjs
  |%
  ++  nouns
    |=  c=cache
    ^-  json
    :-  %a
    %+  turn
    %+  sort  (turn ~(tap by c) tail)
    |=  [a=(pair tnoun index) b=(pair tnoun index)]
    (lth q.a q.b)
    en-noun
  ++  en-noun
    |=  n=(pair tnoun index)
    ^-  json
    =,  enjs:format
    ?-  -.p.n
        %atom
      (pairs atom+(num +.p.n) i+(num q.n) ~)
        %cell
      %-  pairs 
      :~  cell+(pairs head+(num +<.p.n) tail+(num +>.p.n) ~)
          i+(num q.n)
      ==
    ==
  ++  hints
    |=  h=^hints
    ^-  json
    a+(turn h en-hint)
  ::
  ++  en-hint
    |=  hin=cairo-hint
    ^-  json
    =,  enjs:format
    :: ?:  &(?=(_-:*$<(?(%1 %cons %invalid %jet) cairo-hint) -.hin) ?=(%| +<.hin))
    ::     (frond %broke (num p.hin))
    ::  for some reason previous conditional doesn't assert %& case here
    ::  it should mint-vain, but doesn't
    ?+  hin  !!
        [%0 * *]
      %-  pairs
      :~  jmp-dest+(num 0)
          pred+(en-pred pred.hin)
          path+(en-path path.hin)
      ==
        [%1 *]  (pairs jmp-dest+(num 1) pred+(en-pred +.hin) ~)
    ::
        [%2 * * * *]
      %-  pairs
      :~  jmp-dest+(num 2)
          pred+(en-pred pred.hin)
          sf1+(hints sf1.hin)
          sf2+(hints sf2.hin)
          sf3+(hints sf3.hin)
      ==
    ::     
        [?(%3 %4) * *] :: TODO handle a failed sf?
      %-  pairs
      :~  jmp-dest+(num -.hin)
          pred+(en-pred pred.hin)
          sf+(hints sf.hin)
      ==
    ::
        [?(%5 %6 %7 %8) * * *]
      %-  pairs
      :~  jmp-dest+(num -.hin)
          pred+(en-pred pred.hin)
          sf1+(hints sf1.hin)
          sf2+(hints sf2.hin)
      ==
    ::
        [%9 * * * *]
      %-  pairs
      :~  jmp-dest+(num 9)
          pred+(en-pred pred.hin)
          sf+(hints sf.hin)
          leaf+(num leaf.hin)
          path+(en-path path.hin)
      ==
    ::
        [%10 * * * * *]
      %-  pairs
      :~  jmp-dest+(num 10)
          pred+(en-pred pred.hin)
          sf1+(hints sf1.hin)
          sf2+(hints sf2.hin)
          old-leaf+(num old-leaf.hin)
          path+(en-path path.hin)
      ==
    ::
        [%11 * * %| @]
      %-  pairs
      :~  pred+(en-pred pred.hin)
          sf+(hints sf.hin)
          tag+(num +>+>:hin)
      ==
    ::
        [%cons *]
      %-  pairs
      :~  jmp-dest+(tape "cons")
          pred+(en-pred pred.hin)
          sf1+(hints sf1.hin)
          sf2+(hints sf2.hin)
      ==   
    ::     [%invalid *]  (en-invalid +.hin)
    ==
  ::
  ++  en-path
    |=  path=(list (pair @ud index))
    a+(turn path en-path-elem)
  ::
  ++  en-path-elem
    |=  a=(pair @ud index)
    ^-  json
    =,  enjs:format
    a+[(num p.a) (num q.a) ~]
  ::
  ++  en-pred
    |=  =pred
    ^-  json
    a+[(num s.pred) (num f.pred) (num p.pred) ~]
  ::
  ++  num
    |=  n=@ud
    ^-  json
    [%s `cord`(rsh [3 2] (scot %ui n))]
  --
--
