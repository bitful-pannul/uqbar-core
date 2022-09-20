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
  :: ++  hints
  ::   |=  h=^hints
  ::   ^-  json
  ::   a+(turn h en-hint)
  :: ::
  :: ++  en-hint
  ::   |=  hin=cairo-hint
  ::   ^-  json
  ::   =,  enjs:format
  ::   :: ?:  &(?=(_-:*$<(?(%1 %cons %invalid %jet) cairo-hint) -.hin) ?=(%| +<.hin))
  ::   ::     (frond %broke (num p.hin))
  ::   ::  for some reason previous conditional doesn't assert %& case here
  ::   ::  it should mint-vain, but doesn't
  ::   ?+  hin  !!
  ::       [%1 *]  (pairs jmp-dest+(num 1) pred+(en-pred +.hin) ~)
  ::       :: [?(%2 %5 %12) %& *]   (pairs sf1+(en-subf sf1.p.hin) sf2+(en-subf sf2.p.hin) ~)
  ::     ::   [?(%3 %4) %& *]
  ::     :: ?~  sf-res.p.hin  (frond %sf (en-subf sf.p.hin))
  ::     :: %-  pairs
  ::     :: :~  sf+(en-subf sf.p.hin)
  ::     ::     sf-res+(en-hash-req u.sf-res.p.hin)
  ::     :: ==
  ::   :: ::
  ::   ::     [%6 %& *]  (pairs sf1+(en-subf sf1.p.hin) sf2+(num sf2.p.hin) sf3+(num sf3.p.hin) ~)
  ::   ::     [?(%7 %8) %& *]  (pairs sf1+(en-subf sf1.p.hin) sf2+(num sf2.p.hin) ~)
  ::   :: ::
  ::   ::     [%invalid *]  (en-invalid +.hin)
  ::   ==
  ::   ::


  :: ++  en-invalid
  ::   |=  hin=(each @ [@ phash])
  ::   ^-  json
  ::   =,  enjs:format
  ::   ?-  -.hin
  ::     %&  (pairs is-atom+b+%& head+(num p.hin) ~)
  ::     %|  (pairs is-atom+b+%| head+(num -.p.hin) tail+(num +.p.hin) ~)
  ::   ==
  :: ::
  :: ++  en-subf
  ::   |=  subf
  ::   ^-  json
  ::   =,  enjs:format
  ::   (pairs hash+(num h) hints+(hints hit) ~)
  :: ::
  :: ++  en-hash-req
  ::   |=  hash-req
  ::   ^-  json
  ::   =,  enjs:format
  ::   ?-  +<-
  ::     %cell  (pairs head+(num head) tail+(num tail) ~)
  ::     %atom  (frond %atom (num val))
  ::   ==
  ::
  :: ++  en-pred
  ::   |=  =pred
  ::   ^-  json
  ::   a+(turn [1 2 3 ~] num)
    :: a+[(num s.pred) (num f.pred) (num p.pred)]
  ::
  ++  num
    |=  n=@ud
    ^-  json
    [%s `cord`(rsh [3 2] (scot %ui n))]
  --
--