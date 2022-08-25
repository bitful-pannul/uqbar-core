|%
+$  granary-scry
  $-  ^
  (unit [path=(list phash) product=*])
::
+$  cache  (map * (pair phash @ud))
+$  child  *
+$  parent  *
+$  pfhash  @                     ::  Pedersen hash
+$  hash-req
  $%  [%cell head=phash tail=phash]
      [%atom val=@]
  ==
::
+$  phash-tree
  $:  p=phash
      $=  q
      $%  %cache
          [%cell head=phash-tree tail=phash-tree]
          [%atom val=@]
      ==
  ==
+$  subf-tree
  =<  [=hash p=body]
  |%
  ++  body
    $%  [%cache h=phash]
        [%cell head=subf-tree tail=subf-tree]
        [%atom val=@]
    ==
  --
+$  subfh  [op=?(%cons %0 %1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11 %12 [%unknown p=@]) h=phash]
+$  subf  [p=subfh hit=(list cairo-hint)]
::
+$  cairo-hint
  $%  [%0 axis=@ leaf=(each phash atom) path=(list phash)]
      [%1 res=phash]
      [%2 subf1=subf subf2=subf]
      ::  encodes to
      ::   [3 subf-hash atom 0] if atom
      ::   [3 subf-hash 0 cell-hash cell-hash] if cell
      ::
      $:  %3
          =subf
          $=  subf-res
          %-  unit
          $%  [%atom @]
              [%cell head=phash tail=phash]
          ==
      ==
      [%4 subf=subf atom=(each atom (pair phash phash))]
      [%5 subf1=subf subf2=subf]
      [%6 subf1=subf subf2=subfh subf3=subfh]
      [%7 subf1=subf subf2=subfh]
      [%8 subf1=subf subf2=subf]
      [%9 axis=@ subf1=subf leaf=(each phash atom) path=(list phash)]
      [%10 axis=@ subf1=subf subf2=subf oldleaf=(each phash atom) path=(list phash)]
      [%12 grain-id=@ leaf=phash path=(list phash)]  ::  leaf should be hash of grain-id, path is through granary
      [%cons subf1=subf subf2=subf]
      ::[%jet core=phash sample=* jet=@t]
  ==
:: subject -> formula -> hint
::+$  hints  (mip phash phash cairo-hint)
+$  hints  (list cairo-hint)
::  map of a noun's merkle children. root -> [left right]
+$  merk-tree  (map phash [phash phash])
::  map from jet tag to gas cost
+$  jetmap  (map @tas @ud)
::  Axis map of jets in stdlib
++  jets
  ::  TODO: determine *real* costs
  ::  these are totally made up placeholders
  %-  ~(gas by *jetmap)
  :~  ::  math
      [%add 1]  [%dec 1]  [%div 1]
      [%dvr 1]  [%gte 1]  [%gth 1]
      [%lte 1]  [%lth 1]  [%max 1]
      [%min 1]  [%mod 1]  [%mul 1]
      [%sub 1]
      ::  bits
      [%bex 1]  [%can 1]  [%cat 1]
      [%cut 1]  [%end 1]  [%fil 1]
      [%lsh 1]  [%met 1]  [%rap 1]
      [%rep 1]  [%rev 1]  [%rip 1]
      [%rsh 1]  [%run 1]  [%rut 1]
      [%sew 1]  [%swp 1]  [%xeb 1]
      ::  list
      [%turn 5]
      ::  sha
      [%sham 1.000]
      [%shax 1.000]
      [%shay 1.000]
      ::  etc
      [%need 1]
      [%scot 5]
      [%pedersen-hash 10]
    ==
--
