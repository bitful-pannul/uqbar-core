|%
+$  granary-scry
  $-  ^
  (unit (unit *))
::
+$  cache  (map * (pair phash @ud))
+$  child  *
+$  parent  *
+$  phash  @                     ::  Pedersen hash
+$  hash-req
  $%  [%cell head=phash tail=phash]
      [%atom val=@]
  ==
::
+$  subf  [h=phash hit=hints]
+$  cairo-hint
  $%  
      [%0 (each [axis=@ leaf-or-atom=(each phash [=atom crash-axis=@]) path=(list phash)] phash)]
      [%1 phash]
      [%2 (each [sf1=subf sf2=subf] phash)]
      [%3 (each [sf=subf sf-res=(unit hash-req)] phash)]
      [%4 (each [sf=subf sf-res=(unit hash-req)] phash)]
      [%5 (each [sf1=subf sf2=subf] phash)]
      [%6 (each [sf1=subf sf2=phash sf3=phash] phash)]
      [%7 (each [sf1=subf sf2=phash] phash)]
      [%8 (each [sf1=subf sf2=phash] phash)]
      [%9 (each [axis=@ sf=subf leaf-or-atom=(each phash [=atom crash-axis=@]) path=(list phash)] phash)]
      $:  %10
          %+  each
            $:  axis=@
                sf1=subf
                sf2=subf
                old-leaf-or-atom=(each phash [=atom crash-axis=@])
                path=(list phash)
            ==
          phash
      ==
      [%11 (each [(each [tag=@ clue=subf] @) sf=phash] phash)]
      [%12 (each [sf1=subf sf2=subf] phash)]
      [%jet jet=@tas data=*] :: not every jet will return the whole sample as a noun
      [%cons sf1=subf sf2=subf]
      [%invalid (each @ [@ phash])]
  ==
:: subject -> formula -> hint
::+$  hints  (mip phash phash cairo-hint)
+$  hints  $@(~ [i=cairo-hint t=(list cairo-hint)])
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
      [%zock 1]
    ==
--
