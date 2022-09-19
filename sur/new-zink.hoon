|%
+$  granary-scry
  $-  ^
  (unit (unit *))
::
:: +$  cache  (map * (pair phash @ud))
+$  cache  (map * (pair * *)) :: pair is head and tail

+$  phash  @                     ::  Pedersen hash
:: +$  hash-req
::   $%  [%cell head=phash tail=phash]
::       [%atom val=@]
::   ==
::
:: +$  subf  [h=phash hit=hints]
+$  pred  [s=* f=* p=*]
+$  cairo-hint
  $%
      :: [%0 (each [axis=@ leaf-or-atom=(each phash [=atom crash-axis=@]) path=(list phash)] phash)]
      [%1 =pred]
      :: [%2 (each [sf1=subf sf2=subf] phash)]
      [%3 =pred sf=hints]
      [%4 =pred sf=hints]
      :: [%5 (each [sf1=subf sf2=subf] phash)]
      :: [%6 (each [sf1=subf sf2=phash sf3=phash] phash)]
      :: [%7 (each [sf1=subf sf2=phash] phash)]
      :: [%8 (each [sf1=subf sf2=phash] phash)]
      :: [%9 (each [axis=@ sf=subf leaf-or-atom=(each phash [=atom crash-axis=@]) path=(list phash)] phash)]
      :: $:  %10
      ::     %+  each
      ::       $:  axis=@
      ::           sf1=subf
      ::           sf2=subf
      ::           old-leaf-or-atom=(each phash [=atom crash-axis=@])
      ::           path=(list phash)
      ::       ==
      ::     phash
      :: ==
      :: [%11 (each [(each [tag=@ clue=subf] @) sf=phash] phash)]
      :: [%12 (each [sf1=subf sf2=subf] phash)]
      :: [%jet =jet data=json] :: not every jet will return the whole sample as a noun
      :: [%cons sf1=subf sf2=subf]
      :: [%invalid (each @ [@ phash])]
      [%invalid *]
  ==
:: subject -> formula -> hint
::+$  hints  (mip phash phash cairo-hint)
+$  hints  $@(~ [i=cairo-hint t=(list cairo-hint)]) :: TODO not sure if this needs to be a list
::  map of a noun's merkle children. root -> [left right]
+$  merk-tree  (map phash [phash phash])
--
