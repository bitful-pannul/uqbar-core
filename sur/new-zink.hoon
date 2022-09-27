|%
+$  granary-scry
  $-  ^
  (unit (unit *))
::
+$  index  @ud
+$  tnoun  $%([%atom @] [%cell index index])
+$  cache  (map * (pair tnoun index))  :: noun to [[ihead itail] inoun]

+$  phash  @                     ::  Pedersen hash
:: +$  hash-req
::   $%  [%cell head=phash tail=phash]
::       [%atom val=@]
::   ==
::
+$  pred  [s=index f=index p=index]
+$  cairo-hint
  $%
      [%0 =pred path=(list (pair ?(%2 %3) index))]
      [%1 =pred]
      [%2 =pred sf1=hints sf2=hints sf3=hints]
      [%3 =pred sf=hints]
      [%4 =pred sf=hints]
      [%5 =pred sf1=hints sf2=hints]
      [%6 =pred sf1=hints sf2=hints] :: got rid of the subf that doesn't get run...should be fine?
      [%7 =pred sf1=hints sf2=hints]
      [%8 =pred sf1=hints sf2=hints]
      [%9 =pred sf=hints leaf=index path=(list (pair ?(%2 %3) index))]
      [%10 =pred sf1=hints sf2=hints old-leaf=index path=(list (pair ?(%2 %3) index))]
      [%11 =pred sf=hints (each [tag=@ clue=hints] @)]
      :: [%12 (each [sf1=subf sf2=subf] phash)]
      :: [%jet =jet data=json] :: not every jet will return the whole sample as a noun
      [%cons =pred sf1=hints sf2=hints]
      [%invalid *] :: TODO: didn't want to deal with this [%invalid (each @ [@ phash])]
  ==
+$  hints  $@(~ [i=cairo-hint t=(list cairo-hint)]) :: TODO not sure if this needs to be a list
::  map of a noun's merkle children. root -> [left right]--
