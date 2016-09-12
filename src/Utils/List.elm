module Utils.List exposing (..)

import List exposing (..)

import Maybe exposing (withDefault)

import Set as S


singleton : a -> List a 
singleton x = [x]


transpose : a -> (List (List a)) -> (List (List a))
transpose def cols =
  if all isEmpty cols
  then
    []
  else
    (map (withDefault def << head) cols) :: (transpose def <| map (withDefault [] << tail) cols)


andThen : List a -> (a -> List b) -> List b
andThen = flip concatMap


bindL : List a -> (a -> List b) -> List b
bindL = andThen


returnL : a -> List a
returnL = singleton



foldlr : (a -> b -> b) -> (a -> b -> b) -> b -> List a -> b
foldlr left right x0 list =
  case list of
    [] ->
      x0

    (x::xs) ->
      right x 
        <| foldlr left right (left x x0) xs
  
foldba : (a -> b -> b) -> (a -> b -> b -> b) -> b -> List a -> b
foldba before after x0 list =
  case list of
    [] ->
      x0

    (x::xs) ->
      after x x0
        <| foldba before after (before x x0) xs

-- due to limitations of dict-type b must be comparable

uniqBy : (a -> comparable) -> List a -> List a
uniqBy f list = fst <|
  foldba
    (\val (ls,set) ->
      (ls,S.insert (f val) set)
    )
    (\val (oldls,oldset) (ls,set) ->
      if (f val) `S.member` oldset
      then
        (ls,set)
      else
        (val::ls,set)
    )
    ([],S.empty)
    list


-- more general uniqBy with equality check

uniqByG : (a -> comparable) -> List a -> List a
uniqByG f list = fst <|
  foldba
    (\val (ls,set) ->
      if (f val) `member` set
      then
        (ls,set)
      else
        (ls,(f val)::set)
    )
    (\val (oldls,oldset) (ls,set) ->
      if (f val) `member` oldset
      then
        (ls,set)
      else
        (val::ls,set)
    )
    ([],[])
    list
