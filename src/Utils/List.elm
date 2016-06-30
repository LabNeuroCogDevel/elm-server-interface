module Utils.List exposing (..)

import List exposing (..)
import Maybe exposing (withDefault)

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

