module Utils exposing (..)


import String
import Result


maybeReadInt : String -> Maybe Int
maybeReadInt = Result.toMaybe << String.toInt

with : (b -> a -> a) -> b -> (a -> c) -> a -> c
with updater newVal f obj =
  f (updater newVal obj)


