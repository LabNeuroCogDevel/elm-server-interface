module Utils.Date exposing (..)

import ISO8601 exposing (Time)

import String exposing (join,length)

atLeastTwoInLength : String -> String
atLeastTwoInLength str =
  let
    l = length str
  in
    if l == 0
    then
      "00"
    else 
      if l == 1
      then
        "0"++str
      else
        str

dateToString : Time -> String
dateToString date =
  join "-" <| List.map (atLeastTwoInLength << toString << ((|>) date)) [.year,.month,.day]
