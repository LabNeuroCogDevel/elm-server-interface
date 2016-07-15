module Utils.Date exposing (..)

import Date exposing (..)

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

monthNumber : Month -> Int
monthNumber m = case m of
  Jan -> 1
  Feb -> 2
  Mar -> 3
  Apr -> 4
  May -> 5
  Jun -> 6
  Jul -> 7
  Aug -> 8
  Sep -> 9
  Oct -> 10
  Nov -> 11
  Dec -> 12

dateToString : Date -> String
dateToString date =
  join "-" <| List.map (atLeastTwoInLength << toString << ((|>) date)) [year,monthNumber << month,day]

