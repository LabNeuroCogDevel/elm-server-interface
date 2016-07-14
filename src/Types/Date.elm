module Types.Date exposing (..)

import String exposing (join)


type alias Date =
  { year : Int
  , month : Int
  , day : Int
  }

dateToString : Date -> String
dateToString date =
  join "-" <| List.map (toString << ((|>) date)) [ .year, .month, .day ]

makeDate : Int -> Int -> Int -> Maybe Date
makeDate y m d = 
  let
    ans = Just { year = y, month = m, day = d }
  in 
    if m > 12 || m < 1
    then
      Nothing
    else
      if d > 31
      then
        Nothing
      else
        if m == 4 || m == 6 || m == 9 || m == 11 || m == 2
        then
          if d == 31
          then
            Nothing
          else
            if m == 2
            then
              if d < 29 
              then
                ans
              else 
                if y % 4 == 0
                then
                  if d == 29
                  then
                    ans
                  else
                    Nothing
                else
                  Nothing
            else
              ans
        else
          ans

