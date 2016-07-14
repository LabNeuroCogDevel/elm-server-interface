module Types.Date exposing (..)

import String exposing (concat)


type alias Date =
  { year : Int
  , month : Int
  , day : Int
  }

dateToString : Date -> String
dateToString date =
  concat <| map (toString << ((|>) date)) [ .year, .month, .day ]

