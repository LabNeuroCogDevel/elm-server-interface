module Types.Study.JsonEncoders exposing (..)

import Types.Study exposing (..)
import Utils.JsonEncoders exposing (..)
import Json.Encode exposing (..)

import List as L

studyEncoder : Encoder Study
studyEncoder st =
  object
    [ ("study", string st.name)
    , ("grantname", string st.grantName)
    , ("cohorts", list <| L.map string st.cohorts)
    , ("visit_types", list <| L.map string st.visitTypes)
    ]

