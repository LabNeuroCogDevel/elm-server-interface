module Types.Person.Hand exposing (..)

import Json.Encode as Enc
import Utils.JsonEncoders exposing (..)

import Json.Decode exposing (..)
import Utils.JsonDecoders exposing (..)

import Maybe exposing (Maybe, withDefault)
import String exposing (join,concat,toLower)

type Hand 
  = Left
  | Right
  | Ambi
  | Unknown


handEncoder : Encoder Hand
handEncoder = Enc.string << handToString

handToString : Hand -> String
handToString hnd = case hnd of
  Left ->
    "L"
  Right ->
    "R"
  Unknown ->
    "U"
  Ambi -> 
    "A"


prettyHandToString : Hand -> String
prettyHandToString hnd = case hnd of
  Left ->
    "Left"
  Right ->
    "Right"
  Unknown ->
    "Unknown"
  Ambi -> 
    "Ambidextrous"

hand : Decoder Hand
hand =
  customDecoder string
    (\str ->
      let
        s = toLower str
      in
        case s of
          "l" ->
            Ok Left
          "r" ->
            Ok Right
          "u" ->
            Ok Unknown
          "a" ->
            Ok Ambi
          _ ->
            Err <| concat ["Invalid hand: \"", str, "\"."]
      )

