module Types.Person.Sex exposing (..)

import Json.Encode as Enc
import Utils.JsonEncoders exposing (..)

import Json.Decode exposing (..)
import Utils.JsonDecoders exposing (..)

import Maybe exposing (Maybe, withDefault)
import String exposing (join,concat,toLower)


type Sex
  = Male
  | Female
  | Other
  | Unknown

sexEncoder : Encoder Sex
sexEncoder = Enc.string << sexToString


sexToString : Sex -> String
sexToString sx = case sx of
  Male ->
    "M"
  Female ->
    "F"
  Unknown ->
    "U"
  Other ->
    "O"


prettySexToString : Sex -> String
prettySexToString sx = case sx of
  Male ->
    "Male"
  Female ->
    "Female"
  Unknown ->
    "Unknown"
  Other ->
    "Other"


sex : Decoder Sex
sex =
  customDecoder string
    (\str ->
      let
        s = toLower str
      in
        case s of
          "m" ->
            Ok Male
          "f" ->
            Ok Female
          "u" ->
            Ok Unknown
          "o" ->
            Ok Other
          _ ->
            Err <| concat ["Invalid sex: \"", str, "\"."]
      )
