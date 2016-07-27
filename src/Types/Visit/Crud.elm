module Types.Visit.Crud exposing (..)

import Utils.Http exposing (CrudInfo)
import Types.Visit exposing (Visit)
import Types.Visit.Json exposing (decode,encode)
import Core.HttpCmds exposing (urlstring)
import Dict exposing (empty)

visitCrudInfo : CrudInfo Person
visitCrudInfo = 
  { url = urlstring ++ "visit"
  , search = Nothing
  , getId = (.vid)
  , idField = "vid"
  , decode = decode
  , encode = encode
  , headers = always empty
  , noSearch = False
  }
