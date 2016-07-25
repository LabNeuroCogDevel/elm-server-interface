module Types.Person.Crud exposing (..)

import Utils.Http exposing (..)
import Core.HttpCmds exposing (urlstring)

import Types.Person exposing (..)
import Types.Person.Json exposing (..)

import Dict exposing (..)


personCrudInfo : CrudInfo Person
personCrudInfo = 
  { url = urlstring ++ "person"
  , search =
      Just
        { url = urlstring ++ "person_search_view"
        , decode = decode
        }
  , getId = (.pid)
  , idField = "pid"
  , decode = decode
  , encode = encode
  , headers = always empty
  , noSearch = False
  }


