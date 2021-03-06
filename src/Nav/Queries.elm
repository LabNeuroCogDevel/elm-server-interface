module Nav.Queries exposing (..)

{- Queries need to be preserved when changing route,
 - but staying on the same page.
 - 
 - Ideally this module will accomplish this.
 - 
 -}

import Dict exposing (Dict)

import Result
import String

import Maybe as M
import Dict as D

type alias Query = Dict String String 

type alias PeopleQuery = { page : Int, search : String, order : String }

defaultPeopleQuery : PeopleQuery
defaultPeopleQuery =
  { page = 1
  , search = ""
  , order = ""
  }

type alias Paged x = 
  { x
  | page : Int
  }

type alias Searchable x =
  { x
  | search : String
  }

type alias Orderable x =
  { x
  | order : String
  }


getPageFromQuery : Dict String String -> Paged x -> Paged x
getPageFromQuery queries struct = 
  let
    n = M.withDefault struct.page
          <| (D.get "page" queries) `M.andThen`
              (Result.toMaybe << String.toInt)

  in
    { struct | page = n }
  
getSearchFromQuery : Dict String String -> Searchable x -> Searchable x
getSearchFromQuery queries struct =
  let
    str
      = M.withDefault struct.search
          <| D.get "search" queries
  in
    { struct | search = str }

getOrderFromQuery : Dict String String -> Orderable x -> Orderable x
getOrderFromQuery queries struct =
  let
    str
      = M.withDefault struct.order
          <| D.get "order" queries
  in
    { struct | order = str }
