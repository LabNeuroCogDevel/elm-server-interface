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

type alias Paged x = 
  { x
  | page : Int
  }

type alias Searchable x =
  { x
  | search : String
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

