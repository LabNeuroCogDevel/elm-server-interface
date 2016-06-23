module Nav.Routes exposing (..)

import Hop exposing (makeUrl, makeUrlFromLocation, matchUrl, setQuery)
import Hop.Types exposing (Config, Query, Location, PathMatcher, Router)
import Navigation

import Dict exposing (Dict)
import Dict
import Regex

import Hop.Matchers exposing (..)


type Operation 
  = View Int
  | Edit Int 
  | Delete Int
  | All
  | New
  | Cancel
  --| Create

type alias Query = Dict String String 

opBasePath : Operation -> String
opBasePath x =
  case x of
    (View _) -> "/"
    (Edit _) -> "/edit/"
    (Delete _) -> "/delete/"
    --Create -> "/create"
    All -> "/all"
    New -> "/new"
    Cancel -> "/cancel"


operationToPath : Operation -> String
operationToPath x =
  case x of
    View n ->
      (opBasePath x) ++ (toString n)

    Edit n ->
      (opBasePath x) ++ (toString n)

    Delete n ->
      (opBasePath x) ++ (toString n)

    _ ->
      opBasePath x

operationMatchers : List (PathMatcher Operation)
operationMatchers =
  [ match2 View (opBasePath <| View 0) int
  , match2 Edit (opBasePath <| Edit 0) int
  , match2 Delete (opBasePath <| Delete 0) int
  , match1 All <| opBasePath All
  , match1 All ""
  , match1 New <| opBasePath New
  , match1 Cancel <| opBasePath Cancel
  --, match1 Create <| opBasePath Create
  ]

type Route
  = Root
  | People Operation
  | NotFound
                

routeBasePath : Route -> String
routeBasePath x =
  case x of
    Root -> "/"
    People _ -> "/people"
    NotFound -> "/err404"


routeToPath : Route -> String
routeToPath route = 
  case route of
    People op -> 
      (routeBasePath route) ++ (operationToPath op)

    _ ->
      routeBasePath route


matchers : List (PathMatcher Route)
matchers = 
  [ match1 Root <| routeBasePath Root
  , match1 Root ""
  , nested1 People (routeBasePath <| People All) operationMatchers
  , match1 NotFound <| routeBasePath NotFound
  ]


routerConfig : Config Route
routerConfig =
  { basePath = ""
  , hash = True
  , matchers = matchers
  , notFound = NotFound
  }


  
