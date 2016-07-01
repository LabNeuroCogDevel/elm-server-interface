module Nav.Routes exposing (..)

import Hop exposing (makeUrl, makeUrlFromLocation, matchUrl, setQuery)
import Hop.Types exposing (Config, Query, Location, PathMatcher, Router)
import Navigation

import Dict exposing (Dict,empty)

import Dict
import Regex

import Hop.Matchers exposing (..)
import Nav.Operations exposing (..)
import Nav.Queries exposing (..)

import Nav.Queries as NQ

-- base Route represents the page. Anything else after that is a subroute
type Route
  = Root
  | People PeopleQuery Operation
  | NotFound


defaultPeople : Operation -> Route
defaultPeople = People defaultPeopleQuery

routeBasePath : Route -> String
routeBasePath x =
  case x of
    Root -> "/"
    People _ _ -> "/people"
    NotFound -> "/err404"


routeToPath : Route -> String
routeToPath route = 
  case route of
    People _ op -> 
      (routeBasePath route) ++ (operationToPath op)

    _ ->
      routeBasePath route


matchers : List (PathMatcher Route)
matchers = 
  [ match1 Root <| routeBasePath Root
  , match1 Root ""
  , nested1 defaultPeople (routeBasePath <| defaultPeople All) operationMatchers
  , match1 NotFound <| routeBasePath NotFound
  ]


routerConfig : Config Route
routerConfig =
  { basePath = ""
  , hash = True
  , matchers = matchers
  , notFound = NotFound
  }



updateQueryRoute : Route -> NQ.Query -> Route
updateQueryRoute route query = case route of
  People oldQuery subroute ->
    flip People subroute
      <| getOrderFromQuery query
      <| getSearchFromQuery query
      <| getPageFromQuery query
      <| oldQuery

  _ ->
    route


samePage : Route -> Route -> Bool
samePage p1 p2 = case (p1,p2) of
  (Root,Root) ->
    True

  (NotFound,NotFound) ->
    True

  (People _ _,People _ _) ->
    True

  _ -> False



routeToQuery : Route -> NQ.Query
routeToQuery page = case page of
  Root ->
    empty

  NotFound ->
    empty

  People x _ ->
    Dict.fromList 
      [("page", toString x.page)
      ,("search", x.search)
      ,("order", x.order)
      ]



  
