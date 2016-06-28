module Nav.Pages exposing (..)

import Nav.Routes exposing (..)
import Nav.Queries exposing (..)
import Dict exposing (..)

type Page
  = NotFoundPage
  | PeoplePage { page : Int, search : String }
  | RootPage

getPage : Route -> Query -> Page
getPage route query = case route of
  Root ->
    RootPage

  NotFound ->
    NotFoundPage

  People _ ->
    PeoplePage
      <| getSearchFromQuery query
      <| getPageFromQuery query
      <| { page = 1
         , search = ""
         }

samePage : Page -> Page -> Bool
samePage p1 p2 = case (p1,p2) of
  (RootPage,RootPage) ->
    True

  (NotFoundPage,NotFoundPage) ->
    True

  (PeoplePage _,PeoplePage _) ->
    True

  _ -> False

samePageRoute : Page -> Route -> Bool
samePageRoute p = samePage p << flip getPage empty

pageToQuery : Page -> Query
pageToQuery page = case page of
  RootPage ->
    empty

  NotFoundPage ->
    empty

  PeoplePage x ->
    Dict.fromList 
      [("page", toString x.page)
      ,("search", x.search)
      ]



