module Nav.RQ exposing (..)

import Nav.Routes exposing (..)
import Nav.Pages exposing (..)
import Hop exposing (..)

import Dict exposing (Dict)

import Navigation
import Regex


-- RQ is short for Route and Query

type alias RQ =
  { route : Route
  --, query : Dict String String
  , page : Page
  }

makeRQ : Route -> Query -> RQ
makeRQ r q =
  { route = r
  --, query = q
  , page = getPage r q 
  }

getQueryRQ : RQ -> Query
getQueryRQ rq = pageToQuery rq.page

getPageRQ : RQ -> Page
getPageRQ rq = rq.page

getRouteRQ : RQ -> Route
getRouteRQ rq = rq.route

updateRoute : RQ -> Route -> RQ
updateRoute rq route = makeRQ route <| getQueryRQ rq

updateQuery : RQ -> String -> String -> RQ
updateQuery rq k v = 
  makeRQ rq.route
    <| Dict.insert k v <| getQueryRQ rq



urlParser : Navigation.Parser RQ
urlParser = Navigation.makeParser <| \loc ->
  let
    (r,hloc) = matchUrl routerConfig loc.href
  in
    makeRQ r hloc.query


