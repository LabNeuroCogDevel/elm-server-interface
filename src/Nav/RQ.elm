module Nav.RQ exposing (..)

import Nav.Routes exposing (..)
import Nav.Queries exposing (..)
--import Nav.Pages exposing (..)
import Hop exposing (..)

import Dict exposing (Dict)

import Navigation
import Regex
import Dict

import Nav.Routes as Rs

-- RQ is short for Route and Query

type alias RQ = Route
{--
  { route : Route
  --, query : Dict String String
  --, page : Page
  }
--}


makeRQ : Route -> Query -> RQ
makeRQ r q = updateQueryRoute r q
{--
  { route = updateQueryRoute r q
  --, query = q
  --, page = getPage r q 
  }
--}


getQueryRQ : RQ -> Query
getQueryRQ rq = routeToQuery <| getRouteRQ rq


getQueryParam : String -> RQ -> Maybe String
getQueryParam str rq = Dict.get str (getQueryRQ rq)

{--
getPageRQ : RQ -> Page
getPageRQ rq = rq.page
--}


getRouteRQ : RQ -> Route
getRouteRQ rq = rq


updateRoute : RQ -> Route -> RQ
updateRoute rq route = makeRQ route <| getQueryRQ rq


updateQuery : RQ -> String -> String -> RQ
updateQuery rq k v = 
  makeRQ (getRouteRQ rq)
    <| Dict.insert k v <| getQueryRQ rq



urlParser : Navigation.Parser RQ
urlParser = Navigation.makeParser <| \loc ->
  let
    (r,hloc) = matchUrl routerConfig loc.href
  in
    makeRQ r hloc.query
