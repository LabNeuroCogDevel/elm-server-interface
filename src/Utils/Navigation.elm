module Utils.Navigation exposing (..)


import Nav.Routes exposing (..)
import Nav.Queries exposing (..)
import Nav.RQ exposing (..)
import Dict exposing (..)
import Maybe exposing (..)

import Hop exposing (makeUrlFromLocation)
import Platform.Cmd exposing (Cmd)
import Types.Either exposing (Either (..))

import List as L

import Navigation
import String
import Result
import Regex

-- Takes the current route and maybe a new route and maybe a new
-- query
-- 
-- 1. If there is no new route or query, we do nothing
-- 2. If there is a new query, but no new route, we update the query
-- 3. If there is a new route, we update with new route and whatever
--    query was given.
navigateTo : RQ -> Maybe Route -> Maybe Query -> Cmd msg
navigateTo rq route query =
  case route of 
    Nothing ->
      case query of
        Nothing ->
          Cmd.none
        Just q ->
          let
            q' = union q <| getQueryRQ rq
          in
            Navigation.modifyUrl
              <| makeUrlRQ
              <| makeRQ (getRouteRQ rq) q'
    Just r ->
      let 
        q1 = 
          case query of
            Nothing -> 
              empty
            Just q ->
              q
        q2 =
          if samePage (getRouteRQ rq) r
          then
            getQueryRQ rq
          else
            empty
        q' = union q1 q2
      in
        Navigation.modifyUrl
          <| makeUrlRQ
          <| makeRQ r q'

makeUrlR : Route -> String
makeUrlR r = makeUrlRQ <| makeRQ r Dict.empty


makeUrlRQ : RQ -> String
makeUrlRQ rq = 
  makeUrlFromLocation routerConfig
    { path = Regex.split Regex.All (Regex.regex "/") <|
               routeToPath <| getRouteRQ rq
    , query = getQueryRQ rq
    }
