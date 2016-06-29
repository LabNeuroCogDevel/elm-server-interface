module Utils exposing (..)

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

singleton : a -> List a 
singleton x = [x]

transpose : a -> (List (List a)) -> (List (List a))
transpose def cols =
  if L.all L.isEmpty cols
  then
    []
  else
    (L.map (withDefault def << L.head) cols) :: (transpose def <| L.map (withDefault [] << L.tail) cols)

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


maybeReadInt : String -> Maybe Int
maybeReadInt = Result.toMaybe << String.toInt


makeUrlR : Route -> String
makeUrlR r = makeUrlRQ <| makeRQ r Dict.empty


makeUrlRQ : RQ -> String
makeUrlRQ rq = 
  makeUrlFromLocation routerConfig
    { path = Regex.split Regex.All (Regex.regex "/") <|
               routeToPath <| getRouteRQ rq
    , query = getQueryRQ rq
    }


