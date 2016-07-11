module Pages.People.HttpCmds exposing (..)

import Core.HttpCmds exposing (..)
import Json.Decode exposing (..)
import Types.Person.JsonDecoders exposing (..)
import Nav.Paging exposing (..)
import Components.Search.Model exposing (..)
import Pages.People.Search exposing (..)

import Regex
import String
import Utils

import List as L
import Dict as D
import Pages.People.Model as P


updateNavFromModel : P.Model -> Cmd P.Msg
updateNavFromModel model
 = Utils.navigateTo
    model.routeQuery
    Nothing
    (Just <| D.fromList [("search",P.searchString model),("page","1"),("order",P.ordString model)])


runSearch : Search PeopleKey -> Ordering PeopleKey -> Cmd P.Msg
runSearch search order = getPeople search order 25 1


-- TODO ERROR HANDLING
-- Errors "silently" fail here
-- not exactly silent since it returns -1 or -2, but still pretty
-- silent
getPeople : Search PeopleKey -> Ordering PeopleKey -> Int -> Int -> Cmd P.Msg
getPeople srch ord itemsPerPage page = 
  let
    fIndex = (page - 1) * itemsPerPage
    lIndex = fIndex + itemsPerPage - 1 
  in
    Cmd.map (\result -> case result of 
      Err _ ->
        P.NoOp
      Ok (people,headers) ->
        let 
          contentRange = D.get "Content-Range" headers
        in P.ChangePeopleList people <|
          case contentRange of 
            Nothing -> makePagingInfo 1 -2 -2 -2 -- "No content-range" --(2,1)
            Just "*/0" -> makePagingInfo itemsPerPage 0 0 0
            Just range -> let
                            ints = L.map String.toInt <|
                              Regex.split Regex.All
                                          (Regex.regex "[-/]")
                                          range
                          in case ints of
                            [ Ok l, Ok u, Ok m ] ->
                              makePagingInfo itemsPerPage l u m
                            
                            _ ->
                              makePagingInfo 1 -1 -1 -1)

    <| getWithHeaders (list memberDecoderLarge) (makePersonUrl <| (orderToQuery peopleKeyInfo ord)::(searchToQuery peopleKeyInfo srch))
         [ ("Range-Unit", "items")
         , ("Range", toString fIndex ++ "-" ++ toString lIndex)
         ]
