module Pages.People.HttpCmds exposing (..)

import Core.HttpCmds exposing (..)
import Json.Decode exposing (..)
import Types.Person.JsonDecoders exposing (..)
import Nav.Paging exposing (..)
import Components.Search.Model exposing (..)

import Regex
import String

import List as L
import Dict as D
import Pages.People.Model as P

-- TODO ERROR HANDLING
-- Errors "silently" fail here
-- not exactly silent since it returns -1 or -2, but still pretty
-- silent
getPeople : Search -> Int -> Int -> Cmd P.Msg
getPeople srch itemsPerPage page = 
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

    <| getWithHeaders (list memberDecoderLarge) (makePersonUrl <| searchToQuery srch)
         [ ("Range-Unit", "items")
         , ("Range", toString fIndex ++ "-" ++ toString lIndex)
         ]
