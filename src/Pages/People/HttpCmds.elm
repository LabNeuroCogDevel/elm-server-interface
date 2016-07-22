module Pages.People.HttpCmds exposing (..)

import Utils.Http.Handlers exposing (..)
import Utils.Http exposing (getWithHeaders, postWithHeaders, patchWithHeaders)
import Utils.Http.Tag exposing (Tag)
import Utils.Navigation exposing (navigateTo)
import Core.HttpCmds exposing (urlstring)
import Json.Decode exposing (..)
import Types.Person exposing (..)
import Types.Person.JsonDecoders exposing (..)
import Types.Person.JsonEncoders exposing (..)
import Nav.Paging exposing (..)
import Components.Search.Model exposing (..)
import Pages.People.Search exposing (..)
import Http exposing (..)

import Regex
import String
import Utils

import List as L
import Dict as D
import String as S
import Pages.People.Model as P


personSearchUrl : String
personSearchUrl = urlstring ++ "person_search_view"

personInsertUrl : String
personInsertUrl = urlstring ++ "person"


makePersonUrl : List (String, String) -> String
makePersonUrl = url personSearchUrl

crudHandler : CrudResult Person -> P.Msg
crudHandler cr = case cr of
  Error tag err ->
    P.NoOp

  Create (person, headers) ->
    P.SubmittedPerson person

  Read (people, headers) ->
    let 
      contentRange = D.get "Content-Range" headers
      itemsPerPage = 25
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
                          makePagingInfo 1 -1 -1 -1

  Update (person, headers) ->
    P.SavedPerson person

  Delete ((), headers) ->
    P.NoOp


-- TODO ERROR HANDLING
insertPerson : Person -> Cmd P.Msg
insertPerson person =
  Cmd.map 
    (\result -> case result of 
      Err _ ->
        P.NoOp

      Ok (person, headers) ->
        P.SubmittedPerson person
    )
    <| 
    postWithHeaders personEncoder memberDecoderLarge personInsertUrl
      [ ("Prefer", "return=representation")
      , ("Content-Type", "application/json")
      ]
      person

updatePerson : Person -> Cmd P.Msg
updatePerson person = 
  Cmd.map 
    (\result -> case result of 
      Err _ ->
        P.NoOp

      Ok (person, headers) ->
        P.SavedPerson person
    )
    <| 
    patchWithHeaders personEncoder memberDecoderLarge (makePersonUpdateUrl person.pid)
      [ ("Prefer", "return=representation")
      , ("Content-Type", "application/json")
      ]
      person


makePersonUpdateUrl : Pid -> String
makePersonUpdateUrl id = url personInsertUrl [("pid",S.concat ["eq.",toString id])]


updateNavFromModel : P.Model -> Cmd P.Msg
updateNavFromModel model
 = navigateTo
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
