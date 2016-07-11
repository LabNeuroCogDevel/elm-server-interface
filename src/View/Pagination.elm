module View.Pagination exposing (..)

import Html.Attributes exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Nav.Paging exposing (..)
import Nav.Routes exposing (..)
import Nav.RQ exposing (..)
import Utils exposing (..)

import ElmEscapeHtml exposing (unescape)


last : List a -> Maybe a
last xs = case xs of 
  (x::xs') ->
    if xs' == []
    then
      Just x
    else
      last xs'

  _ ->
    Nothing


getDisplayList : Int -> Int -> List Int
getDisplayList numPages curPage = case numPages of
  1 ->
    []
  2 ->
    []
  3 ->
    [2]
  4 ->
    [2, 3]
  5 ->
    [2, 3, 4]
  6 ->
    [2, 3, 4, 5]
  7 ->
    [2, 3, 4, 5, 6]
  _ -> 
    if curPage <= 4
      then
        [2, 3, 4, 5]
      else if curPage >= numPages-3
      then
        [numPages-4, numPages - 3, numPages - 2, numPages - 1]
      else
        [ curPage - 1, curPage, curPage + 1 ]
      


paginatorPrevious : RQ -> PagingInfo -> Html msg
paginatorPrevious rq pg = 
  
  li [ classList 
         [("page-item", True)
         ,("disabled", isFirstPage pg)
         ]
     ]
     [ (if isFirstPage pg then span else a)
         [ classList 
             [("page-link",True)
             ,("disabled", isFirstPage pg)
             ]
         , href <| makeUrlRQ
                <| updateQuery rq "page"
                <| toString (pg.curPage - 1)
         , attribute "aria-label" "Previous"
         ]
         [ span [ attribute "aria-hidden" "true" ]
                [ text <| unescape "&laquo;" ]
         , span [ class "sr-only" ]
                [ text "Previous" ]
         ]
     ]

paginatorNumber : RQ -> PagingInfo -> Int -> Html msg
paginatorNumber rq pg n =
  li [ classList
         [("page-item", True)
         ,("active", n == pg.curPage)
         ]
     ]
     [ (if n == pg.curPage then span else a)
         [ class "page-link"
         , href <| makeUrlRQ
                <| updateQuery rq "page"
                <| toString n
         ]
         [ text (toString n) ]
     ]

paginatorNext : RQ -> PagingInfo -> Html msg
paginatorNext rq pg = 
  li [ classList 
         [("page-item", True)
         ,("disabled", isLastPage pg)
         ]
     ]
     [ (if isLastPage pg then span else a)
         [ classList 
             [("page-link",True)
             ,("disabled", isLastPage pg)
             ]
         , href <| makeUrlRQ
                <| updateQuery rq "page"
                <| toString (pg.curPage + 1)
         , attribute "aria-label" "Next"
         ]
         [ span [ attribute "aria-hidden" "true" ]
                [ text <| unescape "&raquo;" ]
         , span [ class "sr-only" ]
                [ text "Next" ]
         ]
     ]

paginatorDots : Html msg
paginatorDots = 
  li [ class "page-item" ]
     [ span
         [ class "page-link"
         ]
         [ text "..." ]
     ]--<| unescape "&ellipsis;" ]

makePaginator : RQ -> PagingInfo -> Html msg
makePaginator rq pg =
  let
    pagesList = getDisplayList pg.totalPages pg.curPage
  in
    nav []
      [ ul [ class "pagination no-select" ]
          <|
            [ paginatorPrevious rq pg
            , paginatorNumber rq pg 1
            ]
            ++
            ( if List.head pagesList == Just 2 || List.isEmpty pagesList
              then
                []
              else 
                [ paginatorDots ]
            )
            ++
            ( List.map (paginatorNumber rq pg) pagesList )
            ++
            ( if last pagesList == Just (pg.totalPages - 1) || List.isEmpty pagesList
              then
                []
              else 
                [ paginatorDots ]
            )
            ++
            ( if pg.totalPages == 1
              then
                []
              else
                [ paginatorNumber rq pg (pg.totalPages)
                ]
            )
            ++
            [ paginatorNext rq pg
            ]
      ]



