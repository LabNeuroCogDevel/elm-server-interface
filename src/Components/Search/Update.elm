module Components.Search.Update exposing (..)

import Components.Search.Model exposing (..)

import Types.Either exposing (..)

import List as L
import Dict as D
import Utils.List as UL


init : KeyInfo k -> SearchModel k
init info =
  { keyInfo = info
  , search = ""
  , order = Right []
  , modifier = defaultModifier
  , additionalSearches = D.empty
  }


update : SearchModel k -> SearchMsg k -> SearchModel k
update model msg = 
  case msg of
    ChangeSorting key ->
      let 
        ord = order model
        ss = getSortStatus key ord
        newSS = model.modifier ss
      in 
        { model 
        | order =
            Right
              <| (UL.uniqBy <| getKeyNameOP model.keyInfo)
              <| case newSS of
                Ascending ->
                  (Asc key) :: ord

                Descending ->
                  (Desc key) :: ord

                Unsorted ->
                  L.filter (((/=) key) << getKey) ord

        }
      
    ModifySearch searchName search ->
      modifySearch searchName search model
      
    SearchStringChanged searchStr ->
      { model
      | search = searchStr
      }
      
    SearchEnter ->
      model
      
    OrdStringChanged ordStr ->
      { model
      | order = Left ordStr
      }
      
    OrdEnter ->
      { model
      | order = Right <| order model
      }
      



