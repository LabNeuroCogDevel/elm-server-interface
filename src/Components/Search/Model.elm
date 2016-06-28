module Components.Search.Model exposing (..)

import Regex exposing (..)
import Maybe exposing (..)
import String exposing (trim, startsWith, dropLeft, trimLeft)

import Dict exposing (Dict)
import List exposing (map, filterMap, filter, head, concat)

import Dict as D
import Regex as R
import String as S
import List as L

type Key
  = Name
  | Age
  | Sex
  | Hand
  | Id


allKeys : List Key
allKeys =
  [ Name, Age, Sex, Hand, Id ]


keyNames : Key -> (String, List String)
keyNames key = case key of
  Name ->
    ("name", [])

  Age ->
    ("age", [])

  Sex ->
    ("sex", [])

  Hand ->
    ("hand", [])

  Id ->
    ("id", [])


type Operator
  = Eq
  | Like
  | Lt
  | Gt

allOperators : List Operator
allOperators = 
  [ Eq, Like, Lt, Gt ]

operatorNames : Operator -> (String, List String)
operatorNames operator = case operator of
  Eq ->
    ("=",["equals", "eq", "is"])

  Like ->
    (":",["like"])

  Lt ->
    ("<",["lt","less than"])

  Gt ->
    (">",["gt","greater than"])

type alias SearchParam =
  { key : Key
  , operator : Operator
  , args : String
  }


type alias Search = List SearchParam

getList : (c -> List d) -> (c -> d -> a) -> List c -> List a
getList info handle base
  =  concat
  <| L.map
      (\k ->
        L.map (handle k) (info k)
      )
      base

keys : Dict String Key
keys = D.fromList
  <| getList ((\(pk,ks) -> pk::ks) << keyNames) (flip (,)) allKeys


keyList : List String
keyList = 
  getList ((\(pk,ks) -> pk::ks) << keyNames) (\k str -> str) allKeys



operators : Dict String Operator
operators = D.fromList
  <| getList ((\(pk,ks) -> pk::ks) << operatorNames) (flip (,)) allOperators


operatorList : List String
operatorList =
  getList ((\(pk,ks) -> pk::ks) << operatorNames) (\k str -> str) allOperators



parseSearch : String -> Search
parseSearch = (<<) (filterMap <| parseParam << trim) <| R.split All <| regex "\\s*,\\s*"

{--
parseParam : String -> Maybe SearchParam
parseParam s = Nothing
--}

{--}
parseParam : String -> Maybe SearchParam
parseParam str = 
  (head <| filter (flip startsWith (S.toLower str)) keyList) -- find the string it starts with
  `andThen`
  (\kstr -> -- save the start string as kstr
    (D.get kstr keys) -- find the corresponding key
    `andThen`
    (\key ->  -- save the key as key
      let
        rest = trimLeft <| dropLeft (S.length kstr) str -- drop the matched string and trim whitespace
        (operator, dl) =  -- save the operator and length of matched operator string
          withDefault (Eq,0) -- if no operator specified assume equality
            ( (head <| filter (flip startsWith (S.toLower rest)) operatorList) -- find the operator it starts with
              `andThen` 
              (\opstr ->  -- save the operator string as opstr
                (D.get opstr operators) -- look up the operator
                `andThen`
                (\op -> Just (op, S.length opstr)) -- return the operator and length of matched string
              )
            )
        rest' = trimLeft <| dropLeft dl rest -- drop matched string and trim whitespace
      in -- return results
        Just
          { key = key
          , operator = operator
          , args = rest'
          }
    )
  )

--}
