module Components.Search.Model exposing (..)

import Regex exposing (..)
import Maybe exposing (..)
import Utils.Maybe exposing (..)

import String exposing (trim, startsWith, dropLeft, trimLeft, join)
import Dict exposing (Dict)
import List exposing (map, filterMap, filter, head, concat)

import Dict as D
import Regex as R
import Result as Res
import String as S
import List as L
import Maybe as M
import Utils.List as UL

type alias KeyInfo k =
  { allKeys : List k
  , defaultKey : k
  , keyNames : k -> (String,List String)
  , keyDefault : k -> OpTag
  }

getKeyName : KeyInfo k -> k -> String
getKeyName = curry <| fst << (uncurry (.keyNames))

-- pull a tag off the input
-- use tag to determine how to parse args and
-- send off the request

type OpTag
  = EqT
  | LikeT
  | ILikeT
  | LtT
  | LteT
  | GtT
  | GteT
  | RangeT
  | InT
  | ContainsT
  | ContainedInT
  | NotT OpTag

type Operator
  = Eq String
  | Like String
  | ILike String
  | Lt String
  | Lte String
  | Gt String
  | Gte String
  | Range (Maybe String) (Maybe String)
  | In (List String)
  | Contains (List String)
  | ContainedIn (List String)
  | Not Operator

simpleOperators : List OpTag
simpleOperators = 
  [ ContainedInT, ContainsT, LteT, GteT, EqT, ILikeT, LikeT, LtT, GtT, InT, RangeT ]

allOperators : List OpTag
allOperators = simpleOperators ++ (L.map NotT simpleOperators)

notNames : List String
notNames = ["not ","!","/","! ","/ ","not"]

basicOperatorNames : OpTag -> List String
basicOperatorNames operator = case operator of
  EqT ->
    ["eq","equals", "=", "is"]

  LikeT ->
    ["like"]

  RangeT ->
    ["range"]

  ILikeT ->
    ["ilike"]

  LtT ->
    ["lt","<","less than","before"]

  LteT ->
    ["lte","<=","less than or equal to", "at most"]

  GtT ->
    ["gt",">","greater than","after","more than"]

  GteT ->
    ["gte",">=","greater than or equal to", "at least"]

  InT ->
    ["in"]

  ContainsT ->
    ["@>","contains","contain"]

  ContainedInT ->
    ["<@","contained in"]

  NotT op ->
    notNames `UL.andThen` \notname ->
    basicOperatorNames op `UL.andThen` \opname ->
    [ notname ++ opname ]

operatorNames : KeyInfo k -> k -> OpTag -> List String
operatorNames info key operator = 
  let
    defs = basicOperatorNames operator
    b=":"::defs
  in
    case operator of
      NotT op ->
        notNames `UL.andThen` \notname ->
        operatorNames info key op `UL.andThen` \opname ->
        [ notname ++ opname ]

      _ ->
        if operator == (info.keyDefault key)
        then
          b
        else
          defs

--opSearchName : OpTag -> String
--opSearchName = fst << basicOperatorNames

type alias SearchParam k =
  { key : k
  , operator : Operator
  }


type alias Search k = List (SearchParam k)


type SortStatus
  = Unsorted
  | Ascending
  | Descending


type OrderParam k
  = Asc k
  | Desc k


getKey : OrderParam k -> k
getKey op = case op of
  Asc key ->
    key

  Desc key ->
    key

getKeyNameOP : KeyInfo k -> OrderParam k -> String
getKeyNameOP info = getKeyName info << getKey

type alias Ordering k = List (OrderParam k)


getSortStatus : k -> Ordering k -> SortStatus
getSortStatus k o = case head <| filter (((==) k) << getKey) o of
  Nothing ->
    Unsorted

  Just (Asc _) ->
    Ascending

  Just (Desc _) ->
    Descending 


getList : (c -> List d) -> (c -> d -> a) -> List c -> List a
getList info handle base
  =  concat
  <| L.map
      (\k ->
        L.map (handle k) (info k)
      )
      base

keys : KeyInfo k -> Dict String k
keys info = D.fromList
  <| getList ((\(pk,ks) -> pk::ks) << info.keyNames) (flip (,)) info.allKeys


keyList : KeyInfo k -> List String
keyList info = 
  getList ((\(pk,ks) -> pk::ks) << info.keyNames) (\k str -> str) info.allKeys



operators : KeyInfo k -> k -> Dict String OpTag
operators info key = D.fromList
  <| getList (operatorNames info key) (flip (,)) allOperators


operatorList : KeyInfo k -> k -> List String
operatorList info key =
  getList (operatorNames info key) (\k str -> str) allOperators


parseSearch : KeyInfo k -> String -> Search k
parseSearch info = (<<) (filterMap <| parseParam info << trim) <| R.split All <| regex "\\s*;\\s*"


parseOrder : KeyInfo k -> String -> Ordering k
parseOrder info = 
  (<<)
    (UL.uniqBy <| getKeyNameOP info)
    <| (<<)
        (filterMap <| parseOrderParam info << trim)
        <| R.split All <| regex "\\s*[;,]\\s*"


parseKey : KeyInfo k -> String -> Maybe (k, String)
parseKey info str =
  (head <| filter (flip startsWith (S.toLower str)) <| keyList info) -- find the string it starts with
  `andThen`
  (\kstr -> -- save as kstr
    (D.get kstr <| keys info) -- get the key
    `andThen`
    (\key -> -- save as key
      Just (key, trimLeft <| dropLeft (S.length kstr) str) -- drop matched string and trim whitespace
    )
  )


parseOrderParam : KeyInfo k -> String -> Maybe (OrderParam k)
parseOrderParam info str = 
  (parseKey info str)
  `andThen`
  (\(key,rest) ->
    if L.any (flip startsWith (S.toLower rest)) ["d","desc","dec","descending","decreasing","v",">"]
    then
      Just <| Desc key
    else
      Just <| Asc key
  )


parseOperator : KeyInfo k -> k -> String -> Maybe Operator
parseOperator info key str =
  withDefault "" (head <| filter (flip startsWith (S.toLower str)) <| operatorList info key) -- find the string it starts with
  |>
  (\opstr -> -- save as opstr
    let
      optag = withDefault (info.keyDefault key) (D.get opstr <| operators info key) -- get the operator
    in
      opTagParseArgs optag <| trimLeft <| dropLeft (S.length opstr) str -- drop matched string and trim whitespace
  )


opTagParseArgs : OpTag -> String -> Maybe Operator
opTagParseArgs optag = case optag of
  EqT ->
    Just << Eq 

  LikeT ->
    Just << Like 

  ILikeT ->
    Just << ILike

  LtT ->
    Just << Lt

  LteT ->
    Just << Lte

  GtT ->
    Just << Gt

  GteT ->
    Just << Gte

  RangeT ->
    \str -> case S.indices "-" str of
      [] ->
        M.map
          (\f ->
            Range
              (Just <| toString f)
              (Just <| toString <| f+1)
          )
          <| Res.toMaybe
          <| S.toFloat str


      [i] ->
        let
          start = S.left i str
          end = S.dropLeft (i+1) str
        in
          Just
            <| Range
                (test (not << S.isEmpty) start)
                (test (not << S.isEmpty) end)

      _ ->
        Nothing

  InT ->
    Just << In << splitR "\\s,\\s"

  ContainsT ->
    Just << Contains << splitR "\\s,\\s"

  ContainedInT ->
    Just << ContainedIn << splitR "\\s,\\s"

  NotT opt ->
    opTagParseArgs opt


parseParam : KeyInfo k -> String -> Maybe (SearchParam k)
parseParam info str = 
  withDefault (info.defaultKey,str) (parseKey info str)
  |>
  \(key,rest) ->  -- save the key as key
    let
      operator =  -- save the operator and length of matched operator string
        withDefault (Eq rest) -- if no operator specified assume equality
          <| parseOperator info key rest
    in -- return results
      Just
        { key = key
        , operator = operator
        }


searchToQuery : KeyInfo k -> Search k -> List (String,String)
searchToQuery info = L.concatMap (paramToQueries info)


removeWhitespace : String -> String
removeWhitespace = R.replace R.All (regex "\\s") (\_ -> "")


splitR : String -> String -> List String
splitR regx str = R.split R.All (regex regx) str


getDBQueries : Operator -> List String
getDBQueries op = case op of
  Eq str ->
    [ S.concat ["eq",".",str] ]

  Like str ->
    [ S.concat ["like",".","*",str,"*"] ]

  ILike str ->
    [ S.concat ["ilike",".","*",str,"*"] ]

  Lt str ->
    [ S.concat ["lt",".",str] ]

  Lte str ->
    [ S.concat ["lte",".",str] ]

  Gt str ->
    [ S.concat ["gt",".",str] ]

  Gte str ->
    [ S.concat ["gte",".",str] ]

  Range s1 s2 ->
    concat 
      <| filterMap
          (uncurry M.map)
          [ (\str ->
              getDBQueries
                <| Gte str
            , s1
            )
          , (\str ->
              getDBQueries
                <| Lt str
            , s2
            )
          ]

  In strs ->
    [ S.concat <| ["in","."]++strs ]

  Contains strs ->
    [ S.concat <| ["@>","."]++["["]++(L.map (\str -> S.concat ["\"",str,"\""]) strs)++["]"] ]

  ContainedIn strs ->
    [ S.concat <| ["<@","."]++["["]++(L.map (\str -> S.concat ["\"",str,"\""]) strs)++["]"] ]

  Not o ->
    L.map (\str -> S.concat ["not",".",str]) <| getDBQueries o
  

paramToQueries : KeyInfo k -> SearchParam k -> List (String,String)
paramToQueries info s =
  L.map ((,) <| fst <| info.keyNames s.key) 
    <| getDBQueries s.operator

orderToQuery : KeyInfo k -> Ordering k -> (String, String)
orderToQuery info order = 
  ("order",join "," <| L.map (orderParamToString info) order)

orderParamToString : KeyInfo k -> OrderParam k -> String
orderParamToString info ordP = case ordP of
  Desc key ->
    S.concat [ getKeyName info key, ".", "desc" ]

  Asc key ->
    S.concat [ getKeyName info key, ".", "asc" ]

orderingToString : KeyInfo k -> Ordering k -> String
orderingToString info ord =
  S.join ", "
    <| L.map (\ordP -> case ordP of
                Desc key ->
                  S.concat [ getKeyName info key, " ", "desc" ]
                Asc key ->
                  S.concat [ getKeyName info key ]
             )
             ord

  


