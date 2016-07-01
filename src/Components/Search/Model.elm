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

type Key
  = Name
  | Age
  | Sex
  | Hand
  | Id
  | Ids
  | DOB


allKeys : List Key
allKeys =
  [ Ids, Name, Age, Sex, Hand, Id, DOB ]


keyNames : Key -> (String, List String)
keyNames key = case key of
  Name ->
    ("fullname", ["name"])

  Age ->
    ("curage", ["age"])

  Sex ->
    ("sex", [])

  Hand ->
    ("hand", [])

  Id ->
    ("pid", ["id"])

  Ids ->
    ("ids", [])

  DOB ->
    ("dob", ["born","bdate","birthdate"])

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

keyDefault : Key -> OpTag
keyDefault key = case key of
  Name -> ILikeT
  Sex -> ILikeT
  Hand -> ILikeT
  Id -> RangeT
  Ids -> ContainsT
  Age -> RangeT
  DOB -> RangeT


operatorNames : Key -> OpTag -> List String
operatorNames key operator = 
  let
    defs = basicOperatorNames operator
    b=":"::defs
  in case (key,operator) of
    (Name, ILikeT) ->
      b

    (Sex, ILikeT) ->
      b

    (Hand, ILikeT) ->
      b

    (Id, RangeT) ->
      b

    (Ids, ContainsT) ->
      b

    (Age, RangeT) ->
      b

    (key,NotT op) ->
      notNames `UL.andThen` \notname ->
      operatorNames key op `UL.andThen` \opname ->
      [ notname ++ opname ]
      
    _ ->
      defs


--opSearchName : OpTag -> String
--opSearchName = fst << basicOperatorNames

type alias SearchParam =
  { key : Key
  , operator : Operator
  }


type alias Search = List SearchParam

type OrderParam
  = Asc Key
  | Desc Key

type alias Ordering = List OrderParam


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



operators : Key -> Dict String OpTag
operators key = D.fromList
  <| getList (operatorNames key) (flip (,)) allOperators


operatorList : Key -> List String
operatorList key =
  getList (operatorNames key) (\k str -> str) allOperators


parseSearch : String -> Search
parseSearch = (<<) (filterMap <| parseParam << trim) <| R.split All <| regex "\\s*;\\s*"


parseOrder : String -> Ordering
parseOrder = (<<) (filterMap <| parseOrderParam << trim) <| R.split All <| regex "\\s*[;,]\\s*"


parseKey : String -> Maybe (Key, String)
parseKey str =
  (head <| filter (flip startsWith (S.toLower str)) keyList) -- find the string it starts with
  `andThen`
  (\kstr -> -- save as kstr
    (D.get kstr keys) -- get the key
    `andThen`
    (\key -> -- save as key
      Just (key, trimLeft <| dropLeft (S.length kstr) str) -- drop matched string and trim whitespace
    )
  )


parseOrderParam : String -> Maybe OrderParam
parseOrderParam str = 
  (parseKey str)
  `andThen`
  (\(key,rest) ->
    if L.any (flip startsWith (S.toLower rest)) ["d","desc","dec","descending","decreasing","v",">"]
    then
      Just <| Desc key
    else
      Just <| Asc key
      {--
      if L.any (flip startsWith (S.toLower rest)) ["a","asc","ascending","i","inc","increasing","^","<"]
      then
        Just <| Asc key
      else
        Nothing
      --}
  )


parseOperator : Key -> String -> Maybe Operator
parseOperator key str =
  withDefault "" (head <| filter (flip startsWith (S.toLower str)) <| operatorList key) -- find the string it starts with
  |>
  (\opstr -> -- save as opstr
    let
      optag = withDefault (keyDefault key) (D.get opstr <| operators key) -- get the operator
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


parseParam : String -> Maybe SearchParam
parseParam str = 
  withDefault (Name,str) (parseKey str)
  |>
  \(key,rest) ->  -- save the key as key
    let
      operator =  -- save the operator and length of matched operator string
        withDefault (Eq rest) -- if no operator specified assume equality
          <| parseOperator key rest
    in -- return results
      Just
        { key = key
        , operator = operator
        }


searchToQuery : Search -> List (String,String)
searchToQuery = L.concatMap paramToQueries


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
  

paramToQueries : SearchParam -> List (String,String)
paramToQueries s =
  L.map ((,) <| fst <| keyNames s.key) 
    <| getDBQueries s.operator

orderToQuery : Ordering -> (String, String)
orderToQuery order = 
  ("order",join "," <| L.map orderParamToString order)

orderParamToString : OrderParam -> String
orderParamToString ordP = case ordP of
  Desc key ->
    S.concat [ fst <| keyNames key, ".", "desc" ]

  Asc key ->
    S.concat [ fst <| keyNames key, ".", "asc" ]

