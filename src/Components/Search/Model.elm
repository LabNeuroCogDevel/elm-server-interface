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


type Operator
  = Eq
  | Like
  | ILike
  | Lt
  | Gt
  | Range
  | In
  | Contains
  | ContainedIn
  | Not Operator

simpleOperators : List Operator
simpleOperators = 
  [ ContainedIn, Contains, Eq, ILike, Like, Lt, Gt, In ]

allOperators : List Operator
allOperators = simpleOperators ++ (L.map Not simpleOperators)

notNames : List String
notNames = ["not ","!","/","! ","/ ","not"]

basicOperatorNames : Operator -> (String, List String)
basicOperatorNames operator = case operator of
  Eq ->
    ("eq",["equals", "=", "is"])

  Like ->
    ("like",[])

  Range ->
    ("",[])

  ILike ->
    ("ilike",[])

  Lt ->
    ("lt",["<","less than"])

  Gt ->
    ("gt",[">","greater than"])

  In ->
    ("in",[])

  Contains ->
    ("@>",["contains","contain"])

  ContainedIn ->
    ("<@",["contained in"])

  Not op ->
    ( "not."++(opSearchName op)
    , notNames `UL.andThen` \notname ->
      allBasicOpNames op `UL.andThen` \opname ->
      [ notname ++ opname ]
    )

operatorNames : Key -> Operator -> (String, List String)
operatorNames key operator = 
  let
    (def, defs) = basicOperatorNames operator
    a=(def,defs)
    b=(def,":"::defs)
  in case (key,operator) of
    (Name, ILike) ->
      b

    (Sex, ILike) ->
      b

    (Hand, ILike) ->
      b

    (Id, Eq) ->
      b

    (Ids, Contains) ->
      b

    (key,Not op) ->
      ( def
      , notNames `UL.andThen` \notname ->
        allOpNames key op `UL.andThen` \opname ->
        [ notname ++ opname ]
      )
      
    _ ->
      a


allBasicOpNames : Operator -> List String
allBasicOpNames op = let (b,bs) = basicOperatorNames op in b::bs

allOpNames : Key -> Operator -> List String
allOpNames key op = let (b,bs) = operatorNames key op in b::bs

opSearchName : Operator -> String
opSearchName = fst << basicOperatorNames

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



operators : Key -> Dict String Operator
operators key = D.fromList
  <| getList ((\(pk,ks) -> pk::ks) << operatorNames key) (flip (,)) allOperators


operatorList : Key -> List String
operatorList key =
  getList ((\(pk,ks) -> pk::ks) << operatorNames key) (\k str -> str) allOperators



parseSearch : String -> Search
parseSearch = (<<) (filterMap <| parseParam << trim) <| R.split All <| regex "\\s*;\\s*"

{--
parseParam : String -> Maybe SearchParam
parseParam s = Nothing
--}

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

parseOperator : Key -> String -> Maybe (Operator, String)
parseOperator key str =
  (head <| filter (flip startsWith (S.toLower str)) <| operatorList key) -- find the string it starts with
  `andThen`
  (\opstr -> -- save as opstr
    (D.get opstr <| operators key) -- get the operator
    `andThen`
    (\op -> -- save as op
      Just (op, trimLeft <| dropLeft (S.length opstr) str) -- drop matched string and trim whitespace
    )
  )
{--}
parseParam : String -> Maybe SearchParam
parseParam str = 
  (parseKey str)
  `andThen`
  (\(key,rest) ->  -- save the key as key
    let
      (operator, rest') =  -- save the operator and length of matched operator string
        withDefault (Eq,rest) -- if no operator specified assume equality
          <| parseOperator key rest
    in -- return results
      Just
        { key = key
        , operator = operator
        , args = rest'
        }
  )
--}

searchToQuery : Search -> List (String,String)
searchToQuery = L.map paramToQuery

removeWhitespace : String -> String
removeWhitespace = R.replace R.All (regex "\\s") (\_ -> "")

splitR : String -> String -> List String
splitR regx str = R.split R.All (regex regx) str

transformArg : Operator -> String -> String
transformArg op arg = case op of
  Not op' ->
    transformArg op' arg

  ILike ->
    "*"++arg++"*"

  Like ->
    "*"++arg++"*"

  In ->
    removeWhitespace arg

  Contains ->
    "["++(S.join "," <| L.map (\str -> S.concat ["\"",str,"\""]) <| splitR "\\s,\\s" arg)++"]"

  ContainedIn ->
    "["++(removeWhitespace arg)++"]"

  _ ->
    arg

paramToQuery : SearchParam -> (String,String)
paramToQuery s =
  ( fst <| keyNames s.key
  , (opSearchName s.operator)
    ++
    "."
    ++
    (transformArg s.operator s.args)
  )



