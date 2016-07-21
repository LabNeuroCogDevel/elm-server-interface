module Nav.Operations exposing (..)

import Hop.Matchers exposing (..)
import Hop.Types exposing (..)


type Operation 
  = View Int
  | Edit Int 
  | Delete Int
  | All
  | New
  | Cancel
  --| Create



opBasePath : Operation -> String
opBasePath x =
  case x of
    (View _) -> "/"
    (Edit _) -> "/edit/"
    (Delete _) -> "/delete/"
    --Create -> "/create"
    All -> "/all"
    New -> "/new"
    Cancel -> "/cancel"


operationToPath : Operation -> String
operationToPath x =
  case x of
    View n ->
      (opBasePath x) ++ (toString n)

    Edit n ->
      (opBasePath x) ++ (toString n)

    Delete n ->
      (opBasePath x) ++ (toString n)

    _ ->
      opBasePath x

operationMatchers : List (PathMatcher Operation)
operationMatchers =
  [ match2 View (opBasePath <| View 0) int
  , match2 Edit (opBasePath <| Edit 0) int
  , match2 Delete (opBasePath <| Delete 0) int
  , match1 All <| opBasePath All
  , match1 All ""
  , match1 New <| opBasePath New
  , match1 Cancel <| opBasePath Cancel
  --, match1 Create <| opBasePath Create
  ]

