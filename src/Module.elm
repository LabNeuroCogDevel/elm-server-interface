module Module exposing (..)

import Platform.Cmd exposing (..)

import Maybe as M

type alias Module indat msg model out res =
  { init : indat -> (model, Cmd msg) -- initialize the model
  , update : msg -> model -> (model, Cmd msg) -- update the model on receipt of message
  , view : model -> out msg -- view the model in intermediate format out
  , model : Maybe model -- model, if initialized
  , result : model -> Maybe res -- computes final result
  }

type alias Const x a = x

type alias Id x = x

type alias Flip f x a = f a x

view : Module indat msg model out res -> Maybe (out msg)
view mod = M.map mod.view mod.model

update : mg -> Module i mg m o r -> (Module i mg m o r, Cmd mg)
update msg mod = 
  withDefault
    (mod,Cmd.none)
    ( (M.map (mod.update msg) mod.model)
      `M.andThen`
      (\(modl, cmd) ->
        Just
          ( { mod
            | model = Just modl
            }
          , cmd
          )
      )
    )

init : i -> Module i mg m o r -> (Module i mg m o r, Cmd mg)
init x mod = 
  let
    (modl,cmd) = mod.init x
  in
    ( { mod
      | model = Just modl
      }
    , cmd
    )

result : Module i mg m o r -> Maybe r
result mod =
  mod.model
  `M.andThen`
  mod.result


