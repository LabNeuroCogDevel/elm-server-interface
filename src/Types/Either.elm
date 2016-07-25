module Types.Either exposing (..)

import Maybe exposing (..)

type Either a b
  = Left a
  | Right b

fromMaybe : a -> Maybe b -> Either a b
fromMaybe def mayb = case mayb of 
  Nothing ->
    Left def

  Just b ->
    Right b


right : Either a b -> Maybe b
right val = case val of
  Left _ -> 
    Nothing

  Right v ->
    Just v


left : Either a b -> Maybe a
left val = case val of
  Left v ->
    Just v

  Right _ ->
    Nothing


toMaybe : Either a b -> Maybe b
toMaybe = right


