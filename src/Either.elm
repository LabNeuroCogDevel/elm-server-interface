module Either exposing (..)

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




