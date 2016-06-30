module Utils.Maybe exposing (..)


-- if val fails the test, return Nothing
test : (a -> Bool) -> a -> Maybe a
test t val = if t val then Just val else Nothing

