module Utils exposing (..)


import String
import Result


{-- toInt produces a Result, ignore the error message to get a maybe value.
 -- arguably not great to use too much.
 --}
maybeReadInt : String -> Maybe Int
maybeReadInt = Result.toMaybe << String.toInt


{-- Higher order function, takes an updater, new value
 -- function, the object to update for the duration of the function,
 -- and produces a result.
 -- 
 -- Basically used for temporary changes to an object, and
 -- default args.
 -- 
 -- For example, consider the Utils.Http function 
 --   create : CrudInfo a -> a -> Cmd (Result Error (HttpResult a))
 -- the default headers to use for the crud operation are contained
 -- in the CrudInfo object. In order to specify the headers we can use
 --   with headers (newHeaders) create (info) (obj)
 -- instead of
 --   create (headers (newHeaders) (info)) (obj)
 --
 -- The extra parens everywhere are unnecessary, just used to denote what
 -- an imperative design would consider the args of the function.
 --}
with : (b -> a -> a) -> b -> (a -> c) -> a -> c
with updater newVal f obj =
  f (updater newVal obj)

-- Reader monad, a computation that produces an a by reading a c
type alias Reader c a = c -> a

returnF : a -> c -> a
returnF a _ = a

bindF : (c -> a) -> (a -> c -> b) -> (c -> b)
bindF m f c = f (m c) c



{--
ap : (a -> m a) -> (m a -> (a -> m b) -> m b) -> m (a -> b) -> m a -> m b
ap return bind mf ma = 
  bind mf \f ->
  bind ma (return << f)
--}

mapF : (a -> b) -> (c -> a) -> (c -> b)
mapF = (<<)

apF : (c -> a -> b) -> (c -> a) -> (c -> b)
apF mf ma =
  bindF mf <| \f ->
  mapF f ma

-- lifts a 2 argument function
-- The same as 
-- returnF f `ap` ma `ap` mb
lift2F : (a -> b -> d) -> (c -> a) -> (c -> b) -> (c -> d)
lift2F f ma mb c = f (ma c) (mb c)

{--
lift1 : (a -> m a) -> (m a -> (a -> m b) -> m b) -> (a -> b) -> m a -> m b
lift1 return bind f ma = bind ma <| return << f

map = lift1

lift2 : (a -> m a) -> (m a -> (a -> m b) -> m b) -> (a -> b -> c) -> m a -> m b -> m c
lift2 return bind f ma mb = (return f) `ap return bind` ma `ap return bind` mb
--}
