module Utils.Http.Handlers exposing (..)

import Utils.Http exposing (..)

import Http exposing (Error)
import Utils.Http.Tag exposing (Tag)
import Dict exposing (Dict)

import Utils.Http.Tag as Tag

import Utils.Http as U

type alias CrudHandlers a r =
  { info : CrudInfo a
  , onError : Tag -> Error -> r
  , onCreate : HttpResult a -> r
  , onRead : HttpResult (List a) -> r
  , onUpdate : HttpResult a -> r
  , onDelete : HttpResult () -> r
  }

type CrudResult a
  = Error Tag Error
  | Create (HttpResult a)
  | Read (HttpResult (List a))
  | Update (HttpResult a)
  | Delete (HttpResult ())

defaultHandlers : CrudInfo a -> CrudHandlers a (CrudResult a)
defaultHandlers info =
  { info = info
  , onError = Error
  , onCreate = Create
  , onRead = Read
  , onUpdate = Update
  , onDelete = Delete
  }

makeHandlers : CrudInfo a -> (CrudResult a -> r) -> CrudHandlers a r
makeHandlers info func = 
  { info = info
  , onError = \tag -> func << Error tag
  , onCreate = func << Create
  , onRead = func << Read
  , onUpdate = func << Update
  , onDelete = func << Delete
  }

createHandler : CrudHandlers a r -> Handler a r
createHandler handlers = (handlers.onCreate, handlers.onError Tag.Create)

readHandler : CrudHandlers a r -> Handler (List a) r
readHandler handlers = (handlers.onRead, handlers.onError Tag.Read)

updateHandler : CrudHandlers a r -> Handler a r
updateHandler handlers = (handlers.onUpdate, handlers.onError Tag.Update)

deleteHandler : CrudHandlers a r -> Handler () r
deleteHandler handlers = (handlers.onDelete, handlers.onError Tag.Delete)


create : CrudHandlers a r -> a -> Cmd r
create handlers = 
  U.handle (createHandler handlers) << U.create (handlers.info)

read : CrudHandlers a r -> Dict String String -> Cmd r
read handlers = 
  U.handle (readHandler handlers) << U.read (handlers.info)

update : CrudHandlers a r -> a -> Cmd r
update handlers = 
  U.handle (updateHandler handlers) << U.update (handlers.info)

delete : CrudHandlers a r -> a -> Cmd r
delete handlers = 
  U.handle (deleteHandler handlers) << U.delete (handlers.info)

deleteById : CrudHandlers a r -> Int -> Cmd r
deleteById handlers = 
  U.handle (deleteHandler handlers) << U.deleteById (handlers.info)

