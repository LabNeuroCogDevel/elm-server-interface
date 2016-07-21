module Utils.Http.Handlers

import Utils.Http exposing (..)

import Utils.Http as U

type alias CRUDHandlers a r =
  { info : CRUDInfo a
  , onError : Tag -> Error -> r
  , onCreate : HttpResult a -> r
  , onRead : HttpResult (List a) -> r
  , onUpdate : HttpResult a -> r
  , onDelete : HttpResult () -> r
  }

createHandler : CrudHandlers a r -> Handler a r
createHandler handlers = (handlers.onCreate, handlers.onError Create)

readHandler : CrudHandlers a r -> Handler (List a) r
readHandlers handlers = (handlers.onRead, handlers.onError Read)

updateHandler : CrudHandlers a r -> Handler a r
updateHandlers handlers = (handlers.onUpdate, handlers.onError Update)

deleteHandler : CrudHandlers a r -> Handler () r
deleteHandler handlers = (handlers.onDelete, handlers.onError Delete)


create : CrudHandlers a r -> a -> Cmd r
create handlers = 
  U.handle (createHandler handlers) << U.create (handlers.info)

read : CrudHandlers a r -> Dict String String -> Cmd r
read handlers = 
  U.handle (createHandler handlers) << U.read (handlers.info)

update : CrudHandlers a r -> a -> Cmd r
update handlers = 
  U.handle (createHandler handlers) << U.update (handlers.info)

delete : CrudHandlers a r -> a -> Cmd r
delete handlers = 
  U.handle (createHandler handlers) << U.delete (handlers.info)

deleteById : CrudHandlers a r -> Int -> Cmd r
deleteById handlers = 
  U.handle (createHandler handlers) << U.deleteById (handlers.info)

