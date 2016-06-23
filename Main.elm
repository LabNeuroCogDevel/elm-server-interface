module Main exposing (..)

import Html.App as Html
import Core.View exposing (view)
import Core.Update exposing (init, update, urlUpdate)
import Nav.RQ exposing (urlParser)
import Navigation

      
app = Navigation.program urlParser
  { init = init
  , update = update
  , view = view
  , urlUpdate = urlUpdate
  , subscriptions = \_ -> Sub.none
  }

main = app


  

