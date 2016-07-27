module Types.ContactInfo.Crud exposing (..)

import Types.ContactInfo exposing (ContactInfo)
import Utils.Http exposing (CrudInfo)
import Core.HttpCmds exposing (urlstring)
import Json.Decode exposing (succeed)
import Types.ContactInfo.JsonDecoders exposing (contactInfoDecoder)
import Types.ContactInfo.JsonEncoders exposing (contactInfoEncoder)


contactInfoListCI : CrudInfo ContactInfo
contactInfoListCI = 
  { url = urlstring ++ "contact"
  , search =
      Just
        { url = urlstring ++ "contacts_view"
        , decode = contactInfoDecoder
        }
  , getId = always 0 --(.belongsTo) Can't delete a list of contact info
  , idField = "cid"
  , decode = succeed [] --decode
  , encode = contactInfoEncoder
  , headers = always empty
  , noSearch = False
  }





