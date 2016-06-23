module Nav.Paging exposing (..)

type alias PagingInfo =
  { firstItem : Int
  , lastItem : Int
  , numItems : Int
  , itemsPerPage : Int
  , curPage : Int
  , totalPages : Int
  }


makePagingInfo : Int -> Int -> Int -> Int -> PagingInfo
makePagingInfo itemsPerPage fIndex lIndex total =
  { firstItem = fIndex + 1
  , lastItem = lIndex + 1
  , numItems = total
  , itemsPerPage = itemsPerPage
  , curPage = fIndex // itemsPerPage + 1
  , totalPages = (total - 1) // itemsPerPage + 1
  }


isLastPage : PagingInfo -> Bool
isLastPage info = info.curPage == info.totalPages


isFirstPage : PagingInfo -> Bool
isFirstPage info = info.curPage == 1


updatePagingInfo : PagingInfo -> Int -> Int -> Int -> PagingInfo
updatePagingInfo = makePagingInfo << (.itemsPerPage)


