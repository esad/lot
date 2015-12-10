module Helpers.Matrix (insertCol, insertRow, dropCol, dropRow) where

import Array
import List
import Matrix exposing (..)

-- Internal helper which returns a new array with an element inserted at the particular index.
arrayInsertAt : Int -> a -> Array.Array a -> Array.Array a
arrayInsertAt i el arr =
  [ Array.slice 0 i arr
  , Array.repeat 1 el
  , Array.slice i (Array.length arr) arr
  ] |> List.foldr Array.append Array.empty


-- Internal helper which returns a new array without an element at the particular index.
arrayDropAt : Int -> Array.Array a -> Array.Array a
arrayDropAt i arr =
  Array.slice (i+1) (Array.length arr) arr
  |> Array.append (Array.slice 0 i arr)


{-| Insert a new column at the particular index

    insertCol 0 (always 42) (fromList [[0, 1], [2, 3]]) == fromList [[42, 0, 1], [43, 2, 3]]
-}
insertCol : Int -> (Location -> a) -> Matrix a -> Matrix a
insertCol col f m =
  Array.indexedMap (\row mm -> arrayInsertAt col (loc row col |> f) mm) m


{-| Insert a new row at the particular index

    insertRow 0 (always 42) (fromList [[0, 1], [2, 3]]) == fromList [[42, 42], [0, 1], [2, 3]]
-}
insertRow : Int -> (Location -> a) -> Matrix a -> Matrix a
insertRow row f m =
  let
    newRow = Array.initialize (colCount m) (\col -> loc row col |> f)
  in
    arrayInsertAt row newRow m


{-| Drops a column at the particular index

    dropCol 0 (fromList [[0, 1], [2, 3]]) == fromList [[1], [3]]
-}
dropCol : Int -> Matrix a -> Matrix a
dropCol col m =
  Array.map (arrayDropAt col) m


{-| Drops a row at the particular index

    dropRow 0 (fromList [[0, 1], [2, 3]]) == fromList [[2, 3]]
-}
dropRow : Int -> Matrix a -> Matrix a
dropRow row m =
  arrayDropAt row m

