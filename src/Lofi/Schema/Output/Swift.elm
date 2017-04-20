module Lofi.Schema.Output.Swift exposing
  ( createStructCode
  )

{-|

# Functions
@docs createStructCode
-}

import Lofi.Schema exposing (Schema, Item, Kind(..))
import Char as Char
import String.Extra exposing (underscored, camelize, quote)


lowerCamelCase : String -> String
lowerCamelCase input =
  case
    input
    |> camelize
    |> String.uncons
  of
    Just (c, r) ->
      String.cons (Char.toLower c) r
    Nothing ->
      ""

pairsToStruct : String -> List (String, String) -> String
pairsToStruct name pairs =
  let
    propertyDivider =
      "\n  "
    
    wrapInBraces =
      \s -> "{\n  " ++ s ++ "\n}"
  
    innerCode =
      pairs
      |> List.map (\(a, b) -> "var " ++ a ++ ": " ++ b)
      |> String.join propertyDivider
      |> wrapInBraces
  in
    "struct " ++ name ++ " " ++ innerCode

createStructPropertiesCodeFold : Item -> List (Int, (String, String)) -> List (Int, (String, String))
createStructPropertiesCodeFold item list =
  let
    nativeKind =
      case item.kind of
        Text _ ->
          "String"
        Number _ ->
          "Double"
        Date _ ->
          "Date"
    
    propertyName =
      lowerCamelCase item.name

    requiredString =
      if item.optional then
        Nothing
      else
        Just "?"
    
    schemaString =
      [ Just nativeKind
      , requiredString
      ]
      |> List.filterMap identity
      |> String.join ""

    field =
      (propertyName, schemaString)
    
    priority : Int
    priority =
      if item.isPrimaryKey then
        2
      else
        1
  in
    ( priority, field ) :: list

{-| Generates Swift code declaring a struct -}
createStructCode : Schema -> String
createStructCode schema =
  let
    structCode =
      List.foldr createStructPropertiesCodeFold [] schema.items
      |> List.sortBy Tuple.first
      |> List.map Tuple.second
      |> pairsToStruct (camelize schema.individualName)
  in
    structCode
