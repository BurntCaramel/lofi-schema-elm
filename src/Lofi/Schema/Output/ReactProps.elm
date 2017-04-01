module Lofi.Schema.Output.ReactProps exposing
  ( createPropTypesCode
  )

{-|

# Functions
@docs createPropTypesCode
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

pairsToJSObject : Bool -> List (String, String) -> String
pairsToJSObject multilined pairs =
  let
    propertyDivider =
      if multilined then ",\n  " else ", "
    
    wrapInBraces =
      if multilined then
        \s -> "{\n  " ++ s ++ "\n}"
      else
        \s -> "{ " ++ s ++ " }"
  in
    pairs
    |> List.map (\(a, b) -> a ++ ": " ++ b)
    |> String.join propertyDivider
    |> wrapInBraces

createPropTypesCodeFold : Item -> List (Int, (String, String)) -> List (Int, (String, String))
createPropTypesCodeFold item list =
  let
    nativeKind =
      case item.kind of
        Text _ ->
          "PropTypes.string"
        Number _ ->
          "PropTypes.number"
        Date _ ->
          "PropTypes.instanceOf(Date)"
    
    propertyName =
      lowerCamelCase item.name

    requiredString =
      if item.optional then
        Nothing
      else
        Just ".isRequired"
    
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

{-| Generates JavaScript code creating a React PropTypes declaration -}
createPropTypesCode : Schema -> String
createPropTypesCode schema =
  let
    fields =
      List.foldr createPropTypesCodeFold [] schema.items
      |> List.sortBy Tuple.first
      |> List.map Tuple.second
      |> pairsToJSObject True
  in
    "propTypes = " ++ fields ++ ";"
