module Lofi.Schema.Output.Mongoose exposing
  ( createModelCode
  )

{-|

# Functions
@docs createModelCode
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

createModelCodeFold : Item -> List (Int, (String, String)) -> List (Int, (String, String))
createModelCodeFold item list =
  let
    nativeKind =
      case item.kind of
        Text { maximumLength } ->
          "String"
        Number { real, allowNegative } ->
          "Number"
        Date { time } ->
          "Date"
    
    propertyName =
      underscored item.name

    requiredString =
      if item.optional then
        Nothing
      else
        Just ("required", "true")
    
    defaultString =
      case item.kind of
        Text { default } ->
          Maybe.map (\default -> ("default", (quote default))) default
        Number { default } ->
          Maybe.map (\default -> ("default", (toString default))) default
        Date { time, defaultIsNow } ->
          if time && defaultIsNow then
            Just ("default", "Date.now")
          else
            Nothing
    
    schemaString =
      [ Just ("type", nativeKind)
      , defaultString
      , requiredString
      ]
      |> List.filterMap identity
      |> pairsToJSObject False

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

{-| Generates JavaScript code creating a mongoose.Schema -}
createModelCode : Schema -> String
createModelCode schema =
  let
    constantName =
      schema.collectionName ++ "_schema"
      |> lowerCamelCase

    fields =
      List.foldr createModelCodeFold [] schema.items
      |> List.sortBy Tuple.first
      |> List.map Tuple.second
      |> pairsToJSObject True
    
    schemaOptions =
      pairsToJSObject True
        [ ("collection", schema.collectionName |> lowerCamelCase |> quote )
        ]
  in
    "const " ++ constantName ++ " = " ++
    "new mongoose.Schema(" ++ fields ++ ", " ++ schemaOptions ++");"
