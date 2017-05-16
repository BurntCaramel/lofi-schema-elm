module Lofi.Schema.Output.GraphQL exposing
  ( createObjectCode
  )

{-|

# Functions
@docs createObjectCode
-}

import Lofi.Schema exposing (Schema, Item, Kind(..))
import String.Extra exposing (underscored, camelize, decapitalize, quote)


lowerCamelize : String -> String
lowerCamelize = camelize >> decapitalize

pairsToObject : String -> List (String, String) -> String
pairsToObject name pairs =
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
    "type " ++ name ++ " " ++ innerCode

createObjectPropertiesCodeFold : Item -> List (Int, (String, String)) -> List (Int, (String, String))
createObjectPropertiesCodeFold item list =
  let
    nativeKind =
      case item.kind of
        Text _ ->
          "String"
        Number _ ->
          "Float"
        _ ->
          "String"
    
    propertyName =
      lowerCamelize item.name

    requiredString =
      if item.optional then
        Nothing
      else
        Just "!"
    
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

{-| Generates GraphQL declaring an object -}
createObjectCode : Schema -> String
createObjectCode schema =
  let
    code =
      List.foldr createObjectPropertiesCodeFold [] schema.items
      |> List.sortBy Tuple.first
      |> List.map Tuple.second
      |> pairsToObject (camelize schema.individualName)
  in
    code
