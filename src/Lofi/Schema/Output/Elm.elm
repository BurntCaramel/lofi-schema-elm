module Lofi.Schema.Output.Elm exposing
  ( createTypeAliasCode
  )

{-|

# Functions
@docs createTypeAliasCode
-}

import Lofi.Schema exposing (Schema, Item, Kind(..))
import String.Extra exposing (underscored, camelize, decapitalize, quote)


lowerCamelize : String -> String
lowerCamelize = camelize >> decapitalize

pairsToRecordTypeAlias : String -> List (String, String) -> String
pairsToRecordTypeAlias name pairs =
  let
    propertyDivider =
      "\n  , "
    
    wrapInBraces =
      \s -> "  { " ++ s ++ "\n  }"
  
    innerCode =
      pairs
      |> List.map (\(a, b) -> "" ++ a ++ " : " ++ b)
      |> String.join propertyDivider
      |> wrapInBraces
  in
    "type alias " ++ name ++ " =\n" ++ innerCode

createRecordPropertiesCodeFold : Item -> List (Int, (String, String)) -> List (Int, (String, String))
createRecordPropertiesCodeFold item list =
  let
    nativeKind =
      case item.kind of
        Text _ ->
          "String"
        Number _ ->
          "number"
        Date _ ->
          "Date"
    
    propertyName =
      lowerCamelize item.name

    requiredString =
      if item.optional then
        Just "Maybe "
      else
        Nothing
    
    schemaString =
      [ requiredString
      , Just nativeKind
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

{-| Generates Elm code declaring a record type alias -}
createTypeAliasCode : Schema -> String
createTypeAliasCode schema =
  let
    structCode =
      List.foldr createRecordPropertiesCodeFold [] schema.items
      |> List.sortBy Tuple.first
      |> List.map Tuple.second
      |> pairsToRecordTypeAlias (camelize schema.individualName)
  in
    structCode
