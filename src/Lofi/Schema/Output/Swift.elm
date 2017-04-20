module Lofi.Schema.Output.Swift exposing
  ( createStructCode
  )

{-|

# Functions
@docs createStructCode
-}

import Lofi.Schema exposing (Schema, Item, Kind(..))
import String.Extra exposing (underscored, camelize, decapitalize, quote)


lowerCamelize : String -> String
lowerCamelize = camelize >> decapitalize

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
        Date { time } ->
          if time then
            "Date"
          else
            "DateComponents"
    
    propertyName =
      lowerCamelize item.name

    requiredString =
      if item.optional then
        Just "?"
      else
        Nothing
    
    defaultValue =
      case item.kind of
        Text { default } ->
          Maybe.map (\default -> " = " ++ (quote default)) default
        Number { default } ->
          Maybe.map (\default -> " = " ++ (toString default)) default
        Date { time, defaultIsNow } ->
          if defaultIsNow then
            if time then
              Just " = Date()"
            else
              Just " = DateComponents()"
          else
            Nothing
    
    schemaString =
      [ Just nativeKind
      , requiredString
      , defaultValue
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
