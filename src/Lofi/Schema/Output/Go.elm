module Lofi.Schema.Output.Go exposing
  ( createStructCode
  )

{-|

# Functions
@docs createStructCode
-}

import Lofi.Schema exposing (Schema, Item, Kind(..))
import String.Extra exposing (underscored, camelize, classify, decapitalize, quote)


type alias Prop =
  { name : String
  , type_ : String
  , jsonName : String
  }

lowerCamelize : String -> String
lowerCamelize = camelize >> decapitalize

propsToStruct : String -> List Prop -> String
propsToStruct name pairs =
  let
    propertyDivider =
      "\n\t"
    
    wrapInBraces =
      \s -> "{\n\t" ++ s ++ "\n}"
    
    maxNameLength =
      pairs
      |> List.map (\{ name } -> String.length name)
      |> List.maximum
      |> Maybe.withDefault 4
    
    maxTypeLength =
      pairs
      |> List.map (\{ type_ } -> String.length type_)
      |> List.maximum
      |> Maybe.withDefault 4
  
    innerCode =
      pairs
      |> List.map (\{ name, type_, jsonName } -> (String.padRight maxNameLength ' ' name) ++ " " ++ (String.padRight maxTypeLength ' ' type_) ++ " `json:\"" ++ jsonName ++ "\"`")
      |> String.join propertyDivider
      |> wrapInBraces
  in
    "type " ++ name ++ " struct " ++ innerCode

createStructPropertiesCodeFold : Item -> List (Int, Prop) -> List (Int, Prop)
createStructPropertiesCodeFold item list =
  let
    nativeKind =
      case item.kind of
        Text _ ->
          "string"
        Number _ ->
          "float64"
        Date _ ->
          "time.Time"
    
    propertyName =
      classify item.name
    
    schemaString =
      [ Just nativeKind
      ]
      |> List.filterMap identity
      |> String.join ""
    
    jsonPropertyName =
      lowerCamelize item.name

    field =
      { name = propertyName
      , type_ = schemaString
      , jsonName = jsonPropertyName
      }
    
    priority : Int
    priority =
      if item.isPrimaryKey then
        2
      else
        1
  in
    ( priority, field ) :: list

{-| Generates Golang code declaring a struct -}
createStructCode : Schema -> String
createStructCode schema =
  let
    structCode =
      List.foldr createStructPropertiesCodeFold [] schema.items
      |> List.sortBy Tuple.first
      |> List.map Tuple.second
      |> propsToStruct (camelize schema.individualName)
  in
    structCode
