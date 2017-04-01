module Lofi.Schema.Output.MySQL exposing
  ( createTableCommand
  , insertRowCommand
  )

{-|

# Functions
@docs createTableCommand, insertRowCommand
-}

import Lofi.Schema exposing (Schema, Item, Kind(..))
import String.Extra exposing (underscored, quote)


createTableCommandFold : Item -> List (Int, String) -> List (Int, String)
createTableCommandFold item list =
  let
    nativeKind =
      case item.kind of
        Text { maximumLength } ->
          "TEXT"
        Number { real, allowNegative } ->
          "BIGINT"
        Date { time } ->
          "DATE"
    
    columnName =
      underscored item.name

    nullString =
      if item.optional then
        ""
      else
        " NOT NULL"
    
    defaultString =
      let
        result = case item.kind of
          Text { default } ->
            Maybe.map (\default -> " DEFAULT " ++ (quote default)) default
          Number { default } ->
            Maybe.map (\default -> " DEFAULT " ++ (toString default)) default
          Date { time, defaultIsNow } ->
            if time && defaultIsNow then
              Just " DEFAULT CURRENT_TIMESTAMP"
            else
              Nothing
      in
        Maybe.withDefault "" result

    field =
      columnName ++ " " ++ nativeKind ++ nullString ++ defaultString
    
    priority : Int
    priority =
      if item.isPrimaryKey then
        2
      else
        1
  in
    ( priority, field ) :: list

{-| Generates a CREATE TABLE MySQL command for the passed schema -}
createTableCommand : Schema -> String
createTableCommand schema =
  let
    fields =
      List.foldr createTableCommandFold [] schema.items
      |> List.sortBy Tuple.first
      |> List.map Tuple.second
      |> String.join ",\n  "
    
    tableName =
      underscored schema.collectionName      
  in
    "CREATE TABLE " ++ tableName ++ " (\n  " ++ fields ++ "\n);"

{-| Generates a INSERT MySQL command for the passed schema -}
insertRowCommand : Schema -> String
insertRowCommand schema =
  let
    columnNames =
      List.map (\item -> underscored item.name) schema.items
      |> String.join ",\n  "
    
    values =
      List.map (always "?") schema.items
      |> String.join ", "
    
    tableName =
      underscored schema.collectionName      
  in
    "INSERT INTO " ++ tableName ++ " (\n  " ++ columnNames ++ "\n) VALUES (" ++ values ++ " );"