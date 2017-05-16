module Lofi.Schema exposing
  ( Kind(..)
  , Item
  , Schema
  , fromElement
  )

{-|

# Types
@docs Kind, Item, Schema

# Functions
@docs fromElement
-}

import Lofi exposing (Element(..), Tags, TagValue)
import Dict exposing (Dict)

{-| The kind of value, e.g. text, number, date -}
type Kind
  = Text
    { maximumLength : Maybe Int
    , isEmail : Bool
    , default : Maybe String
    }
  | Number
    { real : Bool -- TODO?: change to quantity : Bool
    , allowNegative : Bool -- TODO: change to min : Maybe Float
    , default : Maybe Float
    -- TODO?: Add defaultInt : Int
    }
  | Date
    { time : Bool
    , defaultIsNow : Bool
    }

{-| An individual attribute of a schema -}
type alias Item =
  { name : String
  , kind : Kind
  , optional : Bool
  , isPrimaryKey : Bool
  }

{-| A representation of a model and its names and kinds -}
type alias Schema =
  { collectionName : String
  , individualName : String
  , items : List Item
  }


hasTagIn : Tags -> String -> Bool
hasTagIn tags tagName =
  Dict.member tagName tags

tagTextValueIn : Tags -> String -> Maybe String
tagTextValueIn tags tagName =
  Dict.get tagName tags
  |> Maybe.andThen (\tagValue ->
    case tagValue of
      Lofi.Content {texts, mentions} ->
        Just (String.join "" texts)
      _ ->
        Nothing
  )


tagsToKind : Tags -> Kind
tagsToKind tags =
  let
    hasTag =
      hasTagIn tags
    
    tagText =
      tagTextValueIn tags
  in
    if hasTag "time" then
      Date { time = True, defaultIsNow = hasTag "now" }
    else if hasTag "date" then
      Date { time = False, defaultIsNow = hasTag "now" }
    else if hasTag "number" then
      let
        default =
          tagText "default"
          |> Maybe.andThen
            (  String.toFloat
            >> Result.toMaybe
            )
      in
        Number { real = True, allowNegative = True, default = default }
    else
      let
        tagMaybeInt tagValue =
          case tagValue of
            Lofi.Content { texts, mentions } ->
              case texts of
                [text] ->
                  String.toInt text
                  |> Result.toMaybe
                
                _ ->
                  Nothing
            _ ->
              Nothing
        
        maximumLength =
          Dict.get "max" tags
          |> Maybe.andThen tagMaybeInt
        
        isEmail =
          hasTag "email"
      in
        Text { maximumLength = maximumLength, isEmail = isEmail, default = tagText "default" }

{-| Convert a Lofi.Element to a schema Item -}
fromElement : Element -> Item
fromElement element =
  case element of
    Element {texts, mentions, tags} ->
      let
        hasTag =
          hasTagIn tags
        
        name =
          String.join "" texts -- TODO mentions

        kind =
          tagsToKind tags
        
        optional =
          hasTag "optional"

        isPrimaryKey =
          hasTag "primary"
      in
        { name = name
        , kind = kind
        , optional = optional
        , isPrimaryKey = isPrimaryKey
        }

{-| Convert a List of Lofi.Element to Schema -}
addElements : List Element -> Schema -> Schema
addElements elements schema =
  let
    items = List.map fromElement elements
  in
    { schema | items = schema.items ++ items }