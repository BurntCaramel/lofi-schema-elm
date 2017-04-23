# #lofi #schema in Elm

Low fidelity schemas. Write with a friendly syntax and convert to other forms easily.

## Interactive demo

I’ve made an [online tool to convert #lofi schemas](https://schema.lofi.design/). Convert to React PropTypes, Mongoose schemas, Joi validations, and MySQL commands.

## Basic usage

```elm
import Html exposing (Html, section, h2, pre, text)
import Lofi.Parse exposing (parseElement)
import Lofi.Schema exposing (Schema, fromElement)
import Lofi.Schema.Output.MySQL as MySQL
import Lofi.Schema.Output.Mongoose as Mongoose
import Lofi.Schema.Output.Joi as Joi
import Lofi.Schema.Output.ReactProps as ReactProps
import Lofi.Schema.Output.Swift as Swift
import Lofi.Schema.Output.Elm as Elm
import Lofi.Schema.Output.Go as Go

type alias Model =
  { collectionName : String
  , individualName : String
  , lines : List String
  }

model : Model
model =
  { collectionName = "Users"
  , individualName = "User"
  , lines =
      [ "First name"
      , "Middle name #optional"
      , "Last name #text #max: 255"
      , "Email #email"
      , "Date of birth #date"
      , "Last signed in at #time #now"
      , "Favorite number #number #default: 7"
      ]
  }

view : Model -> Html Msg
view model =
  let
    elements =
      List.map parseElement model.lines

    schema : Schema
    schema =
      { collectionName = model.collectionName
      , individualName = model.individualName
      , items = List.map fromElement elements
      }
  in
    section []
    [ article []
      [ h2 [] [ text "React PropTypes" ]
      , pre [] [ text (ReactProps.createPropTypesCode schema) ]
      ]
    , article []
      [ h2 [] [ text "Mongoose" ]
      , pre [] [ text (Mongoose.createModelCode schema) ]
      ]
    , article []
      [ h2 [] [ text "Swift" ]
      , pre [] [ text (Swift.createStructCode schema) ]
      ]
    , article []
      [ h2 [] [ text "Elm" ]
      , pre [] [ text (Elm.createTypeAliasCode schema) ]
      ]
    , article []
      [ h2 [] [ text "Golang" ]
      , pre [] [ text (Go.createStructCode schema) ]
      ]
    , article []
      [ h2 [] [ text "Joi" ]
      , pre [] [ text (Joi.createSchemaCode schema) ]
      ]
    , article []
      [ h2 [] [ text "MySQL" ]
      , pre [] [ text (MySQL.createTableCommand schema) ]
      , pre [] [ text (MySQL.insertRowCommand schema) ]
      ]
    ]
```
