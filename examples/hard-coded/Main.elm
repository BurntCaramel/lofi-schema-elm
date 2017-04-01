module LofiSchemaExampleHardCoded exposing (main)

import Html exposing (Html, section, header, div, h1, h2, pre, textarea, text)
import Html.Attributes exposing (style, rows)
import Html.Events exposing (onInput)
import Lofi.Parse exposing (parseElement)
import Lofi.Schema exposing (Schema, fromElement)
import Lofi.Schema.Output.MySQL as MySQL
import Lofi.Schema.Output.Mongoose as Mongoose
import Lofi.Schema.Output.Joi as Joi


type alias Model =
  { collectionName : String
  , lines : List String
  }

model : Model
model =
  { collectionName = "Users"
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

type Msg
  = AddItem
  | ChangeCollectionName String
  | ChangeSchemaText String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ChangeCollectionName newName ->
      ( { model | collectionName = newName }
      , Cmd.none
      )
    
    ChangeSchemaText text ->
      ( { model | lines = String.split "\n" text }
      , Cmd.none
      )

    AddItem ->
      ( { model | lines = List.append model.lines [""] }
      , Cmd.none
      )

viewSchemaItemRaw : Lofi.Schema.Item -> Html Msg
viewSchemaItemRaw item =
  div []
  [ text item.name
  , text ": "
  , text (toString item.kind)
  ]

viewSchema : Schema -> Html Msg
viewSchema schema =
  div []
  (List.map viewSchemaItemRaw schema.items)

viewCode : String -> Html Msg
viewCode code =
  pre [
    style [ ("overflow", "auto") ]
  ] [ text code ]

view : Model -> Html Msg
view model =
  let
    validLines =
      model.lines
      |> List.map String.trim
      |> List.filter (\s -> String.length s > 0)

    elements =
      List.map parseElement validLines

    schema : Schema
    schema =
      { collectionName = model.collectionName
      , items = List.map fromElement elements
      }
  in
    section [
      style [("maxWidth", "30em"), ("margin", "auto")]
    ]
    [ header []
      [ h1 [] [ text "Write your #lofi schema:" ] ]
    {- , div [] (List.map (\line -> div [] [ text line ]) model.lines) -}
    , div [] [
      textarea
      [ rows 1
      , onInput ChangeCollectionName
      , style [("width", "100%"), ("fontSize", "1rem"), ("resize", "none")]
      ]
      [ text model.collectionName
      ]
    ]
    , div [] [
      textarea
      [ rows (List.length model.lines)
      , onInput ChangeSchemaText
      , style [("width", "100%"), ("fontSize", "1rem"), ("resize", "none")]
      ]
      [ text (String.join "\n" model.lines)
      ]
    ]
    {-, div []
      [ h2 [] [ text "Raw" ]
      , viewSchema schema
      ]-}
    , div []
      [ h2 [] [ text "MySQL" ]
      , viewCode (MySQL.createTableCommand schema)
      , viewCode (MySQL.insertRowCommand schema)
      ]
    , div []
      [ h2 [] [ text "Mongoose" ]
      , viewCode (Mongoose.createModelCode schema)
      ]
    , div []
      [ h2 [] [ text "Joi" ]
      , viewCode (Joi.createSchemaCode schema)
      ]
    ]

main : Program Never Model Msg
main =
  Html.program
    { view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    , init = ( model, Cmd.none )
    }