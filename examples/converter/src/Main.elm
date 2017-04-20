module Main exposing (main)

import Html exposing (Html, section, header, article, div, h1, h2, pre, textarea, text)
import Html.Attributes exposing (style, rows, placeholder)
import Html.Events exposing (onInput)
import Lofi.Parse exposing (parseElement)
import Lofi.Schema exposing (Schema, fromElement)
import Lofi.Schema.Output.MySQL as MySQL
import Lofi.Schema.Output.Mongoose as Mongoose
import Lofi.Schema.Output.Joi as Joi
import Lofi.Schema.Output.ReactProps as ReactProps
import Lofi.Schema.Output.Swift as Swift
import Lofi.Schema.Output.Elm as Elm


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

type Msg
  = AddItem
  | ChangeCollectionName String
  | ChangeIndividualName String
  | ChangeSchemaText String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ChangeCollectionName newName ->
      ( { model | collectionName = newName }
      , Cmd.none
      )
    
    ChangeIndividualName newName ->
      ( { model | individualName = newName }
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

contentWidthEm : Float
contentWidthEm = 30

viewCode : String -> Html Msg
viewCode code =
  pre [
    style
      [ ("overflow", "auto")
      , ("maxWidth", "none")
      , ("width", "calc(50vw + " ++ (toString (contentWidthEm / 2)) ++ "em)")
      ]
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
      , individualName = model.individualName
      , items = List.map fromElement elements
      }
  in
    section [
      style
        [ ("maxWidth", (toString contentWidthEm) ++ "em")
        , ("margin", "auto")
        ]
    ]
    [ header []
      [ h1 [] [ text "Write your #lofi schema:" ] ]
    {- , div [] (List.map (\line -> div [] [ text line ]) model.lines) -}
    , div []
        [ textarea
          [ rows 1
          , placeholder "Individual name"
          , onInput ChangeIndividualName
          , style [("width", "49.5%"), ("marginRight", "1%"), ("fontSize", "1rem"), ("resize", "none")]
          ]
          [ text model.individualName
          ]
        , textarea
          [ rows 1
          , placeholder "Collection name"
          , onInput ChangeCollectionName
          , style [("width", "49.5%"), ("fontSize", "1rem"), ("resize", "none")]
          ]
          [ text model.collectionName
          ]
        , textarea
          [ rows (List.length model.lines)
          , onInput ChangeSchemaText
          , style [("width", "100%"), ("fontSize", "1rem"), ("resize", "none")]
          ]
          [ text (String.join "\n" model.lines)
          ]
        ]
    {-, article []
      [ h2 [] [ text "Raw" ]
      , viewSchema schema
      ]-}
    , article []
      [ h2 [] [ text "React PropTypes" ]
      , viewCode (ReactProps.createPropTypesCode schema)
      ]
    , article []
      [ h2 [] [ text "Mongoose" ]
      , viewCode (Mongoose.createModelCode schema)
      ]
    , article []
      [ h2 [] [ text "Swift" ]
      , viewCode (Swift.createStructCode schema)
      ]
    , article []
      [ h2 [] [ text "Elm" ]
      , viewCode (Elm.createTypeAliasCode schema)
      ]
    , article []
      [ h2 [] [ text "Joi" ]
      , viewCode (Joi.createSchemaCode schema)
      ]
    , article []
      [ h2 [] [ text "MySQL" ]
      , viewCode (MySQL.createTableCommand schema)
      , viewCode (MySQL.insertRowCommand schema)
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