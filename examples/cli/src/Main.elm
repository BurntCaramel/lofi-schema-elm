port module Main
    exposing
        ( main
        )

import Json.Decode
import Lofi.Parse
import Lofi.Schema exposing (Schema)
import Lofi.Schema.Output.GraphQL as GraphQL
import Lofi.Schema.Output.MySQL as MySQL
import Lofi.Schema.Output.Mongoose as Mongoose
import Lofi.Schema.Output.Joi as Joi
import Lofi.Schema.Output.ReactProps as ReactProps
import Lofi.Schema.Output.Swift as Swift
import Lofi.Schema.Output.Elm as Elm
import Lofi.Schema.Output.Go as Go
import Lofi.Schema.Output.Julia as Julia


type alias ConversionRequest =
    { collectionName : String
    , individualName : String
    , inputFormat : String
    , input : String
    , outputFormat : String
    }

type alias ConversionOutput =
    { format : String
    , errorMessage : Maybe String
    , result: Maybe String
    }


type ConversionError =
    UnknownInputFormat String


port beginConversion : (ConversionRequest -> msg) -> Sub msg


port conversionComplete : ConversionOutput -> Cmd msg


type alias Flags =
    {}


type alias Model =
    {}


type Msg
    = BeginConversion ConversionRequest
    | None


requestToSchema : ConversionRequest -> Result ConversionError Schema
requestToSchema request =
    let
        validLines =
            request.input
                |> String.split "\n"
                |> List.map String.trim
                |> List.filter (\s -> String.length s > 0)

        elements =
            List.map Lofi.Parse.parseElement validLines

        schema : Schema
        schema =
            { collectionName = request.collectionName
            , individualName = request.individualName
            , items = elements |> List.map Lofi.Schema.fromElement
            }
    in
        Ok schema


init : Flags -> ( Model, Cmd Msg )
init flags =
    {} ! []


conformFormat : String -> String
conformFormat input =
    case String.toLower input of
        "go" ->
            "golang"

        format ->
            format


convertToFormat : String -> Schema -> Maybe String
convertToFormat format schema =
    case conformFormat format of
        "graphql" ->
            Just <| GraphQL.createObjectCode schema

        "golang" ->
            Just <| Go.createStructCode schema

        "prop-types" ->
            Just <| ReactProps.createPropTypesCode schema

        "swift" ->
            Just <| Swift.createStructCode schema

        "mongoose" ->
            Just <| Mongoose.createModelCode schema

        _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BeginConversion request ->
            let
                output =
                    case requestToSchema request of
                        Ok schema ->
                            ConversionOutput request.outputFormat Nothing (convertToFormat request.outputFormat schema)
                        
                        Err error ->
                            ConversionOutput request.outputFormat (Just <| toString error) Nothing
            in
                model
                    ! [ conversionComplete output
                      ]

        None ->
            model ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ beginConversion BeginConversion
        ]


main : Program Flags Model Msg
main =
    Platform.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
