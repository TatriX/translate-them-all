module Main exposing (..)

import Html exposing (Html, button, div, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, src, value)
import Http
import Json.Decode as Decode exposing (int, string, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


apiUrl : String
apiUrl =
    "http://alpha.rogalik.tatrix.org/translate-them-all/api"


getDocuments : Cmd Msg
getDocuments =
    let
        request =
            Http.get (apiUrl ++ "/documents") decodeDocuments
    in
        Http.send GetDocuments request


decodeDocuments : Decoder (List Document)
decodeDocuments =
    list decodeDocument


decodeDocument : Decoder Document
decodeDocument =
    decode Document
        |> required "description" string
        |> required "file" string
        |> required "lines" int
        |> required "translations" (list decodeTranslation)


decodeTranslation : Decoder Translation
decodeTranslation =
    decode Translation
        |> required "lang" decodeLang
        |> required "approved" int
        |> required "pending" int


decodeLang : Decoder Lang
decodeLang =
    string
        |> Decode.andThen
            (\str ->
                case str of
                    "ru" ->
                        Decode.succeed Ru

                    "en" ->
                        Decode.succeed En

                    "ko" ->
                        Decode.succeed Ko

                    "de" ->
                        Decode.succeed De

                    lang ->
                        Decode.fail <| "Unknown lang: " ++ lang
            )



---- MODEL ----


type Lang
    = Ru
    | En
    | Ko
    | De


type alias Translation =
    { lang : Lang
    , approved : Int
    , pendeing : Int
    }


type alias Document =
    { description : String
    , file : String
    , lines : Int
    , translations : List Translation
    }


type alias Model =
    { content : List Document
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { content = [], error = Nothing }
    , getDocuments
    )



---- UPDATE ----


type Msg
    = GetDocuments (Result Http.Error (List Document))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetDocuments result ->
            let
                newModel =
                    case result of
                        Ok content ->
                            { model | content = content, error = Nothing }

                        Err error ->
                            { model | error = Just (toString error) }
            in
                ( newModel, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ table [ class "striped" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Имя файла" ]
                    , th [] [ text "Описание" ]
                    , th [] [ text "Строк" ]
                    , th [] [ text "Процент перевода" ]
                    ]
                ]
            , tbody []
                (List.map (\file -> tr [] [ td [] [ text file.file ], td [] [ text file.description ], td [] [ text (toString file.lines) ], td [] [ text "empty" ] ]) model.content)
            ]
        , span [] [ text (toString model) ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
