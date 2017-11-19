module Main exposing (..)

import Array
import Html exposing (Html, a, button, div, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href, src, value)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)


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
    , pending : Int
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


fileToLangCode : String -> String
fileToLangCode file =
    Maybe.withDefault "??" <| List.head <| (String.split "." file)


viewTranslationRow : Document -> Translation -> Html msg
viewTranslationRow file translation =
    a
        [ href <|
            (fileToLangCode file.file)
                ++ "/"
                ++ toString translation.lang
        , class "waves-effect waves-light chip"
        ]
        [ text (toString translation.lang ++ ": " ++ toString (translation.approved * 100 // file.lines) ++ "%")
        ]


viewFileRow : Document -> Html msg
viewFileRow file =
    tr []
        [ td [] [ text file.file ]
        , td [] [ text file.description ]
        , td [] [ text (toString file.lines) ]
        , td [] <| List.map (viewTranslationRow file) file.translations
        ]


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
            , tbody [] <| List.map viewFileRow model.content
            ]
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
