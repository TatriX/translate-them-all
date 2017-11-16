module Main exposing (..)

import Html exposing (Html, button, div, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, src, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


apiUrl : String
apiUrl =
    "http://alpha.rogalik.tatrix.org/translate-them-all/api/documents"


decodePost : Decode.Decoder String
decodePost =
    Decode.at [ "content" ] Decode.string


getDocuments : Cmd Msg
getDocuments =
    let
        url =
            apiUrl

        request =
            Http.get url decodePost
    in
    Http.send GetDocuments request



---- MODEL ----


type alias Translation =
    { x : Int
    , y : Int
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
    = GetDocuments (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetDocuments result ->
            let
                newModel =
                    case result of
                        Ok content ->
                            { model | error = Nothing }

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
