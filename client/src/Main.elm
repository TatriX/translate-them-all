module Main exposing (..)

import Html exposing (Html, text, div, h1, img, button, input, label, textarea)
import Html.Attributes exposing (src, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


apiUrl : String
apiUrl =
    "http://localhost:8000/message/"


decodePost : Decode.Decoder String
decodePost =
    Decode.at [ "contents" ] Decode.string


makeNewPost : Int -> String -> Cmd Msg
makeNewPost id contents =
    let
        url =
            apiUrl ++ (toString id)

        body =
            Http.jsonBody <| Encode.object [ "contents" => Encode.string contents ]

        request =
            Http.post url body (Decode.succeed contents)
    in
        Http.send NewPost request


getPost : Int -> Cmd Msg
getPost id =
    let
        url =
            apiUrl ++ (toString id)

        request =
            Http.get url decodePost
    in
        Http.send NewPost request



---- MODEL ----


type alias Model =
    { id : Int
    , contents : String
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { id = 0, contents = "", error = Nothing }, Cmd.none )



---- UPDATE ----


type Msg
    = MakePost
    | GetPost
    | NewPost (Result Http.Error String)
    | SetId String
    | SetContents String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPost ->
            ( model, getPost model.id )

        MakePost ->
            ( model, makeNewPost model.id model.contents )

        NewPost result ->
            let
                newModel =
                    case result of
                        Ok contents ->
                            { model | contents = contents, error = Nothing }

                        Err error ->
                            { model | error = Just (toString error) }
            in
                ( newModel, Cmd.none )

        SetId id ->
            ( { model | id = String.toInt id |> Result.toMaybe |> Maybe.withDefault 0 }, Cmd.none )

        SetContents contents ->
            ( { model | contents = contents }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , case model.error of
            Nothing ->
                text ""

            Just error ->
                div [] [ text "Error", text error ]
        , label []
            [ text "id"
            , input [ onInput SetId ] []
            ]
        , label []
            [ text "contents"
            , textarea [ onInput SetContents, value model.contents ] []
            ]
        , button [ onClick <| MakePost ] [ text "Make new post!" ]
        , button [ onClick <| GetPost ] [ text "Get post!" ]
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
