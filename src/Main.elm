port module Main exposing (..)

import Json.Decode as D
import Html exposing (..)
import Html.Attributes exposing (..)


port githubOauthSuccess : (String -> msg) -> Sub msg



---- MODEL ----


type Model
    = Initial
    | Authed String
    | Error String


init : D.Value -> ( Model, Cmd Msg )
init flags =
    let
        model =
            decodeFlags flags
    in
        model ! []


decodeFlags : D.Value -> Model
decodeFlags flagsJson =
    let
        decodeResult =
            D.decodeValue flagsDecoder flagsJson
    in
        case decodeResult of
            Ok token ->
                case token of
                    Just token_ ->
                        Authed token_

                    Nothing ->
                        Initial

            Err message ->
                Error message


flagsDecoder : D.Decoder (Maybe String)
flagsDecoder =
    (D.field "accessToken" (D.nullable D.string))



---- UPDATE ----


type Msg
    = GithubOauthSuccess String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GithubOauthSuccess token ->
            case model of
                Initial ->
                    Debug.log "post-initial" Authed token ! []

                Error _ ->
                    Debug.log "post-error" Authed token ! []

                _ ->
                    model ! []



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        stuff =
            case model of
                Initial ->
                    div []
                        [ button
                            [ attribute "onclick" "window.hello('github').login()"
                            ]
                            [ text "Authenticate!" ]
                        ]

                Authed data ->
                    span [] [ text ("hooray: " ++ data) ]

                Error error ->
                    span [] [ text ("oh no: " ++ error) ]
    in
        div []
            [ stuff
            , button
                [ attribute "onclick" "window.hello('github').logout()"
                ]
                [ text "Log out" ]
            ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    githubOauthSuccess GithubOauthSuccess



---- PROGRAM ----


main : Program D.Value Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
