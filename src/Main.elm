port module Main exposing (..)

import Json.Encode as E
import Json.Decode as D
import Html exposing (..)
import Html.Attributes exposing (..)
import Http


port githubOauthSuccess : (String -> msg) -> Sub msg



---- MODEL ----


type Model
    = Initial
    | Error String
    | Authed String
    | Loaded String
    | IssueLoadError String


init : D.Value -> ( Model, Cmd Msg )
init flags =
    let
        model =
            decodeFlags flags

        commands =
            case model of
                Authed token ->
                    [ requestIssues token ]

                _ ->
                    []
    in
        model ! commands


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
    | IssuesLoaded (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GithubOauthSuccess token ->
            case model of
                Initial ->
                    Authed token ! [ requestIssues token ]

                _ ->
                    model ! []

        IssuesLoaded issues ->
            case issues of
                Ok issues_ ->
                    Loaded issues_ ! []

                Err error ->
                    IssueLoadError (toString error) ! []



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

                Loaded issues ->
                    span [] [ text issues ]

                IssueLoadError error ->
                    span [] [ text ("oh no: " ++ error) ]
    in
        div []
            [ stuff
            , button
                [ attribute "onclick" "window.hello('github').logout()"
                ]
                [ text "Log out" ]
            ]


requestIssues : String -> Cmd Msg
requestIssues accessToken =
    Http.send IssuesLoaded (postForIssues accessToken)


postForIssues : String -> Http.Request String
postForIssues accessToken =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("bearer " ++ accessToken) ]
        , url = "https://api.github.com/graphql"
        , body = Http.jsonBody issuesGraphqlQuery
        , expect =
            Http.expectString
            -- , expect = Http.expectJson packagesDataDecoder
        , timeout = Nothing
        , withCredentials = False
        }


issuesGraphqlQuery : E.Value
issuesGraphqlQuery =
    let
        graphql =
            """
            {
              viewer {
                starredRepositories(last: 50) {
                  nodes {
                    name
                    issues(last: 50, labels: ["help wanted"]) {
                      nodes {
                        title
                        url
                        bodyText
                        author {
                          login
                          avatarUrl
                        }
                        labels(first: 10) {
                          nodes {
                            name
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
            """
    in
        E.object
            [ ( "query"
              , E.string graphql
              )
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
