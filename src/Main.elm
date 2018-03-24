port module Main exposing (..)

import Bootstrap.Badge as Badge
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import Markdown


port githubOauthSuccess : (String -> msg) -> Sub msg



---- MODEL ----


type Model
    = Initial
    | Error String
    | Authed String
    | Loaded (List Repo)
    | DataLoadError String


type alias Repo =
    { nameWithOwner : String
    , ownerAvatarUrl : String
    , matchingIssues : List Issue
    }


type alias Issue =
    { title : String
    , url : String
    , body : String
    , authorLogin : String
    , authorAvatarUrl : String
    , labels : List Label
    }


type alias Label =
    { name : String
    , color : String
    }


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
    D.field "accessToken" (D.nullable D.string)



---- UPDATE ----


type Msg
    = GithubOauthSuccess String
    | DataLoaded (Result Http.Error (List Repo))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GithubOauthSuccess token ->
            case model of
                Initial ->
                    Authed token ! [ requestIssues token ]

                _ ->
                    model ! []

        DataLoaded data ->
            case data of
                Ok repos ->
                    Loaded repos ! []

                Err error ->
                    DataLoadError (toString error) ! []



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

                Loaded repos ->
                    div [] (List.concatMap viewRepo repos)

                DataLoadError error ->
                    span [] [ text ("oh no: " ++ error) ]
    in
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ stuff
                , button
                    [ attribute "onclick" "window.hello('github').logout()"
                    ]
                    [ text "Log out" ]
                ]
            ]
        ]


viewRepo : Repo -> List (Html msg)
viewRepo repo =
    List.map (viewIssue repo) repo.matchingIssues


viewIssue : Repo -> Issue -> Html msg
viewIssue repo issue =
    let
        repoTitle =
            em [] [ text repo.nameWithOwner ]

        issueTitleWithLabels =
            span []
                (([ text issue.title ] ++ List.map viewLabel issue.labels)
                    |> List.intersperse (text " ")
                )
    in
    Card.config [ Card.attrs [ Spacing.m3 ] ]
        |> Card.header []
            [ titleLine repo.ownerAvatarUrl repoTitle
            , titleLine issue.authorAvatarUrl issueTitleWithLabels
            ]
        |> Card.block []
            [ Block.text []
                [ Markdown.toHtml [] issue.body
                ]
            ]
        |> Card.view


viewLabel : Label -> Html msg
viewLabel label =
    -- Particular badge type used here doesn't matter as override the
    -- background color, apart from that this badge has dark text which looks
    -- slightly better and is the same as used on GitHub.
    Badge.badgeLight
        [ style [ ( "background-color", "#" ++ label.color ) ] ]
        [ text label.name ]


titleLine : String -> Html msg -> Html msg
titleLine avatarUrl title =
    div []
        [ img [ src avatarUrl, width 30 ] []
        , text " "
        , title
        ]


requestIssues : String -> Cmd Msg
requestIssues accessToken =
    Http.send DataLoaded (postForData accessToken)


postForData : String -> Http.Request (List Repo)
postForData accessToken =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("bearer " ++ accessToken) ]
        , url = "https://api.github.com/graphql"
        , body = Http.jsonBody issuesGraphqlQuery
        , expect = Http.expectJson dataDecoder
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
                starredRepositories(last: 10) {
                  nodes {
                    nameWithOwner
                    owner {
                      avatarUrl
                    }
                    issues(last: 50, labels: ["help wanted"]) {
                      nodes {
                        title
                        url
                        body
                        author {
                          login
                          avatarUrl
                        }
                        labels(first: 10) {
                          nodes {
                            name
                            color
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


dataDecoder : D.Decoder (List Repo)
dataDecoder =
    D.at
        [ "data", "viewer", "starredRepositories", "nodes" ]
        (D.list repoDecoder)


repoDecoder : D.Decoder Repo
repoDecoder =
    D.map3 Repo
        (D.field "nameWithOwner" D.string)
        (D.at [ "owner", "avatarUrl" ] D.string)
        (D.at [ "issues", "nodes" ] (D.list issueDecoder))


issueDecoder : D.Decoder Issue
issueDecoder =
    D.map6 Issue
        (D.field "title" D.string)
        (D.field "url" D.string)
        (D.field "body" D.string)
        (D.at [ "author", "login" ] D.string)
        (D.at [ "author", "avatarUrl" ] D.string)
        (D.at [ "labels", "nodes" ] (D.list labelDecoder))


labelDecoder : D.Decoder Label
labelDecoder =
    D.map2 Label
        (D.field "name" D.string)
        (D.field "color" D.string)



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
