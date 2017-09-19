module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, type_, class, id, href, for)
import Html.Events exposing (onInput, onClick)
import Json.Decode exposing (string, bool, list, Decoder, field)
import Json.Encode
import Http
import List


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { assignments : List Assignment
    , error : String
    , newAssignment : Assignment
    , formActive : Bool
    }


type alias Assignment =
    { title : String
    , course : String
    , date : String
    , done : Bool
    , href : Maybe String
    }


courses : List String
courses =
    [ "AMAT-108"
    , "AMAT-367"
    , "ICSI-107"
    , "ICSI-333"
    , "ICSI-401"
    ]


init : ( Model, Cmd Msg )
init =
    ( Model [] "" (Assignment "" (Maybe.withDefault "AMAT-108" <| List.head courses) "" False Nothing) False
    , (getAssignments "http://localhost:3000/assignments")
    )



-- UPDATE


type Msg
    = FetchSucceed (Result Http.Error (List Assignment))
    | Title String
    | Course String
    | Due String
    | Href String
    | AddAssignment
    | ChangeAssignmentStatus Assignment Bool
    | UpdateSucceed
    | DeleteAssignment Assignment
    | ToggleForm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchSucceed result ->
            case result of
                Ok assigs ->
                    ( { model | assignments = assigs }, Cmd.none )

                Err err ->
                    case err of
                        Http.BadUrl msg ->
                            ( { model | error = msg }, Cmd.none )

                        Http.Timeout ->
                            ( { model | error = "Timeout" }, Cmd.none )

                        Http.NetworkError ->
                            ( { model | error = "NetworkError" }, Cmd.none )

                        Http.BadStatus msg ->
                            ( { model | error = msg.body }, Cmd.none )

                        Http.BadPayload msg _ ->
                            ( { model | error = msg }, Cmd.none )

        Title title ->
            ( { model | newAssignment = setTitle title model.newAssignment }, Cmd.none )

        Course course ->
            ( { model | newAssignment = setCourse course model.newAssignment }, Cmd.none )

        Due due ->
            ( { model | newAssignment = setDue due model.newAssignment }, Cmd.none )

        Href href ->
            ( { model | newAssignment = setHref href model.newAssignment }, Cmd.none )

        AddAssignment ->
            ( model, addAssignment model.newAssignment "http://localhost:3000/addAssignment" )

        UpdateSucceed ->
            ( model, getAssignments "http://localhost:3000/assignments" )

        ChangeAssignmentStatus assignment done ->
            ( model, changeAssignmentStatus assignment done "http://localhost:3000/changeAssignmentStatus" )

        DeleteAssignment assignment ->
            ( model, deleteAssignment assignment "http://localhost:3000/deleteAssignment" )

        ToggleForm ->
            ( { model | formActive = not model.formActive }, Cmd.none )


setTitle : String -> Assignment -> Assignment
setTitle title assignment =
    { assignment | title = title }


setCourse : String -> Assignment -> Assignment
setCourse course assignment =
    { assignment | course = course }


setDue : String -> Assignment -> Assignment
setDue due assignment =
    { assignment | date = due }


setHref : String -> Assignment -> Assignment
setHref href assignment =
    { assignment | href = Just href }


sortAssignments : List Assignment -> List Assignment
sortAssignments =
    List.sortBy <| \assignment -> formatDateForSorting assignment.date


formatDateForSorting : String -> String
formatDateForSorting date =
    case String.split "/" date of
        [ month, day, year ] ->
            year ++ "/" ++ month ++ "/" ++ day

        otherwise ->
            "error"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ nav
            [ class "navbar navbar-inverse bg-inverse" ]
            []
        , div
            [ class "container" ]
            [ headline
            , newAssignmentForm model.formActive
            , div
                [ class "assignments" ]
                [ h2 [] [ text "List of assignments" ]
                , small [] [ text model.error ]
                , div [ id "assignment-list", class "list-group" ] <|
                    List.map assignmentCard <|
                        sortAssignments model.assignments
                ]
            ]
        ]


headline : Html Msg
headline =
    h1
        [ class "text-center" ]
        [ text "Assignments UAlbany" ]


newAssignmentForm : Bool -> Html Msg
newAssignmentForm active =
    div
        [ class "card" ]
        [ h3
            [ class "card-header", id "addAssignmentHeader", onClick ToggleForm ]
            [ text "Add Assignment" ]
        , div
            [ class <|
                "card-block"
                    ++ case active of
                        True ->
                            ""

                        False ->
                            " formDisabled"
            ]
            [ form
                [ id "add-assignment" ]
                [ div
                    [ class "form-group" ]
                    [ label
                        [ for "assignment-title" ]
                        [ text "Assignment Title" ]
                    , input
                        [ type_ "text", class "form-control", id "assignment-title", onInput Title ]
                        []
                    ]
                , div
                    [ class "form-group" ]
                    [ label
                        [ for "due" ]
                        [ text "Due:" ]
                    , input
                        [ placeholder "MM/DD/YYYY", type_ "text", class "form-control", id "due", onInput Due ]
                        []
                    ]
                , div
                    [ class "form-group" ]
                    [ label
                        [ for "course" ]
                        [ text "Course:" ]
                    , select
                        [ class "form-control", id "course", onInput Course ]
                        (dropDownSelector courses)
                    ]
                , div
                    [ class "form-group" ]
                    [ label
                        [ for "href" ]
                        [ text "Href:" ]
                    , input
                        [ placeholder "", type_ "text", class "form-control", id "href", onInput Href ]
                        (dropDownSelector courses)
                    ]
                , button
                    [ type_ "button"
                    , class "btn btn-primary btn-md"
                    , onClick AddAssignment
                    ]
                    [ text "Add Assignment" ]
                ]
            ]
        ]


dropDownSelector : List String -> List (Html msg)
dropDownSelector options =
    let
        toOption str =
            option [] [ text str ]
    in
        List.map toOption options


assignmentCard : Assignment -> Html Msg
assignmentCard assignment =
    let
        { title, course, date, done, href } =
            assignment
    in
        div
            [ doneClass done ]
            [ div
                [ class "d-flex w-100 justify-content-between" ]
                [ h4
                    [ class "mb-1" ]
                    [ titleWithMaybeHref title href ]
                , small
                    []
                    [ text date ]
                ]
            , div
                [ class "d-flex w-100 justify-content-between" ]
                [ p
                    [ class "mb-1 text-left" ]
                    [ text course ]
                , div [ class "d-flex justify-content-end" ]
                    [ button
                        [ type_ "button"
                        , onClick <| ChangeAssignmentStatus assignment <| not done
                        , (class <|
                            "btn "
                                ++ if done then
                                    " btn-warning"
                                   else
                                    " btn-success"
                          )
                        ]
                        (case done of
                            False ->
                                [ span [ class "fa fa-check" ] []
                                , text "Done"
                                ]

                            True ->
                                [ text "Undone" ]
                        )
                    , button
                        [ type_ "button", class "btn btn-danger", onClick <| DeleteAssignment assignment ]
                        [ span [ class "fa fa-trash" ] [] ]
                    ]
                ]
            ]


titleWithMaybeHref : String -> Maybe String -> Html Msg
titleWithMaybeHref title maybeHref =
    case maybeHref of
        Just hrefString ->
            a [ href hrefString ] [ text title ]

        Nothing ->
            text title


doneClass : Bool -> Attribute Msg
doneClass done =
    let
        doneCl =
            case done of
                True ->
                    "list-group-item-success"

                False ->
                    "list-group-item-warning"
    in
        class <| "list-group-item flex-column align-items-start " ++ doneCl



-- HTTP


addAssignment : Assignment -> String -> Cmd Msg
addAssignment newAssignment url =
    Http.post url (Http.jsonBody <| encodeAssignment newAssignment) (Json.Decode.succeed "Woho")
        |> Http.send (always UpdateSucceed)


changeAssignmentStatus : Assignment -> Bool -> String -> Cmd Msg
changeAssignmentStatus assignment newDone url =
    Http.post
        url
        (Http.jsonBody <| encodeAssignment { assignment | done = newDone })
        (Json.Decode.succeed "Woho")
        |> Http.send (always UpdateSucceed)


deleteAssignment : Assignment -> String -> Cmd Msg
deleteAssignment assignment url =
    Http.post
        url
        (Http.jsonBody <| encodeAssignment assignment)
        (Json.Decode.succeed "Woho")
        |> Http.send (always UpdateSucceed)


encodeAssignment : Assignment -> Json.Encode.Value
encodeAssignment assignment =
    Json.Encode.object <|
        [ ( "title", Json.Encode.string assignment.title )
        , ( "course", Json.Encode.string assignment.course )
        , ( "due", Json.Encode.string assignment.date )
        , ( "done", Json.Encode.bool assignment.done )
        ]
            ++ case assignment.href of
                Just hrefString ->
                    [ ( "href", Json.Encode.string hrefString ) ]

                Nothing ->
                    []


getAssignments : String -> Cmd Msg
getAssignments url =
    Http.send FetchSucceed (Http.get url decodeAssignments)


decodeAssignments : Decoder (List Assignment)
decodeAssignments =
    Json.Decode.list decodeAssignment


decodeAssignment : Decoder Assignment
decodeAssignment =
    Json.Decode.map5 Assignment
        (field "title" string)
        (field "course" string)
        (field "due" string)
        (field "done" bool)
        (Json.Decode.maybe (field "href" string))
