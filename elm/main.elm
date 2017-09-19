module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, type_, class, id, href, for)
import Json.Decode exposing (string, bool, list, Decoder, field)
import Json.Encode
import Http
import List
import Assignment exposing (assignmentCard, setDue, sortAssignments)
import Types exposing (Msg(..), Assignment)
import NewAssignmentForm exposing (newAssignmentForm)
import Date
import Task


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { assignments : List Assignment
    , error : String
    , newAssignment : Assignment
    , formActive : Bool
    , currentDate : Maybe Date.Date
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
    Model [] "" Assignment.emptyAssignment False Nothing
        ! [ getAssignments "http://localhost:3000/assignments"
          , Task.perform (Just >> SetDate) Date.now
          ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchSucceed result ->
            case result of
                Ok assigs ->
                    { model | assignments = assigs } ! []

                Err err ->
                    case err of
                        Http.BadUrl msg ->
                            { model | error = msg } ! []

                        Http.Timeout ->
                            { model | error = "Timeout" } ! []

                        Http.NetworkError ->
                            { model | error = "NetworkError" } ! []

                        Http.BadStatus msg ->
                            { model | error = msg.body } ! []

                        Http.BadPayload msg _ ->
                            { model | error = msg } ! []

        UpdateAssignmentField updateFunction value ->
            { model | newAssignment = updateFunction value model.newAssignment } ! []

        AddAssignment ->
            ( model, addAssignment model.newAssignment "http://localhost:3000/addAssignment" )

        UpdateSucceed ->
            ( model, getAssignments "http://localhost:3000/assignments" )

        ChangeAssignmentStatus assignment done ->
            ( model, changeAssignmentStatus assignment done "http://localhost:3000/changeAssignmentStatus" )

        DeleteAssignment assignment ->
            ( model, deleteAssignment assignment "http://localhost:3000/deleteAssignment" )

        ToggleForm ->
            { model | formActive = not model.formActive } ! []

        SetDate date ->
            { model | currentDate = date } ! []

        UpdateDue dateString ->
            case Date.fromString dateString of
                Ok date ->
                    { model | newAssignment = setDue (Just date) model.newAssignment } ! []

                otherwise ->
                    { model | newAssignment = setDue Nothing model.newAssignment } ! []



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
            , newAssignmentForm model.formActive courses
            , div
                [ class "assignments" ]
                [ h2 [] [ text "List of assignments" ]
                , small [] [ text model.error ]
                , div [ id "assignment-list", class "list-group" ] <|
                    List.map (assignmentCard model.currentDate) <|
                        sortAssignments model.assignments
                ]
            ]
        ]


headline : Html Msg
headline =
    h1
        [ class "text-center" ]
        [ text "Assignments UAlbany" ]



-- HTTP


addAssignment : Assignment -> String -> Cmd Msg
addAssignment newAssignment url =
    let
        newAssignmentWCourse =
            case newAssignment.course of
                "" ->
                    Assignment.setCourse
                        (Maybe.withDefault "No course" <| List.head courses)
                        newAssignment

                otherwise ->
                    newAssignment
    in
        Http.post url (Http.jsonBody <| encodeAssignment newAssignmentWCourse) (Json.Decode.succeed "Woho")
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
        , ( "due", Json.Encode.string <| Assignment.dateToAmerican assignment.date )
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
        (Json.Decode.map (Result.toMaybe << Date.fromString) (field "due" string))
        (field "done" bool)
        (Json.Decode.maybe (field "href" string))
