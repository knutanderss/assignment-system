module NewAssignmentForm exposing (newAssignmentForm)

import Html exposing (Html, div, h3, text, form, input, label, select, option, button)
import Html.Attributes exposing (class, id, for, type_, placeholder)
import Html.Events exposing (onClick, onInput)
import Types exposing (Msg(..))
import Assignment exposing (setTitle, setDue, setCourse, setHref)


newAssignmentForm : Bool -> List String -> Html Msg
newAssignmentForm active courses =
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
                        [ type_ "text", class "form-control", id "assignment-title", onInput <| UpdateAssignmentField setTitle ]
                        []
                    ]
                , div
                    [ class "form-group" ]
                    [ label
                        [ for "due" ]
                        [ text "Due:" ]
                    , input
                        [ placeholder "MM/DD/YYYY", type_ "text", class "form-control", id "due", onInput <| UpdateDue ]
                        []
                    ]
                , div
                    [ class "form-group" ]
                    [ label
                        [ for "course" ]
                        [ text "Course:" ]
                    , select
                        [ class "form-control", id "course", onInput <| UpdateAssignmentField setCourse ]
                        (dropDownSelector courses)
                    ]
                , div
                    [ class "form-group" ]
                    [ label
                        [ for "href" ]
                        [ text "Href:" ]
                    , input
                        [ placeholder "", type_ "text", class "form-control", id "href", onInput <| UpdateAssignmentField setHref ]
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
