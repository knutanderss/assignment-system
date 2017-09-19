module Assignment
    exposing
        ( emptyAssignment
        , assignmentCard
        , setTitle
        , setDue
        , setHref
        , setCourse
        , sortAssignments
        , dateToAmerican
        )

import Html exposing (Html, div, button, h4, small, text, p, span, a, Attribute)
import Html.Attributes exposing (class, href, type_)
import Html.Events exposing (onClick)
import Types exposing (Msg(..), Assignment)
import Date exposing (Month(..))
import Date.Extra


emptyAssignment : Assignment
emptyAssignment =
    Assignment "" "" Nothing False Nothing


assignmentCard : Maybe Date.Date -> Assignment -> Html Msg
assignmentCard currentDate assignment =
    let
        { title, course, date, done, href } =
            assignment

        formatDate maybeDate =
            case currentDate of
                Just cDate ->
                    case maybeDate of
                        Just date ->
                            Date.Extra.diff Date.Extra.Day cDate date
                                |> toString

                        Nothing ->
                            dateToAmerican maybeDate

                Nothing ->
                    dateToAmerican date
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
                    [ text <|
                        formatDate date
                            ++ case formatDate date of
                                "1" ->
                                    " day left"

                                x ->
                                    " days left"
                    ]
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


dateToAmerican : Maybe Date.Date -> String
dateToAmerican maybeDate =
    case maybeDate of
        Just date ->
            String.join "/"
                [ toString (monthToInt <| Date.month date)
                , toString (Date.day date)
                , toString (Date.year date)
                ]

        Nothing ->
            ""


formatDateForSorting : Maybe Date.Date -> String
formatDateForSorting maybeDate =
    case String.split "/" (dateToAmerican maybeDate) of
        [ m, d, y ] ->
            y ++ "/" ++ (leadingZeros m) ++ "/" ++ (leadingZeros d)

        otherwise ->
            ""


leadingZeros : String -> String
leadingZeros n =
    case String.toList n of
        [ x ] ->
            String.fromList [ '0', x ]

        x ->
            String.fromList x


monthToInt : Date.Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


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


setTitle : String -> Assignment -> Assignment
setTitle title assignment =
    { assignment | title = title }


setCourse : String -> Assignment -> Assignment
setCourse course assignment =
    { assignment | course = course }


setDue : Maybe Date.Date -> Assignment -> Assignment
setDue due assignment =
    { assignment | date = due }


setHref : String -> Assignment -> Assignment
setHref href assignment =
    { assignment | href = Just href }


sortAssignments : List Assignment -> List Assignment
sortAssignments =
    List.sortBy <| \assignment -> formatDateForSorting assignment.date
