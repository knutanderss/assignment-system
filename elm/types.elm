module Types exposing (Msg(..), Assignment)

import Http
import Date


type alias Assignment =
    { title : String
    , course : String
    , date : Maybe Date.Date
    , done : Bool
    , href : Maybe String
    }


type Msg
    = FetchSucceed (Result Http.Error (List Assignment))
    | UpdateAssignmentField (String -> Assignment -> Assignment) String
    | UpdateDue String
    | AddAssignment
    | ChangeAssignmentStatus Assignment Bool
    | UpdateSucceed
    | DeleteAssignment Assignment
    | ToggleForm
    | SetDate (Maybe Date.Date)
