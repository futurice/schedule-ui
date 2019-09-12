module Types exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Set


type ScheduleStatus
    = ScheduleStatusInProgress
    | ScheduleStatusSuccess
    | ScheduleStatusError
    | ScheduleStatusActionFailed


type ModalDialogStatus
    = Opened
    | Closed


scheduleStatusToText : ScheduleStatus -> String
scheduleStatusToText status =
    case status of
        ScheduleStatusInProgress ->
            "In Progress"

        ScheduleStatusSuccess ->
            "Success"

        ScheduleStatusError ->
            "Error"

        ScheduleStatusActionFailed ->
            "Action Failed"


scheduleStatusToHtml : ScheduleStatus -> Html msg
scheduleStatusToHtml status =
    case status of
        ScheduleStatusSuccess ->
            span [ class "label", class "success" ] [ text (scheduleStatusToText status) ]

        ScheduleStatusInProgress ->
            span [ class "label", class "warning" ] [ text (scheduleStatusToText status) ]

        ScheduleStatusError ->
            span [ class "label", class "alert" ] [ text (scheduleStatusToText status) ]

        ScheduleStatusActionFailed ->
            span [ class "label", class "alert" ] [ text (scheduleStatusToText status) ]


type alias Employee =
    { employeeName : String
    , employeeId : Int
    }


type Offset
    = MonthOffset Int
    | DayOffset Int


isDayOffset : Offset -> Bool
isDayOffset offset =
    case offset of
        DayOffset _ ->
            True

        _ ->
            False


isMonthOffset : Offset -> Bool
isMonthOffset =
    not << isDayOffset


unwrapOffset : Offset -> Int
unwrapOffset offset =
    case offset of
        MonthOffset val ->
            val

        DayOffset val ->
            val


mapOffset : (Int -> Int) -> Offset -> Offset
mapOffset f offset =
    case offset of
        MonthOffset val ->
            MonthOffset <| f val

        DayOffset val ->
            DayOffset <| f val


type alias EventTemplate =
    { eventTemplateId : String
    , eventTemplateSummary : String
    , eventTemplateDescription : String
    , eventTemplateOffset : Offset
    , eventTemplateStartTime : String
    , eventTemplateEndTime : String
    , eventTemplateInviteSupervisors : Bool
    , eventTemplateIsCollective : Bool
    , eventTemplateOtherParticipants : Set.Set Int
    }


type alias ScheduleTemplate =
    { templateName : String
    , templateCalendar : String
    , templateTimezone : String
    , eventTemplates : Dict.Dict String EventTemplate
    }


type alias Event =
    { eventSummary : String
    , eventDescription : String
    , eventLocations : List String
    , eventStartTime : String
    , eventEndTime : String
    , eventInviteEmployees : Bool
    , eventInviteSupervisors : Bool
    , eventIsCollective : Bool
    , eventEmployees : Set.Set String
    }


type alias Schedule =
    { templateName : String
    , events : List Event
    , createdBy : String
    , createdOn : String
    , eventIds : List (Maybe String)
    , status : ScheduleStatus
    }


type alias NewScheduleData =
    { templateName : String
    , startDate : String
    , employees : Set.Set Int
    }


scheduleEmployees : Schedule -> List Employee
scheduleEmployees schedule =
    []
