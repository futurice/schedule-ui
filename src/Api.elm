module Api exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder, bool, dict, field, float, int, list, nullable, oneOf, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Msg exposing (..)
import Set
import Types exposing (..)


rootUrl : String
rootUrl =
    "http://localhost:8826"


employeeDecoder : Decoder Employee
employeeDecoder =
    Decode.succeed
        Employee
        |> required "name" string
        |> required "id" int


fetchEmployees : Cmd Msg
fetchEmployees =
    Http.get
        { url = rootUrl ++ "/employees/"
        , expect = Http.expectJson FetchedEmployees (list employeeDecoder)
        }


fetchSchedules : Cmd Msg
fetchSchedules =
    Http.get
        { url = rootUrl ++ "/schedule/"
        , expect = Http.expectJson FetchedSchedules (list scheduleDecoder)
        }


fetchScheduleTemplates : Cmd Msg
fetchScheduleTemplates =
    Http.get
        { url = rootUrl ++ "/schedule-template/"
        , expect = Http.expectJson FetchedScheduleTemplates (list scheduleTemplateDecoder)
        }


fetchCalendars : Cmd Msg
fetchCalendars =
    Http.get
        { url = rootUrl ++ "/calendar/"
        , expect = Http.expectJson FetchedCalendars (list string)
        }


fetchTimezones : Cmd Msg
fetchTimezones =
    Http.get
        { url = rootUrl ++ "/timezone/"
        , expect = Http.expectJson FetchedTimezones (list string)
        }


postNewScheduleTemplate : ScheduleTemplate -> Cmd Msg
postNewScheduleTemplate scheduleTemplate =
    Http.post
        { url = rootUrl ++ "/schedule-template/"
        , body = Http.jsonBody <| scheduleTemplateToJson scheduleTemplate
        , expect = Http.expectWhatever NewScheduleTemplateSubmittedResult
        }



-- TODO: add confirm dialog?


deleteScheduleTemplate : String -> Cmd Msg
deleteScheduleTemplate templateName =
    Http.request
        { url = rootUrl ++ "/schedule-template/"
        , method = "DELETE"
        , headers = []
        , body = Http.jsonBody (Encode.string templateName)
        , expect = Http.expectWhatever DeletedScheduleTemplate
        , timeout = Nothing
        , tracker = Nothing
        }


deleteEventTemplate : String -> String -> Cmd Msg
deleteEventTemplate scheduleTemplateId eventTemplateId =
    Http.post
        { url = rootUrl ++ "/commands/delete-event-template/"
        , body = Http.jsonBody <| deleteEventTemplateJson scheduleTemplateId eventTemplateId
        , expect = Http.expectWhatever DeletedEventTemplate
        }


addEventTemplate : String -> String -> Cmd Msg
addEventTemplate scheduleName summary =
    Http.post
        { url = rootUrl ++ "/commands/add-event-template"
        , body = Http.jsonBody <| eventTemplateCreationToJson scheduleName summary
        , expect = Http.expectWhatever NewEventTemplateSubmitted
        }


editEventTemplate : String -> EventTemplate -> Cmd Msg
editEventTemplate scheduleTemplateName eventTemplate =
    Http.post
        { url = rootUrl ++ "/commands/edit-event-template"
        , body = Http.jsonBody <| editEventTemplateJson scheduleTemplateName eventTemplate
        , expect = Http.expectWhatever (EditEventTemplateSubmitted scheduleTemplateName eventTemplate.eventTemplateId)
        }


eventDecoder : Decoder Event
eventDecoder =
    Decode.succeed
        Event
        |> required "summary" string
        |> required "description" string
        |> required "locations" (list string)
        |> required "startTime" string
        |> required "endTime" string
        |> required "inviteEmployees" bool
        |> required "inviteSupervisors" bool
        |> required "eventIsCollective" bool
        |> required "eventEmployees" (Decode.map Set.fromList <| list string)


scheduleDecoder : Decoder Schedule
scheduleDecoder =
    Decode.succeed
        Schedule
        |> required "templateName" string
        |> required "events" (list eventDecoder)
        |> required "createdBy" string
        |> required "createdOn" string
        |> required "eventids" (list (nullable string))
        |> hardcoded ScheduleStatusSuccess


monthOffsetDecoder : Decoder Offset
monthOffsetDecoder =
    Decode.succeed
        MonthOffset
        |> required "Right" int


monthOffsetEncoder : Int -> Encode.Value
monthOffsetEncoder offset =
    Encode.object [ ( "Right", Encode.int offset ) ]


dayOffsetDecoder : Decoder Offset
dayOffsetDecoder =
    Decode.succeed
        DayOffset
        |> required "Left" int


dayOffsetEncoder : Int -> Encode.Value
dayOffsetEncoder offset =
    Encode.object
        [ ( "Left", Encode.int offset ) ]


eventTemplateDecoder : Decoder EventTemplate
eventTemplateDecoder =
    Decode.succeed
        EventTemplate
        |> required "eventTemplateId" string
        |> required "summary" string
        |> required "description" string
        |> required "timeOffset" (oneOf [ dayOffsetDecoder, monthOffsetDecoder ])
        |> required "startTime" string
        |> required "endTime" string
        |> required "inviteSupervisors" bool
        |> required "isCollective" bool
        |> required "otherParticipants" (Decode.map Set.fromList <| list int)


scheduleTemplateDecoder : Decoder ScheduleTemplate
scheduleTemplateDecoder =
    Decode.succeed
        ScheduleTemplate
        |> required "name" string
        |> required "calendar" string
        |> required "timezone" string
        |> required "eventTemplates" (dict eventTemplateDecoder)


scheduleTemplateToJson : ScheduleTemplate -> Encode.Value
scheduleTemplateToJson scheduleTemplate =
    Encode.object
        [ ( "name", Encode.string scheduleTemplate.templateName )
        , ( "calendar", Encode.string scheduleTemplate.templateCalendar )
        , ( "timeZone", Encode.string scheduleTemplate.templateTimezone )
        ]


deleteEventTemplateJson : String -> String -> Encode.Value
deleteEventTemplateJson scheduleTemplateId eventTemplateId =
    Encode.object
        [ ( "scheduleTemplate", Encode.string scheduleTemplateId )
        , ( "id", Encode.string eventTemplateId )
        ]


eventTemplateCreationToJson : String -> String -> Encode.Value
eventTemplateCreationToJson scheduleName eventTemplateSummary =
    Encode.object
        [ ( "scheduleTemplateName", Encode.string scheduleName )
        , ( "eventTemplateSummary", Encode.string eventTemplateSummary )
        ]


editEventTemplateJson : String -> EventTemplate -> Encode.Value
editEventTemplateJson scheduleTemplateName eventTemplate =
    let
        offsetValue =
            case eventTemplate.eventTemplateOffset of
                MonthOffset offset ->
                    monthOffsetEncoder offset

                DayOffset offset ->
                    dayOffsetEncoder offset
    in
    Encode.object
        [ ( "scheduleTemplateName", Encode.string scheduleTemplateName )
        , ( "eventTemplateId", Encode.string eventTemplate.eventTemplateId )
        , ( "summary", Encode.string eventTemplate.eventTemplateSummary )
        , ( "description", Encode.string eventTemplate.eventTemplateDescription )
        , ( "timeOffset", offsetValue )
        , ( "startTime", Encode.string eventTemplate.eventTemplateStartTime )
        , ( "endTime", Encode.string eventTemplate.eventTemplateEndTime )
        , ( "inviteEmployees", Encode.bool True )
        , ( "inviteSupervisors", Encode.bool eventTemplate.eventTemplateInviteSupervisors )
        , ( "isCollective", Encode.bool eventTemplate.eventTemplateIsCollective )
        , ( "otherParticipants", Encode.list Encode.int <| Set.toList eventTemplate.eventTemplateOtherParticipants )
        ]
