module Msg exposing (..)

import Browser
import Http
import Time
import Types exposing (..)
import Url


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url.Url
    | FetchedEmployees (Result Http.Error (List Employee))
    | FetchedSchedules (Result Http.Error (List Schedule))
    | FetchedScheduleTemplates (Result Http.Error (List ScheduleTemplate))
    | FetchedCalendars (Result Http.Error (List String))
    | FetchedTimezones (Result Http.Error (List String))
    | SubmitNewScheduleTemplate
    | NewScheduleTemplateSubmittedResult (Result Http.Error ())
    | EditNewScheduleTemplate ScheduleTemplate
    | DeletedScheduleTemplate (Result Http.Error ())
    | DeleteTemplate String
    | DeleteEventTemplate String String
    | DeletedEventTemplate (Result Http.Error ())
    | EditScheduleTemplateCmd String ScheduleTemplate
    | SubmitEditedScheduleTemplate String
    | ToggleEventTemplateOpenStatus String
    | AddEventTemplate String String
    | EditNewEventTemplate String
    | EditEventTemplate String EventTemplate
    | NewEventTemplateSubmitted (Result Http.Error ())
    | ResetEditedScheduleTemplate String
    | SaveEventTemplates String
    | EditEventTemplateSubmitted String String (Result Http.Error ())
    | SetCurrentTime Time.Posix
    | SetCurrentZone Time.Zone
    | EditNewSchedule NewScheduleData
    | SubmitNewSchedule NewScheduleData
    | EditNewScheduleEventTemplate EventTemplate
    | CancelNewSchedule
