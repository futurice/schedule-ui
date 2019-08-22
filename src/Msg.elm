module Msg exposing (..)

import Browser
import Http
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
