module Model exposing (..)

import Api exposing (..)
import Browser.Navigation as Nav
import Dict
import Msg exposing (..)
import Routing exposing (..)
import Set
import Types exposing (..)


emptyNewScheduleTemplate : ScheduleTemplate
emptyNewScheduleTemplate =
    { templateName = ""
    , templateCalendar = ""
    , templateTimezone = ""
    , eventTemplates = Dict.empty
    }


type alias Model =
    { schedules : List Schedule
    , scheduleTemplates : List ScheduleTemplate
    , key : Nav.Key
    , currentPage : Route
    , employees : List Employee
    , newScheduleTemplate : ScheduleTemplate
    , calendars : List String
    , timezones : List String
    , scheduleTemplateToEdit : Maybe String
    , scheduleTemplateEdits : Dict.Dict String ScheduleTemplate
    , eventTemplateEdits : Dict.Dict String (Dict.Dict String EventTemplate)
    , editTemplatePageModalStatus : ModalDialogStatus
    , eventTemplateOpenStatus : Set.Set String
    , newEventTemplateSummary : String
    }


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( { schedules = []
      , scheduleTemplates = []
      , key = key
      , currentPage = Home
      , employees = []
      , newScheduleTemplate = emptyNewScheduleTemplate
      , calendars = []
      , timezones = []
      , scheduleTemplateToEdit = Nothing
      , scheduleTemplateEdits = Dict.empty
      , eventTemplateEdits = Dict.empty
      , editTemplatePageModalStatus = Closed
      , eventTemplateOpenStatus = Set.empty
      , newEventTemplateSummary = ""
      }
    , Cmd.batch
        [ fetchSchedules
        , fetchEmployees
        , fetchScheduleTemplates
        , fetchCalendars
        , fetchTimezones
        ]
    )
