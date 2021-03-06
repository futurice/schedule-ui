module Main exposing (..)

import Api exposing (..)
import Browser
import Browser.Navigation as Nav
import Debug
import Dict
import Foundation exposing (..)
import Html exposing (Html, a, button, div, form, h1, img, input, label, option, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href, multiple, name, selected, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import List exposing (map)
import Model exposing (..)
import Msg exposing (..)
import Pages.EditScheduleTemplatePage exposing (..)
import Routing exposing (..)
import Set
import String
import Time
import Types exposing (..)
import Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ChangedUrl url ->
            case urlToRoute url of
                Just route ->
                    case route of
                        EditScheduleTemplate templateName ->
                            ( { model | currentPage = route, scheduleTemplateToEdit = Just templateName }, Cmd.none )

                        _ ->
                            ( { model | currentPage = route }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        FetchedEmployees result ->
            case result of
                Ok employees ->
                    ( { model | employees = employees }, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        FetchedSchedules result ->
            case result of
                Ok schedules ->
                    ( { model | schedules = schedules }, Cmd.none )

                Err err ->
                    Debug.log "Got error" ( model, Cmd.none )

        FetchedCalendars result ->
            case result of
                Ok calendars ->
                    ( { model | calendars = calendars }, Cmd.none )

                Err err ->
                    Debug.log "Got error" ( model, Cmd.none )

        FetchedTimezones result ->
            case result of
                Ok timezones ->
                    ( { model | timezones = timezones }, Cmd.none )

                Err err ->
                    Debug.log "Got error" ( model, Cmd.none )

        FetchedScheduleTemplates result ->
            case result of
                Ok scheduleTemplates ->
                    ( { model | scheduleTemplates = scheduleTemplates }, Cmd.none )

                Err err ->
                    let
                        joku =
                            Debug.log "Got template error" err
                    in
                    ( model, Cmd.none )

        SubmitNewScheduleTemplate ->
            ( model, Cmd.batch [ postNewScheduleTemplate model.newScheduleTemplate ] )

        NewScheduleTemplateSubmittedResult result ->
            --TODO: better error handling
            case result of
                Ok _ ->
                    ( { model | newScheduleTemplate = emptyNewScheduleTemplate }, Cmd.batch [ fetchScheduleTemplates ] )

                Err err ->
                    ( model, Cmd.none )

        EditNewScheduleTemplate template ->
            let
                joku =
                    Debug.log "uusi value" template
            in
            ( { model | newScheduleTemplate = template }, Cmd.none )

        DeletedScheduleTemplate result ->
            case result of
                Ok _ ->
                    ( model, Cmd.batch [ fetchScheduleTemplates ] )

                Err err ->
                    ( model, Cmd.none )

        DeleteTemplate templateName ->
            ( model, Cmd.batch [ deleteScheduleTemplate templateName ] )


homePage : Model -> Browser.Document Msg
homePage model =
    let
        newTemplate =
            model.newScheduleTemplate

        setName name =
            { newTemplate | templateName = name }

        setCalendar calendar =
            { newTemplate | templateCalendar = calendar }

        setTimezone tz =
            { newTemplate | templateTimezone = tz }

        toOption x =
            option [ selected False, value x ] [ text x ]

        emptyOption =
            List.singleton <| option [ selected False, value "" ] [ text "" ]

        newScheduleRow =
            List.singleton <|
                tr []
                    [ td [] [ input [ type_ "text", onInput (\s -> EditNewScheduleTemplate <| setName s) ] [] ]
                    , td [] [ select [ name "timezone", onInput (\s -> EditNewScheduleTemplate <| setTimezone s) ] <| emptyOption ++ map toOption model.timezones ]
                    , td [] [ select [ name "calendar", onInput (\s -> EditNewScheduleTemplate <| setCalendar s) ] <| emptyOption ++ map toOption model.calendars ]
                    , td [] [ button [ class "success", class "button" ] [ text "Submit" ] ]
                    ]

        body =
            div [ class "row" ]
                [ form [ onSubmit SubmitNewScheduleTemplate ]
                    [ table []
                        [ thead []
                            [ th [] [ text "Template" ]
                            , th [] [ text "Timezone" ]
                            , th [] [ text "Calendar" ]
                            , th [] [ text "Actions" ]
                            ]
                        , let
                            singleRow s =
                                tr []
                                    [ td [] [ a [ href <| "edit-schedule-template/" ++ s.templateName ] [ text s.templateName ] ]
                                    , td [] [ text s.templateTimezone ]
                                    , td [] [ text s.templateCalendar ]
                                    , td []
                                        [ button [ class "button" ] [ text "Edit" ]
                                        , button [ class "button" ] [ text "Copy" ]
                                        , button [ class "button", onClick (DeleteTemplate s.templateName) ] [ text "Delete" ]
                                        ]
                                    ]
                          in
                          tbody [] (map singleRow model.scheduleTemplates ++ newScheduleRow)
                        ]
                    ]
                ]
    in
    toPage model "Schedule Home" body


newSchedulePage : Model -> Browser.Document Msg
newSchedulePage model =
    let
        templateToOption template =
            option [ selected False, value template.templateName ] [ text template.templateName ]

        employeeToOption employee =
            option [ selected False, value <| String.fromInt employee.employeeId ] [ text employee.employeeName ]

        body =
            div [ class "row" ]
                [ form []
                    [ label []
                        [ text "From template"
                        , select [ name "template" ] (map templateToOption model.scheduleTemplates)
                        ]
                    , label []
                        [ text "Start date"
                        , input [ type_ "date", name "start-date" ] []
                        ]
                    , label []
                        [ text "For Employees"
                        , select [ name "employees", multiple True ] (map employeeToOption model.employees)
                        ]
                    , button [ class "button", class "success" ] [ text "Submit" ]
                    ]
                ]
    in
    toPage model "Create new schedule" body


schedulingRequestPage : Model -> Browser.Document Msg
schedulingRequestPage model =
    let
        scheduleEmployeesToText employees =
            case employees of
                [] ->
                    ""

                [ e ] ->
                    e.employeeName

                [ e, e2 ] ->
                    e.employeeName ++ " and " ++ e2.employeeName

                e :: es ->
                    e.employeeName ++ ", " ++ scheduleEmployeesToText es

        scheduleToRow schedule =
            tr []
                [ td [] [ text <| scheduleEmployeesToText <| scheduleEmployees schedule ]
                , td [] [ text schedule.templateName ]
                , td [] [ text schedule.createdBy ]
                , td [] [ text schedule.createdOn ]
                , td [] [ button [ class "success", class "button" ] [ text "Add user" ] ]
                , td [] [ button [ class "alert", class "button" ] [ text "Remove users" ] ]
                , td [] [ button [ class "button" ] [ text "Download" ] ]
                , td [] [ scheduleStatusToHtml schedule.status ]
                , td [] [ button [ class "success", class "button" ] [ text "Delete" ] ]

                --scheduleEmployees
                ]

        body =
            div []
                [ table []
                    [ thead []
                        [ th [] [ text "For Group" ]
                        , th [] [ text "Template" ]
                        , th [] [ text "By" ]
                        , th [] [ text "Created" ]
                        , th [ style "width" "18%" ] [ text "Add Users" ]
                        , th [ style "width" "18%" ] [ text "Remove Users" ]
                        , th [] [ text "Generate pdf" ]
                        , th [] [ text "Status" ]
                        , th [] [ text "Delete" ]
                        ]
                    , tbody [] (map scheduleToRow model.schedules)
                    ]
                ]
    in
    toPage model "Scheduling Requests" body


type alias PersonSchedules =
    { personName : String
    , templateName : String
    , createdOn : String
    }


personalSchedulesPage : Model -> Browser.Document Msg
personalSchedulesPage model =
    let
        personSchedules =
            []

        scheduleToRow personSchedule =
            tr []
                [ td [] [ text personSchedule.personName ]
                , td [] [ text personSchedule.templateName ]
                , td [] [ text personSchedule.createdOn ]
                ]

        body =
            div []
                [ table []
                    [ thead []
                        [ th [] [ text "For" ]
                        , th [] [ text "Template" ]
                        , th [] [ text "Created" ]
                        ]
                    , tbody [] (map scheduleToRow <| personSchedules)
                    ]
                ]
    in
    toPage model "Personal Schedules" body


view : Model -> Browser.Document Msg
view model =
    case model.currentPage of
        Home ->
            homePage model

        NewSchedule ->
            newSchedulePage model

        SchedulingRequest ->
            schedulingRequestPage model

        PersonalSchedules ->
            personalSchedulesPage model

        EditScheduleTemplate _ ->
            editScheduleTemplatePage model



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = \_ _ key -> init key
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
