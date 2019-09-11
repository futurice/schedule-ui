module Main exposing (..)

import Api exposing (..)
import Browser
import Browser.Navigation as Nav
import Debug
import Dict
import Html exposing (Html, a, button, div, form, h1, img, input, label, option, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href, multiple, name, selected, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import List exposing (map)
import Model exposing (..)
import Msg exposing (..)
import Pages.EditNewSchedulePage exposing (..)
import Pages.EditScheduleTemplatePage exposing (..)
import Pages.HomePage exposing (..)
import Pages.NewSchedulePage exposing (..)
import Pages.Util exposing (..)
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
                    ( { model | employees = List.filter (\e -> String.trim e.employeeName /= "") <| List.sortBy .employeeName employees }, Cmd.none )

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
            ( { model | newScheduleTemplate = template }, Cmd.none )

        EditEventTemplate scheduleTemplateName eventTemplate ->
            let
                newDict =
                    case Dict.get scheduleTemplateName model.eventTemplateEdits of
                        Nothing ->
                            Dict.insert scheduleTemplateName (Dict.singleton eventTemplate.eventTemplateId eventTemplate) model.eventTemplateEdits

                        Just d ->
                            Dict.insert scheduleTemplateName (Dict.insert eventTemplate.eventTemplateId eventTemplate d) model.eventTemplateEdits
            in
            ( { model | eventTemplateEdits = newDict }, Cmd.none )

        DeletedScheduleTemplate result ->
            case result of
                Ok _ ->
                    ( model, Cmd.batch [ fetchScheduleTemplates ] )

                Err err ->
                    ( model, Cmd.none )

        DeleteTemplate templateName ->
            ( model, Cmd.batch [ deleteScheduleTemplate templateName ] )

        EditScheduleTemplateCmd oldTemplateName template ->
            let
                newDict =
                    Dict.insert oldTemplateName template model.scheduleTemplateEdits
            in
            ( { model | scheduleTemplateEdits = newDict }, Cmd.none )

        ResetEditedScheduleTemplate oldTemplateName ->
            let
                newScheduleTemplateDict =
                    Dict.remove oldTemplateName model.scheduleTemplateEdits

                newEventTemplateEditsDict =
                    Dict.remove oldTemplateName model.eventTemplateEdits
            in
            ( { model | scheduleTemplateEdits = newScheduleTemplateDict, eventTemplateEdits = newEventTemplateEditsDict }, Cmd.none )

        SubmitEditedScheduleTemplate templateName ->
            ( model, Cmd.none )

        DeleteEventTemplate scheduleTemplateId eventTemplateId ->
            ( model, Cmd.batch [ deleteEventTemplate scheduleTemplateId eventTemplateId ] )

        DeletedEventTemplate result ->
            case result of
                Ok _ ->
                    ( model, Cmd.batch [ fetchScheduleTemplates ] )

                Err err ->
                    ( model, Cmd.none )

        AddEventTemplate scheduleTemplateName eventTemplateSummary ->
            ( model, Cmd.batch [ addEventTemplate scheduleTemplateName eventTemplateSummary ] )

        EditNewEventTemplate eventTemplateSummary ->
            ( { model | newEventTemplateSummary = eventTemplateSummary }, Cmd.none )

        NewEventTemplateSubmitted result ->
            case result of
                Ok _ ->
                    ( { model | newEventTemplateSummary = "" }, Cmd.batch [ fetchScheduleTemplates ] )

                Err err ->
                    ( model, Cmd.none )

        ToggleEventTemplateOpenStatus eventTemplateId ->
            if Set.member eventTemplateId model.eventTemplateOpenStatus then
                ( { model | eventTemplateOpenStatus = Set.remove eventTemplateId model.eventTemplateOpenStatus }, Cmd.none )

            else
                ( { model | eventTemplateOpenStatus = Set.insert eventTemplateId model.eventTemplateOpenStatus }, Cmd.none )

        SaveEventTemplates scheduleTemplateName ->
            let
                eventTemplates =
                    Maybe.withDefault [] (Dict.get scheduleTemplateName model.eventTemplateEdits |> Maybe.andThen (Just << List.map Tuple.second << Dict.toList))
            in
            ( model, Cmd.batch (List.map (editEventTemplate scheduleTemplateName) eventTemplates) )

        EditEventTemplateSubmitted scheduleTemplateName eventTemplateId result ->
            case result of
                Ok _ ->
                    let
                        newDict =
                            Dict.update scheduleTemplateName (Maybe.map <| Dict.remove eventTemplateId) model.eventTemplateEdits

                        eventTemplatesNotProcessedYet =
                            Maybe.withDefault 0 (Maybe.map (List.length << Dict.toList) <| Dict.get scheduleTemplateName newDict)
                    in
                    ( { model | eventTemplateEdits = newDict }
                    , if eventTemplatesNotProcessedYet > 0 then
                        Cmd.none

                      else
                        Cmd.batch [ fetchScheduleTemplates ]
                    )

                Err err ->
                    ( model, Cmd.none )

        SetCurrentTime time ->
            ( { model | currentTime = Just time }, Cmd.none )

        SetCurrentZone zone ->
            ( { model | currentZone = Just zone }, Cmd.none )

        EditNewSchedule newSchedule ->
            ( { model | newSchedule = newSchedule }, Cmd.none )

        SubmitNewSchedule newSchedule ->
            ( { model | newSchedule = newSchedule, currentPage = EditNewSchedulePage, newScheduleEventTemplates = Dict.empty, eventTemplateOpenStatus = Set.empty }, Cmd.none )

        EditNewScheduleEventTemplate eventTemplate ->
            let
                newDict =
                    Dict.insert eventTemplate.eventTemplateId eventTemplate model.newScheduleEventTemplates
            in
            ( { model | newScheduleEventTemplates = newDict }, Cmd.none )

        CancelNewSchedule ->
            ( { model | currentPage = Home }, Cmd.none )


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

        EditNewSchedulePage ->
            editNewSchedulePage model

        SchedulingRequest ->
            schedulingRequestPage model

        PersonalSchedules ->
            personalSchedulesPage model

        EditScheduleTemplate _ ->
            editScheduleTemplatePage model True



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
