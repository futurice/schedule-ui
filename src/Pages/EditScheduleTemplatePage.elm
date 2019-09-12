module Pages.EditScheduleTemplatePage exposing (..)

import Array
import Browser
import Date
import Debug
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Maybe exposing (andThen)
import Model exposing (..)
import Msg exposing (..)
import Pages.Util exposing (..)
import Regex
import Set
import String
import Types exposing (..)
import Url


offsetToComparable : Offset -> String
offsetToComparable offset =
    case offset of
        DayOffset dayOffset ->
            "day-" ++ String.fromInt dayOffset

        MonthOffset monthOffset ->
            "month-" ++ String.fromInt monthOffset


comparableToOffset : String -> Maybe Offset
comparableToOffset comp =
    case String.split "-" comp of
        [ "day", dayOffset ] ->
            String.toInt dayOffset |> andThen (\offset -> Just <| DayOffset offset)

        [ "month", monthOffset ] ->
            String.toInt monthOffset |> andThen (\offset -> Just <| MonthOffset offset)

        _ ->
            Nothing


scheduleTemplateDict : Model -> Dict.Dict String ScheduleTemplate
scheduleTemplateDict model =
    Dict.fromList <| List.map (\s -> ( s.templateName, s )) model.scheduleTemplates


templateToEdit : Model -> Bool -> Maybe ScheduleTemplate
templateToEdit model editTemplate =
    if editTemplate then
        model.scheduleTemplateToEdit |> andThen Url.percentDecode |> andThen (\s -> Dict.get s <| scheduleTemplateDict model)

    else
        Dict.get model.newSchedule.templateName <| scheduleTemplateDict model


addOffsetToDate : String -> Offset -> String
addOffsetToDate dateString offset =
    let
        date =
            Date.fromIsoString dateString
    in
    case ( date, offset ) of
        ( Ok d, DayOffset dayOffset ) ->
            Date.toIsoString <| Date.add Date.Days dayOffset d

        ( Ok d, MonthOffset monthOffset ) ->
            Date.toIsoString <| Date.add Date.Months monthOffset d

        _ ->
            dateString


editScheduleTemplatePage : Model -> Bool -> Browser.Document Msg
editScheduleTemplatePage model editTemplate =
    let
        weekdays =
            Array.fromList [ "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" ]

        toMapItem : EventTemplate -> Dict.Dict String (List EventTemplate)
        toMapItem eventTemplate =
            Dict.fromList <| List.singleton ( offsetToComparable eventTemplate.eventTemplateOffset, List.singleton eventTemplate )

        toBucketMap : List EventTemplate -> Dict.Dict String (List EventTemplate)
        toBucketMap eventTemplates =
            let
                mergeBuckets a b =
                    Dict.merge Dict.insert (\key x y res -> Dict.insert key (x ++ y) res) Dict.insert a b Dict.empty

                mapItems =
                    List.map toMapItem eventTemplates
            in
            List.foldl mergeBuckets Dict.empty mapItems

        toOption comparator x =
            option [ selected (comparator == x), value x ] [ text x ]

        emptyOption =
            List.singleton <| option [ selected False, value "" ] [ text "" ]

        employeeDict =
            Dict.fromList <| List.map (\e -> ( e.employeeId, e )) model.employees

        body =
            case templateToEdit model editTemplate of
                Nothing ->
                    div [] []

                Just template ->
                    let
                        newTemplate =
                            case Dict.get template.templateName model.scheduleTemplateEdits of
                                Just t ->
                                    t

                                Nothing ->
                                    template

                        anyChanges =
                            let
                                hasEventTemplateChanges eventTemplate =
                                    Maybe.withDefault False (Dict.get template.templateName model.eventTemplateEdits |> andThen (Dict.get eventTemplate.eventTemplateId) |> andThen (\old -> Just <| old /= eventTemplate))

                                anyEventTemplateChanges =
                                    List.any hasEventTemplateChanges <| List.map Tuple.second <| Dict.toList template.eventTemplates
                            in
                            template /= newTemplate || anyEventTemplateChanges

                        setName name =
                            { newTemplate | templateName = name }

                        setCalendar calendar =
                            { newTemplate | templateCalendar = calendar }

                        setTimezone tz =
                            { newTemplate | templateTimezone = tz }

                        saveChanges =
                            let
                                changeStatusText =
                                    if anyChanges then
                                        text "There are unsaved changes"

                                    else
                                        text "You haven't made any changes"

                                changeStatusButtons =
                                    div []
                                        [ button [ class "button", onClick <| SaveEventTemplates template.templateName, disabled <| not anyChanges ] [ text "Save all changes" ]
                                        , button [ class "button", onClick <| ResetEditedScheduleTemplate template.templateName, disabled <| not anyChanges ] [ text "Undo changes" ]
                                        ]
                            in
                            div []
                                [ changeStatusText
                                , changeStatusButtons
                                ]

                        leftLabel t =
                            div [ class "small-1 columns" ]
                                [ label [ attribute "for" "right-label", class "text-left" ] [ text t ] ]

                        rightLabel i =
                            div [ class "small-5 columns", class "float-left" ] [ i ]

                        rightLabelWithSize size i =
                            div [ class <| "small-" ++ String.fromInt size ++ " columns", class "float-left" ] [ i ]

                        weekdayText offset =
                            comparableToOffset offset
                                |> andThen
                                    (\o ->
                                        case o of
                                            MonthOffset m ->
                                                if m == 1 then
                                                    Just "+1 month"

                                                else
                                                    Just <| "+" ++ String.fromInt m ++ " months"

                                            DayOffset d ->
                                                Just << Maybe.withDefault "Monday" <| Array.get (modBy 7 d) weekdays
                                    )
                                |> Maybe.withDefault ""

                        offsetHtml ( offset, templates ) =
                            [ h2 [] [ text <| weekdayText offset ] ] ++ List.map eventTemplateHtml templates

                        addEventTemplateHtml =
                            div [ class "row" ]
                                [ leftLabel "Add an event template:"
                                , rightLabel <|
                                    div []
                                        [ input [ type_ "text", value model.newEventTemplateSummary, onInput <| EditNewEventTemplate ] []
                                        , button [ class "button", onClick <| AddEventTemplate template.templateName model.newEventTemplateSummary ] [ text "Add" ]
                                        ]
                                ]

                        submitNewScheduleHtml =
                            div [ class "row" ]
                                [ button [ class "button" ] [ text "Create events in Google calendar" ]
                                , text " "
                                , button [ class "button", onClick CancelNewSchedule ] [ text "Cancel" ]
                                ]

                        eventTemplateHtml eventTemplate =
                            li []
                                [ if Set.member eventTemplate.eventTemplateId model.eventTemplateOpenStatus then
                                    eventTemplateOpenDiv eventTemplate

                                  else
                                    eventTemplateClosedDiv eventTemplate
                                ]

                        eventTemplateClosedDiv eventTemplate =
                            div [ onClick <| ToggleEventTemplateOpenStatus eventTemplate.eventTemplateId ]
                                [ div [] [ text eventTemplate.eventTemplateSummary ]
                                , div [] [ text "+0 days" ]
                                , div [] [ text "Invite: employees" ]
                                ]

                        eventTemplateOpenDiv eventTemplate =
                            let
                                getEventTemplateInfo =
                                    if editTemplate then
                                        Dict.get template.templateName model.eventTemplateEdits |> andThen (Dict.get eventTemplate.eventTemplateId)

                                    else
                                        Dict.get eventTemplate.eventTemplateId model.newScheduleEventTemplates

                                newEventTemplate =
                                    case getEventTemplateInfo of
                                        Nothing ->
                                            eventTemplate

                                        Just t ->
                                            t

                                setEventTemplateSummary summary =
                                    { newEventTemplate | eventTemplateSummary = summary }

                                setEventTemplateDescription description =
                                    { newEventTemplate | eventTemplateDescription = description }

                                setEventTemplateOffsetType t =
                                    let
                                        offsetVal =
                                            unwrapOffset newEventTemplate.eventTemplateOffset
                                    in
                                    case t of
                                        "day" ->
                                            { newEventTemplate | eventTemplateOffset = DayOffset offsetVal }

                                        _ ->
                                            { newEventTemplate | eventTemplateOffset = MonthOffset offsetVal }

                                setEventTemplateOffsetValue val =
                                    case String.toInt val of
                                        Just v ->
                                            { newEventTemplate | eventTemplateOffset = mapOffset (\_ -> v) newEventTemplate.eventTemplateOffset }

                                        Nothing ->
                                            newEventTemplate

                                setEventTemplateStartTime startTime =
                                    { newEventTemplate | eventTemplateStartTime = startTime }

                                setEventTemplateEndTime endTime =
                                    { newEventTemplate | eventTemplateEndTime = endTime }

                                setEventTemplateInviteSupervisors val =
                                    { newEventTemplate | eventTemplateInviteSupervisors = val }

                                setEventTemplateIsCollective val =
                                    { newEventTemplate | eventTemplateIsCollective = val }

                                selectOtherParticipants val =
                                    let
                                        newList =
                                            Set.union newEventTemplate.eventTemplateOtherParticipants <| Maybe.withDefault Set.empty (Maybe.map Set.singleton <| String.toInt val)
                                    in
                                    { newEventTemplate | eventTemplateOtherParticipants = newList }

                                removeSelectionOtherParticipants val =
                                    let
                                        newList =
                                            Set.filter (\v -> v /= val) newEventTemplate.eventTemplateOtherParticipants
                                    in
                                    { newEventTemplate | eventTemplateOtherParticipants = newList }

                                borderColor noErrors =
                                    if noErrors then
                                        style "" ""

                                    else
                                        style "border-color" "red"

                                getEditMsg =
                                    if editTemplate then
                                        EditEventTemplate template.templateName

                                    else
                                        EditNewScheduleEventTemplate

                                employeeIdToLi employeeId =
                                    case Dict.get employeeId employeeDict of
                                        Just employee ->
                                            li []
                                                [ text employee.employeeName
                                                , button [ class "button tiny secondary", onClick <| getEditMsg <| removeSelectionOtherParticipants employeeId ] [ text "x" ]
                                                ]

                                        Nothing ->
                                            li [] []

                                timeBorderValue val =
                                    borderColor (Regex.contains (Maybe.withDefault Regex.never <| Regex.fromString "^([0-2]?\\d:[0-5]\\d)?$") val)
                            in
                            div []
                                [ div [ onClick <| ToggleEventTemplateOpenStatus eventTemplate.eventTemplateId ] [ text "Close" ]
                                , div [ class "row" ]
                                    [ leftLabel "Summary:"
                                    , rightLabel <|
                                        input
                                            [ type_ "text"
                                            , value newEventTemplate.eventTemplateSummary
                                            , onInput (getEditMsg << setEventTemplateSummary)
                                            ]
                                            []
                                    ]
                                , div [ class "row" ]
                                    [ leftLabel "Description:"
                                    , rightLabel <|
                                        input
                                            [ type_ "text"
                                            , value newEventTemplate.eventTemplateDescription
                                            , onInput (getEditMsg << setEventTemplateDescription)
                                            ]
                                            []
                                    ]
                                , div [ class "row" ]
                                    [ leftLabel "Event Type:"
                                    , rightLabel <|
                                        select []
                                            [ option [ selected newEventTemplate.eventTemplateIsCollective ] [ text "Common (invite all employee to same event)" ]
                                            , option [ selected (not newEventTemplate.eventTemplateIsCollective) ] [ text "Individual (one separate event for each employee)" ]
                                            ]
                                    ]
                                , div [ class "row" ]
                                    [ leftLabel "Locations:"
                                    , rightLabel <| select [] []
                                    ]
                                , if editTemplate then
                                    div [ class "row" ]
                                        [ leftLabel "Offset:"
                                        , div [ class "small-2 columns" ]
                                            [ select [ onInput <| (getEditMsg << setEventTemplateOffsetType) ]
                                                [ option [ selected <| isDayOffset newEventTemplate.eventTemplateOffset, value "day" ] [ text "Day offset" ]
                                                , option [ selected <| isMonthOffset newEventTemplate.eventTemplateOffset, value "month" ] [ text "Month offset" ]
                                                ]
                                            ]
                                        , rightLabelWithSize 1 <|
                                            input
                                                [ type_ "number"
                                                , onInput (getEditMsg << setEventTemplateOffsetValue)
                                                , value <| String.fromInt <| unwrapOffset newEventTemplate.eventTemplateOffset
                                                ]
                                                []
                                        ]

                                  else
                                    div [ class "row" ]
                                        [ leftLabel "Date:"
                                        , rightLabelWithSize 2 <| input [ type_ "date", value <| addOffsetToDate model.newSchedule.startDate newEventTemplate.eventTemplateOffset ] []
                                        ]
                                , div [ class "row" ]
                                    [ leftLabel "From:"
                                    , rightLabelWithSize 2 <|
                                        input
                                            [ type_ "text"
                                            , onInput (getEditMsg << setEventTemplateStartTime)
                                            , timeBorderValue newEventTemplate.eventTemplateStartTime
                                            ]
                                            []
                                    , rightLabelWithSize 1 <| span [] [ text " to " ]
                                    , rightLabelWithSize 2 <|
                                        input
                                            [ type_ "text"
                                            , onInput (getEditMsg << setEventTemplateEndTime)
                                            , timeBorderValue newEventTemplate.eventTemplateEndTime
                                            ]
                                            []
                                    ]
                                , div [ class "row" ]
                                    [ leftLabel "Participants:"
                                    , rightLabel <|
                                        div []
                                            [ input [ type_ "checkbox" ] []
                                            , label [] [ text "Invite employees" ]
                                            , input
                                                [ type_ "checkbox"
                                                , onCheck (getEditMsg << setEventTemplateInviteSupervisors)
                                                , checked newEventTemplate.eventTemplateInviteSupervisors
                                                ]
                                                []
                                            , label [] [ text "Invite supervisors" ]
                                            , input
                                                [ type_ "checkbox"

                                                --                                                , onCheck (EditEventTemplate template.templateName << setEventTemplateInviteFutubuddies)
                                                ]
                                                []
                                            , label [] [ text "Invite futubuddies" ]
                                            ]
                                    ]
                                , div [ class "row" ]
                                    [ leftLabel "Other participants:"
                                    , rightLabel <|
                                        div []
                                            [ ul [ class "no-bullet" ] (List.map employeeIdToLi <| Set.toList newEventTemplate.eventTemplateOtherParticipants)
                                            , select [ name "employees", onInput (getEditMsg << selectOtherParticipants) ] (List.map employeeToOption model.employees)
                                            ]
                                    ]
                                , div [ class "row" ]
                                    [ button [ class "button", onClick <| DeleteEventTemplate newTemplate.templateName eventTemplate.eventTemplateId ] [ text "Delete" ]
                                    ]
                                ]

                        bmap =
                            toBucketMap <| List.map Tuple.second <| Dict.toList template.eventTemplates

                        modifyTemplateField =
                            div []
                                [ Html.form []
                                    [ div [ class "row" ]
                                        [ div [ class "small-1 columns" ]
                                            [ label [ attribute "for" "right-label", class "text-left" ] [ text "Name:" ]
                                            ]
                                        , div [ class "small-5 columns", class "float-left" ]
                                            [ input [ type_ "text", id "right-label", onInput (EditScheduleTemplateCmd template.templateName << setName), value newTemplate.templateName ] [] ]
                                        ]
                                    , div [ class "row" ]
                                        [ div [ class "small-1 columns" ]
                                            [ label [ attribute "for" "right-label", class "text-left" ] [ text "Timezone:" ] ]
                                        , div [ class "small-5 columns", class "float-left" ]
                                            [ select [ name "calendar", onInput (EditScheduleTemplateCmd template.templateName << setTimezone) ] <| emptyOption ++ List.map (toOption newTemplate.templateTimezone) model.timezones ]
                                        ]
                                    , div [ class "row" ]
                                        [ div [ class "small-1 columns" ]
                                            [ label [ attribute "for" "right-label", class "text-left" ] [ text "Calendar" ] ]
                                        , div [ class "small-5 columns", class "float-left" ]
                                            [ select [ name "calendar", onInput (EditScheduleTemplateCmd template.templateName << setCalendar) ] <| emptyOption ++ List.map (toOption newTemplate.templateCalendar) model.calendars ]
                                        ]
                                    ]
                                ]
                    in
                    if editTemplate then
                        div []
                            [ saveChanges
                            , modifyTemplateField
                            , ul [ class "no-bullet" ] <| (List.concat <| List.map offsetHtml <| Dict.toList bmap) ++ List.singleton addEventTemplateHtml
                            ]

                    else
                        div []
                            [ ul [ class "no-bullet" ] <|
                                (List.map eventTemplateHtml <|
                                    List.sortBy (\e -> addOffsetToDate model.newSchedule.startDate e.eventTemplateOffset) <|
                                        List.map Tuple.second <|
                                            Dict.toList template.eventTemplates
                                )
                                    ++ List.singleton submitNewScheduleHtml
                            ]

        --ul [ class "accordion" ] []
    in
    toPage model "Edit template" body
