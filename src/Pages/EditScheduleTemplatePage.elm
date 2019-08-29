module Pages.EditScheduleTemplatePage exposing (..)

import Array
import Browser
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


editScheduleTemplatePage : Model -> Browser.Document Msg
editScheduleTemplatePage model =
    let
        scheduleTemplateDict =
            Dict.fromList <| List.map (\s -> ( s.templateName, s )) model.scheduleTemplates

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

        body =
            case model.scheduleTemplateToEdit |> andThen Url.percentDecode |> andThen (\s -> Dict.get s scheduleTemplateDict) of
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
                            template /= newTemplate

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
                                        [ button [ class "button", disabled <| not anyChanges ] [ text "Save all changes" ]
                                        , button [ class "button", onClick <| EditScheduleTemplateCmd template.templateName template, disabled <| not anyChanges ] [ text "Undo changes" ]
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

                        weekdayText offset =
                            comparableToOffset offset
                                |> andThen
                                    (\o ->
                                        case o of
                                            MonthOffset _ ->
                                                Nothing

                                            DayOffset d ->
                                                Just d
                                    )
                                |> andThen (\d -> Array.get (modBy 7 d) weekdays)
                                |> Maybe.withDefault "Monday"

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
                            div []
                                [ div [ class "row" ]
                                    [ leftLabel "Summary:"
                                    , rightLabel <| input [ type_ "text", value eventTemplate.eventTemplateSummary ] []
                                    ]
                                , div [ class "row" ]
                                    [ leftLabel "Description:"
                                    , rightLabel <| input [ type_ "text", value eventTemplate.eventTemplateDescription ] []
                                    ]
                                , div [ class "row" ]
                                    [ leftLabel "Event Type:"
                                    , rightLabel <|
                                        select []
                                            [ option [ selected eventTemplate.eventTemplateIsCollective ] [ text "Common (invite all employee to same event)" ]
                                            , option [ selected (not eventTemplate.eventTemplateIsCollective) ] [ text "Individual (one separate event for each employee)" ]
                                            ]
                                    ]
                                , div [ class "row" ]
                                    [ leftLabel "Locations:"
                                    , rightLabel <| select [] []
                                    ]
                                , div [ class "row" ]
                                    [ leftLabel "Day offset:"
                                    , rightLabel <| input [ type_ "text", value newTemplate.templateName ] []
                                    ]
                                , div [ class "row" ]
                                    [ leftLabel "Month offset:"
                                    , rightLabel <| input [ type_ "text", value newTemplate.templateName ] []
                                    ]
                                , div [ class "row" ]
                                    [ leftLabel "From:"
                                    , rightLabel <| input [ type_ "text", value newTemplate.templateName ] []
                                    ]
                                , div [ class "row" ]
                                    [ leftLabel "Participants:"
                                    , rightLabel <| input [ type_ "text", value newTemplate.templateName ] []
                                    ]
                                , div [ class "row" ]
                                    [ leftLabel "Other participants:"
                                    , rightLabel <| input [ type_ "text", value newTemplate.templateName ] []
                                    ]
                                , div [ class "row" ]
                                    [ button [ class "button", onClick <| DeleteEventTemplate newTemplate.templateName eventTemplate.eventTemplateId ] [ text "Delete" ]
                                    ]
                                ]

                        bmap =
                            Debug.log "templatemap" <| toBucketMap <| List.map Tuple.second <| Dict.toList template.eventTemplates

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
                    div []
                        [ saveChanges
                        , modifyTemplateField
                        , ul [] <| (List.concat <| List.map offsetHtml <| Dict.toList bmap) ++ List.singleton addEventTemplateHtml
                        ]

        --ul [ class "accordion" ] []
    in
    toPage model "Edit template" body
