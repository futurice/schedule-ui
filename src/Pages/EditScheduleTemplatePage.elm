module Pages.EditScheduleTemplatePage exposing (..)

import Browser
import Debug
import Dict
import Foundation exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import Maybe exposing (andThen)
import Model exposing (..)
import Msg exposing (..)
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
            [ "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" ]

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

        eventTemplateHtml eventTemplate =
            li [ class "accordion-item", class "is-active", attribute "data-accordion-item" "" ]
                [ a [ class "accordion-title" ] [ text "Testi" ]
                , div [ class "accordion-content", attribute "tab-content" "" ] [ text "Testijee" ]
                ]

        offsetHtml ( offset, templates ) =
            [ h2 [] [ text "Monday" ] ] ++ List.map eventTemplateHtml templates

        body =
            case model.scheduleTemplateToEdit |> andThen Url.percentDecode |> andThen (\s -> Dict.get s scheduleTemplateDict) of
                Nothing ->
                    div [] []

                Just template ->
                    let
                        bmap =
                            Debug.log "templatemap" <| toBucketMap <| List.map Tuple.second <| Dict.toList template.eventTemplates
                    in
                    ul
                        [ class "accordion"
                        , attribute "data-accordion" ""
                        , attribute "multi-expand" ""
                        , attribute "allow-all-closed" ""
                        ]
                    <|
                        List.concat <|
                            List.map offsetHtml <|
                                Dict.toList bmap

        --ul [ class "accordion" ] []
    in
    toPage model "Edit template" body
