module Pages.NewSchedulePage exposing (..)

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
import Regex
import Set
import String
import Time
import Types exposing (..)
import Url


newSchedulePage : Model -> Browser.Document Msg
newSchedulePage model =
    let
        templateToOption template =
            option [ selected (template.templateName == newSchedule.templateName), value template.templateName ] [ text template.templateName ]

        monthToNumber month =
            case month of
                Time.Jan ->
                    "01"

                Time.Feb ->
                    "02"

                Time.Mar ->
                    "03"

                Time.Apr ->
                    "04"

                Time.May ->
                    "05"

                Time.Jun ->
                    "06"

                Time.Jul ->
                    "07"

                Time.Aug ->
                    "08"

                Time.Sep ->
                    "09"

                Time.Oct ->
                    "10"

                Time.Nov ->
                    "11"

                Time.Dec ->
                    "12"

        currentDate =
            case ( model.currentTime, model.currentZone ) of
                ( Just time, Just zone ) ->
                    String.fromInt (Time.toYear zone time) ++ "-" ++ monthToNumber (Time.toMonth zone time) ++ "-" ++ String.padLeft 2 '0' (String.fromInt (Time.toDay zone time))

                _ ->
                    ""

        defaultTemplate =
            { templateName = ""
            , startDate = currentDate
            , employees = Set.empty
            }

        newSchedule =
            model.newSchedule

        setTemplateName name =
            { newSchedule | templateName = name }

        setCurrentDate date =
            { newSchedule | startDate = date }

        selectEmployees val =
            let
                newList =
                    Set.union newSchedule.employees <| Maybe.withDefault Set.empty (Maybe.map Set.singleton <| String.toInt val)
            in
            { newSchedule | employees = newList }

        removeSelectionEmployees val =
            let
                newList =
                    Set.filter (\v -> v /= val) newSchedule.employees
            in
            { newSchedule | employees = newList }

        employeeDict =
            Dict.fromList <| List.map (\e -> ( e.employeeId, e )) model.employees

        employeeIdToLi employeeId =
            case Dict.get employeeId employeeDict of
                Just employee ->
                    li []
                        [ text employee.employeeName
                        , button [ type_ "button", class "button tiny secondary", onClick <| EditNewSchedule <| removeSelectionEmployees employeeId ] [ text "x" ]
                        ]

                Nothing ->
                    li [] []

        concatWithDefaults =
            { templateName =
                if newSchedule.templateName == "" then
                    Maybe.withDefault "" (Maybe.map .templateName <| List.head model.scheduleTemplates)

                else
                    newSchedule.templateName
            , startDate =
                if newSchedule.startDate == "" then
                    currentDate

                else
                    newSchedule.startDate
            , employees = newSchedule.employees
            }

        body =
            div [ class "row" ]
                [ Html.form [ onSubmit <| SubmitNewSchedule concatWithDefaults ]
                    [ label []
                        [ text "From template"
                        , select [ name "template", onInput <| EditNewSchedule << setTemplateName ] (List.map templateToOption model.scheduleTemplates)
                        ]
                    , label []
                        [ text "Start date"
                        , input
                            [ type_ "date"
                            , name "start-date"
                            , onInput <| EditNewSchedule << setCurrentDate
                            , value
                                (if newSchedule.startDate /= "" then
                                    newSchedule.startDate

                                 else
                                    currentDate
                                )
                            ]
                            []
                        ]
                    , label []
                        [ text "For Employees"
                        , div []
                            [ ul [ class "no-bullet" ] (List.map employeeIdToLi <| Set.toList newSchedule.employees)
                            , select [ name "employees", onInput <| EditNewSchedule << selectEmployees ] (List.map employeeToOption model.employees)
                            ]
                        ]
                    , button [ class "button", class "success" ] [ text "Submit" ]
                    ]
                ]
    in
    toPage model "Create new schedule" body
