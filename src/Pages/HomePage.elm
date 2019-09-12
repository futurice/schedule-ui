module Pages.HomePage exposing (..)

import Browser
import Html exposing (Html, a, button, div, form, h1, img, input, label, option, select, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href, multiple, name, selected, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import List
import Model exposing (..)
import Msg exposing (..)
import Pages.Util exposing (..)


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
                    [ td [] [ input [ type_ "text", onInput (EditNewScheduleTemplate << setName) ] [] ]
                    , td [] [ select [ name "timezone", onInput (EditNewScheduleTemplate << setTimezone) ] <| emptyOption ++ List.map toOption model.timezones ]
                    , td [] [ select [ name "calendar", onInput (EditNewScheduleTemplate << setCalendar) ] <| emptyOption ++ List.map toOption model.calendars ]
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
                          tbody [] (List.map singleRow model.scheduleTemplates ++ newScheduleRow)
                        ]
                    ]
                ]
    in
    toPage model "Schedule Home" body
