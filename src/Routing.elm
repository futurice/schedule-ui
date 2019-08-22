module Routing exposing (..)

import Html exposing (Attribute, Html, a, div, h1, header, li, text, ul)
import Html.Attributes exposing (class, href)
import Url
import Url.Parser as Parser exposing ((</>))


type Route
    = Home
    | NewSchedule
    | SchedulingRequest
    | PersonalSchedules
    | EditScheduleTemplate String


type alias Nav msg =
    ( Attribute msg, String )


routeToTitle : Route -> String
routeToTitle home =
    case home of
        Home ->
            "Home"

        NewSchedule ->
            "New schedule"

        SchedulingRequest ->
            "Scheduling Requests"

        PersonalSchedules ->
            "Personal Schedules"

        EditScheduleTemplate _ ->
            "Edit schedule template"


routeToNav : Route -> Nav msg
routeToNav route =
    case route of
        Home ->
            ( href "/", routeToTitle route )

        NewSchedule ->
            ( href "new-schedule", routeToTitle route )

        SchedulingRequest ->
            ( href "scheduling-request", routeToTitle route )

        PersonalSchedules ->
            ( href "personal-schedules", routeToTitle route )

        EditScheduleTemplate templateName ->
            ( href <| "edit-schedule-template/" ++ templateName, routeToTitle route )


parser : Parser.Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map NewSchedule (Parser.s "new-schedule")
        , Parser.map SchedulingRequest (Parser.s "scheduling-request")
        , Parser.map PersonalSchedules (Parser.s "personal-schedules")
        , Parser.map EditScheduleTemplate (Parser.s "edit-schedule-template" </> Parser.string)
        ]


urlToRoute : Url.Url -> Maybe Route
urlToRoute =
    Parser.parse parser
