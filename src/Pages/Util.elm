module Pages.Util exposing (..)

import Browser
import Html exposing (Attribute, Html, a, button, div, h1, header, li, option, text, ul)
import Html.Attributes exposing (attribute, class, href, selected, style, value)
import Html.Events exposing (onClick)
import List exposing (map)
import Model exposing (..)
import Msg exposing (..)
import Routing exposing (Nav, Route(..), routeToNav, routeToTitle)
import Types exposing (..)


fullRow : List (Attribute msg) -> List (Html msg) -> Html msg
fullRow attr body =
    div [ class "row" ] [ div [ class "columns large-12" ] body ]


navigation : Route -> List Route -> Html msg
navigation currentPage routes =
    let
        toLi route =
            let
                liAttrs =
                    if route == currentPage then
                        [ class "futu-active" ]

                    else
                        []

                ( aAttr, t ) =
                    routeToNav route
            in
            li liAttrs [ a [ aAttr ] [ text t ] ]
    in
    div [ class "top-bar" ]
        [ fullRow []
            [ div [ class "top-bar-left" ]
                [ ul [ class "menu horizontal" ] (map toLi routes)
                ]
            ]
        ]


page : Route -> Html msg
page route =
    let
        navs =
            [ Home
            , NewSchedule
            , SchedulingRequest
            , PersonalSchedules
            ]
    in
    div []
        [ navigation route navs
        , fullRow []
            [ header [] [ h1 [] [ text (routeToTitle route) ] ]
            ]
        ]


toPage : Model -> String -> Html msg -> Browser.Document msg
toPage model title body =
    let
        rest =
            div []
                [ page model.currentPage
                , div [ class "row" ] [ body ]
                ]
    in
    { title = title, body = [ rest ] }


modalDialog : ModalDialogStatus -> String -> Msg -> Msg -> Html Msg
modalDialog status question confirmaction closeaction =
    let
        visibility =
            case status of
                Opened ->
                    "block"

                Closed ->
                    "none"
    in
    div
        [ attribute "display" visibility
        , style "position" "fixed"
        ]
        [ text question
        , div []
            [ button [ class "button", onClick closeaction ] [ text "Cancel" ]
            , button [ class "button", onClick confirmaction ] [ text "Ok" ]
            ]
        ]


employeeToOption : Employee -> Html msg
employeeToOption employee =
    option [ selected False, value <| String.fromInt employee.employeeId ] [ text employee.employeeName ]
