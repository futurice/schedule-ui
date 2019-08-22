module Foundation exposing (..)

import Browser
import Html exposing (Attribute, Html, a, div, h1, header, li, text, ul)
import Html.Attributes exposing (class, href)
import List exposing (map)
import Model exposing (..)
import Routing exposing (Nav, Route(..), routeToNav, routeToTitle)


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
