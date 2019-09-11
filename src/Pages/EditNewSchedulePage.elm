module Pages.EditNewSchedulePage exposing (..)

import Browser
import Model exposing (..)
import Msg exposing (..)
import Pages.EditScheduleTemplatePage exposing (..)


editNewSchedulePage : Model -> Browser.Document Msg
editNewSchedulePage model =
    editScheduleTemplatePage model False
