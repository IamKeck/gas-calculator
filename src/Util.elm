module Util exposing (..)

import Html exposing (Html, text)


viewIf : Bool -> Html msg -> Html msg
viewIf cond content =
    if cond then
        content

    else
        text ""
