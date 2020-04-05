module Editor.Styles exposing (..)

import Html.Attributes as HtmlA


styleSplit =
    [ HtmlA.style "z-index" "1"
    , HtmlA.style "top" "0"
    , HtmlA.style "overflow-x" "hidden"
    , HtmlA.style "padding" "1%"
    ]

styleSplitHoriz =
    styleSplit ++
    [ HtmlA.style "width" "100%"
    , HtmlA.style "height" "100%"
    , HtmlA.style "display" "table"
    , HtmlA.style "border-collapse" "collapse"
    ]

styleSplitTop =
    [ HtmlA.style "display" "table-row"
    , HtmlA.style "height" "48%"
    , HtmlA.style "padding" "1%"
    ]

styleSplitBottom =
    [ HtmlA.style "display" "table-row"
    , HtmlA.style "height" "48%"
    , HtmlA.style "border-top" "2px solid"
    , HtmlA.style "padding" "1%"
    ]

styleSplitLeft =
    styleSplit ++
    [ HtmlA.style "left" "0"
    , HtmlA.style "border-right" "solid"
    , HtmlA.style "position" "fixed"
    , HtmlA.style "height" "100%"
    , HtmlA.style "width" "38%"
    ]

styleSplitRight =
    styleSplit ++
    [ HtmlA.style "right" "0"
    , HtmlA.style "position" "fixed"
    , HtmlA.style "height" "100%"
    , HtmlA.style "width" "58%"
    ]