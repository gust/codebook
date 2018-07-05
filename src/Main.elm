import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onInput)
import Html.Attributes exposing (class, placeholder)
import String exposing (contains)

type Msg = Search String

type alias Code =
  { code : String
  , modifiers : List String
  }

update msg model =
  case msg of
    Search search ->
        search

view model =
  div []
    [ div [ class "search" ]
       [ input 
         [ placeholder "search"
         , onInput Search ] [] 
       ]
    , div [ class "codes" ] <| List.map (renderCode model) codes
    ]

renderCode : String -> Code -> Html Msg
renderCode search c =
  let 
    klass = if (contains search c.code) then "code hl" else "code"
  in
    div [ class klass ]
      [ text c.code
      , div [ class "modifiers"] <| List.map (renderMod search) c.modifiers
      ]

renderMod : String -> String -> Html Msg
renderMod search m =
  let
    klass = if (contains search m) then "modifier hl" else "modifier"
  in
    div [ class klass ]
      [ text ("+ " ++ m)
      ]

codes : List Code
codes =
  [ { code = "alpha", modifiers = [ "what" ] }
  , { code = "beta", modifiers = [ "meta" ] }
  , { code = "chloe", modifiers = [] }
  , { code = "duck", modifiers = [] }
  ]

main =
  Html.beginnerProgram { model = "", view = view, update = update }
