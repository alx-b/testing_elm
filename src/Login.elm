module Login exposing (Model, Msg, update, view, init)

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Html exposing (Html)
import Html.Attributes
import Html.Events


type alias Model =
  { name : String
  , password : String
  }

init : Model
init =
  Model "" ""

type Msg
    = Name String
    | Password String


update : Msg -> Model -> Model
update msg form =
  case msg of
    Name name ->
      { form | name = name }

    Password password ->
      { form | password = password }

    
view : Model -> Html Msg
view form =
    Html.div []
        [ Html.text "LOGIN"
        , viewInput "text" "Name" form.name Name
        , viewInput "password" "Password" form.password Password
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  Html.input [ Html.Attributes.type_ t, Html.Attributes.placeholder p, Html.Attributes.value v, Html.Events.onInput toMsg] []
