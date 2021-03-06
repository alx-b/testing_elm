module Register exposing (Model, Msg, update, view, init)

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Html exposing (Html)
import Html.Attributes
import Html.Events


type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }


init : ( Model, Cmd Msg )
init =
    ( Model "" "" "", Cmd.none )


type Msg
    = Name String
    | Password String
    | PasswordAgain String


update : Msg -> Model -> (Model, Cmd Msg)
update msg form =
    case msg of
        Name name ->
          ({ form | name = name }, Cmd.none)

        Password password ->
          ({ form | password = password }, Cmd.none )

        PasswordAgain password ->
          ({ form | passwordAgain = password }, Cmd.none )

    
view : Model -> Html Msg
view form =
    Html.div [Html.Attributes.class "register-form"]
        [ Html.text "REGISTER"
        , viewInput "text" "Name" form.name Name
        , viewInput "password" "Password" form.password Password
        , viewInput "password" "Re-enter Password" form.passwordAgain PasswordAgain
        , viewValidation form
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  Html.input [ Html.Attributes.type_ t, Html.Attributes.placeholder p, Html.Attributes.value v, Html.Events.onInput toMsg] []


viewValidation : Model -> Html msg
viewValidation model =
  if model.password == model.passwordAgain then
    Html.div [ Html.Attributes.style "color" "green" ] [ Html.text "OK" ]
  else
    Html.div [ Html.Attributes.style "color" "red" ] [ Html.text "Passwords do not match!" ]
