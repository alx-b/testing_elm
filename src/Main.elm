module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Html exposing (Html)
import Html.Attributes
import Url
import Url.Parser exposing ((</>))

import Register
import Login

-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


-- MODEL
    

type alias Model =
    { navigationKey : Browser.Navigation.Key
    , currentRoute : Route
    , registerPageModel : Register.Model
    , loginPageModel : Login.Model
    }

    
init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        newRoute =
            parseUrlToRoute url

        newModel =
            { navigationKey = key
            , currentRoute = newRoute
            , registerPageModel = Register.init
            , loginPageModel = Login.init
            }

    in
        (newModel, Cmd.none)


-- ROUTE


type Route
    = Home
    | Register
    | Login
    | NotFound

        
routeParser : Url.Parser.Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map Home Url.Parser.top
        , Url.Parser.map Register (Url.Parser.s "register")
        , Url.Parser.map Login (Url.Parser.s "login")
        ]


parseUrlToRoute : Url.Url -> Route
parseUrlToRoute url =
    Maybe.withDefault NotFound (Url.Parser.parse routeParser url)


-- UPDATE


type Msg
    = ViewHomePage
    | ViewRegisterPage
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | RegisterMsg Register.Msg
    | LoginMsg Login.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.navigationKey (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        UrlChanged url ->
            ( { model | currentRoute = parseUrlToRoute url }
            , Cmd.none
            )

        ViewHomePage ->
            ({ model | currentRoute = Home }, Cmd.none)
        
        ViewRegisterPage ->
            ({ model | currentRoute = Register }, Cmd.none)

        RegisterMsg message ->
            let
                newRegisterPageModel =
                    Register.update message model.registerPageModel
            in
                ({ model | registerPageModel = newRegisterPageModel }, Cmd.none)

        LoginMsg message ->
            let
                newLoginPageModel =
                    Login.update message model.loginPageModel
            in
                ({ model | loginPageModel = newLoginPageModel }, Cmd.none)
            
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.currentRoute of
                Home ->
                    viewHome

                Register ->
                    Html.map RegisterMsg (Register.view model.registerPageModel)
                    --viewRegister

                Login ->
                    Html.map LoginMsg (Login.view model.loginPageModel)

                NotFound ->
                    Html.text "Not found"
    in
        { title = "SPA test"
        , body =
            [ viewHeader
            , content
            ]
        }


viewHeader =
    Html.div []
        [ viewLink "/" "Home"
        , viewLink "/register" "Register"
        , viewLink "/login" "Login"
        ]


viewHome =
    Html.text "HOME"


viewRegister =
    Html.text "REGISTER"


viewLink : String -> String -> Html msg
viewLink path text =
    Html.li [] [ Html.a [ Html.Attributes.href path ] [ Html.text text ] ]

