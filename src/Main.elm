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
    , currentPage : Page
    }


type Page
    = HomePage
    | RegisterPage Register.Model
    | LoginPage Login.Model
    | NotFoundPage
    

init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    updateUrl url { navigationKey = key, currentPage = NotFoundPage}


-- ROUTE


type Route
    = Home
    | Register
    | Login

        
routeParser : Url.Parser.Parser (Route -> a) a
routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map Home Url.Parser.top
        , Url.Parser.map Register (Url.Parser.s "register")
        , Url.Parser.map Login (Url.Parser.s "login")
        ]

        
updateUrl : Url.Url -> Model -> (Model, Cmd Msg)
updateUrl url model =
    case Url.Parser.parse routeParser url of                                
        Just Register ->
            toRegisterPage model (Register.init)

        Just Login ->
            toLoginPage model (Login.init)

        Just Home ->
            ( { model | currentPage = HomePage }, Cmd.none )    

        Nothing ->
            ( { model | currentPage = NotFoundPage }, Cmd.none )


toRegisterPage : Model -> ( Register.Model, Cmd Register.Msg ) -> ( Model, Cmd Msg )
toRegisterPage model ( register, cmd ) =
    ( { model | currentPage = RegisterPage register }
    , Cmd.map RegisterMsg cmd
    )


toLoginPage : Model -> ( Login.Model, Cmd Login.Msg ) -> ( Model, Cmd Msg )
toLoginPage model ( login, cmd ) =
    ( { model | currentPage = LoginPage login }
    , Cmd.map LoginMsg cmd
    )


-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
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
            updateUrl url model


        RegisterMsg message ->
            case model.currentPage of                                        
                RegisterPage registerModel ->                               
                    toRegisterPage model (Register.update message registerModel)

                _ ->                                               
                    ( model, Cmd.none )                              

        LoginMsg message ->
            case model.currentPage of                                        
                LoginPage loginModel ->                               
                    toLoginPage model (Login.update message loginModel)

                _ ->                                               
                    ( model, Cmd.none )                              
            
            
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.currentPage of
                RegisterPage register ->
                    Register.view register
                        |> Html.map RegisterMsg

                LoginPage login ->
                    Login.view login
                        |> Html.map LoginMsg

                HomePage ->
                    Html.text "HOME"

                NotFoundPage ->
                    Html.text "NOT FOUND"
    in
        { title = "SPA test"
        , body =
            [ navBar 
            , content
            ]
        }


navBar =
   Html.div [Html.Attributes.class "navigation-bar"]
       [ viewLink "/" "Home"
       , viewLink "/register" "Register"
       , viewLink "/login" "Login"
       ]


viewLink : String -> String -> Html msg
viewLink path text =
    Html.div [] [ Html.a [Html.Attributes.href path, Html.Attributes.class "nav-links"] [ Html.text text ] ]
