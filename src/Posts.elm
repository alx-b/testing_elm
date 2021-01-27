module Posts exposing (Model, Msg, update, view, init)

import Browser exposing (UrlRequest(..))
import Browser.Navigation
import Html exposing (Html)
import Html.Attributes
import Html.Events


type alias Post =
  { title : String
  , content : String
  , author : String
  }

type alias Model =
  { posts :  List Post
  }
 

init : (Model, Cmd Msg)
init =
    ( Model
          [ Post "title" "content" "author"
          , Post "title2" "content2" "author2"
          ]
    , Cmd.none
    )
          

type Msg
    = NextPage
    | PreviousPage


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NextPage ->
        (model, Cmd.none)
    PreviousPage ->
        (model, Cmd.none)


--view : Model -> Html Msg
view model =
    Html.div [Html.Attributes.class "posts"]
        ( List.map viewPost model.posts
        )
       
--
viewPost : Post -> Html Msg
viewPost post =
    Html.div [Html.Attributes.class "post"]
        [ Html.div [] [Html.text post.title]
        , Html.div [] [Html.text post.content]
        , Html.div [] [Html.text post.author]
        ]
