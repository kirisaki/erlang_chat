import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import Time

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



type alias Model =
  { input : String
  , messages : List String
  }


init : (Model, Cmd Msg)
init =
  (Model "" [], Cmd.none)

wsUrl : String
wsUrl = "ws://klaraworks.zato.klaraworks.net:8080/websocket"

type Msg
  = Input String
  | Send
  | Heartbeat
  | NewMessage String


update : Msg -> Model -> (Model, Cmd Msg)
update msg {input, messages} =
    case msg of
        Input newInput ->
            (Model newInput messages, Cmd.none)
                
        Send ->
            (Model "" messages, WebSocket.send wsUrl input)

        Heartbeat ->
            (Model "" messages, WebSocket.send wsUrl "4")
                
        NewMessage str ->
            (Model input (str :: messages), Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen wsUrl NewMessage
        , Time.every (30*Time.second) (\_-> Heartbeat)
        ]

view : Model -> Html Msg
view model =
  div []
    [ div [] (List.map viewMessage model.messages)
    , input [onInput Input] []
    , button [onClick Send] [text "Send"]
    ]


viewMessage : String -> Html msg
viewMessage msg =
  div [] [ text msg ]
