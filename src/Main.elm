module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, Attribute, div, p, input, button, br, text)
import Html.Attributes exposing (style, placeholder)
import Html.Events exposing (stopPropagationOn, onClick, onInput)
import Json.Decode as Decode
import String
import Url exposing (Url)
import Random
import Uuid.Barebones exposing (uuidStringGenerator, isValidUuid)
import Debug
import Time
import Http
import Json.Decode as D
import Json.Encode as E

import Xiangqi

gameName = "chinese chess" -- Used for room brokering
pageTitle = "Chinese Chess - Vaughan.Kitchen"
rootPath = "/games/chinese-chess/"
imgPath = rootPath ++ "static/img/"

imgUrl : String -> String
imgUrl u =
  "url('" ++ imgPath ++ u ++ "')"

type JoinState = Lobby | WaitingOpponent | Playing
type alias Room =
  { roomCode : Maybe String
  , joinState : JoinState
  , inputRoomCode : String
  , players: List String
  , turnOrder : Int
  , currentTurn : Int
  }
-- LocalGame currentTurn
type GameType = NoGameType | LocalGame Int | NetworkGame Room

main =
  Browser.application
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    , onUrlRequest = (\_ -> NoOp)
    , onUrlChange = (\_ -> NoOp)
    }

type alias Model =
  { error : Maybe String
  , failedNetReq : Int
  , key : Nav.Key
  , userId : String
  , gameType : GameType
  , gameState : Xiangqi.Model
  }

type alias GameState =
  { game: String
  , players: List String
  , currentTurn : Int
  }

-- Uid is a flag from JS. It is a unique per-browser user code in a cookie
init : String -> Url -> Nav.Key -> ( Model, Cmd Msg )
init uid url key =
  let (subModel, _) = Xiangqi.init uid url key in
  let defaultModel =
        { error = Nothing
        , failedNetReq = 0
        , key = key
        , userId = uid
        , gameType = NoGameType
        , gameState = subModel
        }
  in
  -- Specialisation of JoinRoom update method
  case url.fragment of
    Nothing ->
      ( defaultModel, Cmd.none )
    Just roomCode ->
      if isValidUuid roomCode then
        let url_ = "/rooms/" ++ roomCode ++ ".json" in
        ( { defaultModel
              | gameType = NetworkGame
                  { roomCode = Just roomCode
                  , joinState = Lobby
                  , inputRoomCode = ""
                  , players = []
                  , turnOrder = 0
                  , currentTurn = 1
                  }
          }
        , Http.get
            { url = url_
            , expect = Http.expectJson (RoomMsg << RoomReceived) decodeStateJson
            }
        )
      else
        ( { defaultModel | error = Just "Invalid room code" }
        , Cmd.none
        )

subscriptions : model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick

type GameTypeMsg = SelectLocal | SelectNetwork
type Msg
  = NoOp
  | ReloadPage
  | SelectGameType GameTypeMsg
  | Tick Time.Posix
  | RoomMsg RmMsg
--  | UpdateGameState (Result Http.Error GameState)
--  | GameStateSent (Result Http.Error ())
  | GameMsg Xiangqi.Msg

type RmMsg
  = DetermineTurnOrder Int
  | NewRoom String
  | CreateRoom
  | RoomCreated (Result Http.Error ())
  | InputRoomCode String
  | JoinRoom
  | RoomReceived (Result Http.Error GameState)
  | RoomJoined (Result Http.Error ())

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({key} as model) =
  case msg of
    NoOp ->
      ( model, Cmd.none )
    ReloadPage ->
      ( model, Nav.reload )
    SelectGameType gameType ->
      case gameType of
        SelectNetwork ->
          ( { model
              | gameType = NetworkGame
                { roomCode = Nothing
                , joinState = Lobby
                , inputRoomCode = ""
                , players = []
                , turnOrder = 0
                , currentTurn = 1
                }
            }
          , Random.generate (RoomMsg << DetermineTurnOrder) (Random.int 1 2)
          )
        SelectLocal ->
          ( { model | gameType = LocalGame 1 }, Cmd.none )
    Tick _ ->
      ( model, Cmd.none )
{-
      if not
        ( model.joinState == Playing
        || model.joinState == WaitingOpponent
        )
      then
        ( model, Cmd.none )
      else if model.gameType /= Just Network then
        ( model, Cmd.none )
      else if model.failedNetReq >= 5 then
        ( model, Cmd.none )
      else
        case model.roomCode of
          Just rm ->
            let url = "/rooms/" ++ rm ++ ".json" in
            ( model
            , Http.get
                { url = url
                , expect = Http.expectJson UpdateGameState decodeStateJson
                }
            )
          Nothing ->
            ( model, Cmd.none ) -- IMPOSSIBLE
    GameStateSent (Ok _) ->
      ( model, Cmd.none ) -- Successful noop
    GameStateSent (Err e) ->
      ( { model | error = Just ("""
            Move failed due to network error. Please reload page and try again.
            Error was""" ++ (netErrToString e))
        }, Cmd.none )
-- XXX ???
    UpdateGameState _ ->
      ( model, Cmd.none )
-}
    RoomMsg subMsg ->
      case model.gameType of
        NetworkGame room ->
          let (updated, cmd) = updateRoom subMsg model room in
          case updated of
            Ok updated_ -> ( { model | gameType = NetworkGame updated_ }, cmd )
            Err e -> ( { model | error = Just e }, cmd )
        _ -> ( model, Cmd.none )
    GameMsg subMsg ->
      let (model_, cmd) = Xiangqi.update subMsg model.gameState in
      ( { model | gameState = model_ }, Cmd.none )

updateRoom : RmMsg -> Model -> Room -> ( Result String Room, Cmd Msg )
updateRoom msg model room =
  case msg of
    -- For host only. Redetermines on connection
    DetermineTurnOrder trnOrd ->
      ( Ok { room | turnOrder = trnOrd }, Cmd.none )
    CreateRoom ->
      case room.turnOrder of
        1 -> 
          ( Ok { room | players = [ model.userId, "" ] }
          , Random.generate (RoomMsg << NewRoom) uuidStringGenerator
          )
        _ ->
          ( Ok { room | players = [ "", model.userId ] }
          , Random.generate (RoomMsg << NewRoom) uuidStringGenerator
          )
    NewRoom newRoom ->
      let url = "/rooms/" ++ newRoom ++ ".json" in
      ( Ok { room | roomCode = Just newRoom }
        , Http.post
            { url = url
            , body = Http.jsonBody (encodeStateJson model)
            , expect = Http.expectWhatever (RoomMsg << RoomCreated)
            }
      )
    RoomCreated (Ok _) ->
      case room.roomCode of
        Just rm ->
          ( Ok { room | joinState = WaitingOpponent }
          , Nav.replaceUrl model.key ("#" ++ rm)
          )
        _ ->
          ( Ok room, Cmd.none ) -- IMPOSSIBLE
    RoomCreated (Err e) ->
      ( Err ("Failed to create room." ++ netErrToString e)
      , Cmd.none
      )
    InputRoomCode npt ->
      ( Ok { room | inputRoomCode = npt }, Cmd.none)
    JoinRoom ->
      if isValidUuid room.inputRoomCode then
        let url = "/rooms/" ++ room.inputRoomCode ++ ".json" in
        ( Ok { room | inputRoomCode = "", roomCode = Just room.inputRoomCode }
        , Http.get
            { url = url
            , expect = Http.expectJson (RoomMsg << RoomReceived) decodeStateJson
            }
        )
      else
        ( Err "Invalid room code", Cmd.none )
    RoomReceived (Ok {players, currentTurn}) ->
      let url = "/rooms/" ++ (case room.roomCode of
              Just rm -> rm
              Nothing -> "" -- IMPOSSIBLE
            ) ++ ".json"
      in
      let (firstPlayer, secondPlayer) = case players of
            fp :: sp :: _  -> (fp, sp)
            _ -> ("nope", "nope") -- Will trigger a join error later
      in
      -- Could probably be a bit cleaner
      if firstPlayer == model.userId then
        ( Ok { room
            | joinState = Playing
            , players = players
            , turnOrder = 1
            , currentTurn = currentTurn
        }, Cmd.none )
      else if secondPlayer == model.userId then
        ( Ok { room
            | joinState = Playing
            , players = players
            , turnOrder = 2
            , currentTurn = currentTurn
        }, Cmd.none )
      else if firstPlayer == "" then
        let newRoom = 
              { room
                | players = [ model.userId, secondPlayer ]
                , turnOrder = 1
              }
        in
        ( Ok newRoom, Http.post
              { url = url
              , body = Http.jsonBody (encodeStateJson { model | gameType = NetworkGame newRoom })
              , expect = Http.expectWhatever (RoomMsg << RoomJoined)
              }
        )
      else if secondPlayer == "" then
        let newRoom =
              { room
                | players = [ firstPlayer, model.userId ]
                , turnOrder = 2
              }
        in
        ( Ok newRoom, Http.post
              { url = url
              , body = Http.jsonBody (encodeStateJson { model | gameType = NetworkGame newRoom })
              , expect = Http.expectWhatever (RoomMsg << RoomJoined)
              }
        )
      else
        ( Err "Room appears to already be full", Cmd.none )
    RoomReceived (Err e) ->
      ( Err ("Failed to join room." ++ netErrToString e), Cmd.none )
    RoomJoined (Ok _) ->
      case room.roomCode of
        Just rm ->
          ( Ok { room | joinState = Playing }
          , Nav.replaceUrl model.key ("#" ++ rm)
          )
        _ ->
          ( Ok room, Cmd.none ) -- IMPOSSIBLE
    RoomJoined (Err e) ->
      ( Err ("Failed to join room." ++ netErrToString e), Cmd.none )

netErrToString : Http.Error -> String
netErrToString err =
  case err of
    Http.Timeout ->
      " Network timeout"
    Http.NetworkError ->
      " Network error"
    Http.BadStatus status ->
      " Status code: " ++ String.fromInt status
    _ ->
      " Unknown error"

decodeStateJson : D.Decoder (GameState)
decodeStateJson =
  D.map3 GameState
    (D.field "game" D.string)
    (D.field "players" (D.list D.string))
    (D.field "currentTurn" D.int)

encodeStateJson : Model -> E.Value
encodeStateJson model =
  case model.gameType of
    NetworkGame room ->
      E.object
        [ ( "game", E.string gameName )
        , ( "players", E.list E.string room.players )
        , ( "currentTurn", E.int room.currentTurn )
    -- , ( "gameState", E.list E.string (List.map pieceToString board))
        ]
    _ ->
      E.object []

stopPropOnClick : msg -> Attribute msg
stopPropOnClick msg =
  stopPropagationOn "click" (Decode.succeed (msg, True))

view : Model -> Document Msg
view model =
  { title = pageTitle
  , body =
    [ div
        [ style "position" "relative"
        , style "width" "521px"
        , style "height" "577px"
        , style "display" "inline-block"
        ]
        [ Xiangqi.viewExample ]
    , div
        [ style "display" "inline-block"
        , style "vertical-align" "top"
        , style "width" "300px"
        , style "padding" "10px"
        ]
        [ p [] [ text "Welcome to Chinese Chess" ]
        , p [] [ text "Known bugs: You can currently move into check" ]
        , turnInfo model
        , text "Piece info:"
        ]
    ]
    ++
    case model.error of
      Just err -> [ viewError err ]
      Nothing ->
        case model.gameType of
          NoGameType -> [ gameSelection model ]
          LocalGame _ -> []
          NetworkGame {joinState} ->
            case joinState of
              Lobby -> [ lobby ]
              WaitingOpponent -> [ waitingOpponent model ]
              Playing -> []
  }

turnInfo : Model -> Html Msg
turnInfo model =
  p [] [ text "Game not in progress" ]
{-
  let color = case model.turnOrder of
        1 -> "Red"
        _ -> "Black"
  in
  case model.joinState of
    Playing ->
      case model.gameType of
        Just Network ->
          if model.currentTurn == model.turnOrder then
            p [] [ text ("You are " ++ color ++ ". It is your turn") ]
          else
            p [] [ text ("You are " ++ color ++ ". Waiting for opponent") ]
        Just Local ->
          case model.currentTurn of
            1 -> p [] [ text "Red players turn" ]
            _ -> p [] [ text "Black players turn" ]
        Nothing ->
          p [] [ text "IMPOSSIBLE STATE REACHED" ]
    _ ->
      p [] [ text "Game not in progress" ]
-}

viewError : String -> Html Msg
viewError err =
  modal
    [ p [] [ text "ERROR:" ]
    , p [] [ text err ]
    , button [ onClick ReloadPage ] [ text "Reload page" ]
    ]

gameSelection : Model -> Html Msg
gameSelection model =
  modal
    [ p [] [ text "Please select a game type:" ]
    , button [ onClick (SelectGameType SelectNetwork) ] [ text "1v1 Network" ]
    , button [ onClick (SelectGameType SelectLocal) ] [ text "1v1 Local" ]
    ]

lobby : Html Msg
lobby =
  modal
    [ p [] [ text "Join or create a room:" ]
    , p [] [ button [ onClick (RoomMsg CreateRoom) ] [ text "Create a new room" ] ]
    , p []
        [ input [ placeholder "Enter room id...", onInput (RoomMsg << InputRoomCode) ] []
        , button [ onClick (RoomMsg JoinRoom) ] [ text "Join room" ]
        ]
    ]

waitingOpponent : Model -> Html Msg
waitingOpponent model =
  p [] [ text "Broken" ]
{-
  let roomCode = case model.roomCode of
        Just rmCd -> rmCd
        Nothing -> ""
  in
  modal
    [ p [] [ text "Waiting for opponent to join" ]
    , p [] [ text "Copy the url or room code to send them" ]
    , p [] [ text ("Room code: " ++ roomCode) ]
    ]
-}

modal : List (Html Msg) -> Html Msg
modal children =
  div
    [ style "position" "absolute"
    , style "top" "0"
    , style "right" "0"
    , style "bottom" "0"
    , style "left" "0"
    , style "background-color" "rgba(0, 0, 0, 0.4)"
    ]
    [ div
        [ style "position" "absolute"
        , style "top" "50%"
        , style "left" "50%"
        , style "transform" "translate(-50%, -50%)"
        , style "background-color" "white"
        , style "padding" "16px"
        ]
        children
    ]
