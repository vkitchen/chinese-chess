module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, Attribute, div, p, input, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (stopPropagationOn, onClick, onInput)
import Json.Decode as Decode
import String
import Url exposing (Url)
import Random
import Uuid.Barebones exposing (uuidStringGenerator)
import Debug
import Time
import Http
import Json.Decode as D
import Json.Encode as E

type Color = Red | Black
type PieceType = Advisor | Cannon | Chariot | Elephant | General | Horse | Soldier
-- color piece position file rank
type Piece = Piece Color PieceType Char Int

-- Where is the game rooted in order to load assets
path = "/games/chinese-chess/"
imgPath = path ++ "static/img/"

main =
  Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlRequest = onUrlRequest
    , onUrlChange = onUrlChange
    }

type alias Model =
  { key : Nav.Key
  , board : List Piece
  , selected : Maybe Piece
  , showLobby : Bool -- TODO lobby is derived off of roomCode ?
  , roomCode : Maybe String
  , player : Int
  , input : String
  }

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
  (
  { key = key
  , board = 
      case url.fragment of
        Nothing -> initialBoard
        Just _ -> rotateBoard initialBoard
  , selected = Nothing
  , showLobby =
      case url.fragment of
        Nothing -> True
        Just _ -> False
  , roomCode = url.fragment
  , player =
      case url.fragment of
        Nothing -> 1
        Just _ -> 2
  , input = ""
  }
  , Cmd.none )

rotateBoard : List Piece -> List Piece
rotateBoard board =
  List.map (\(Piece c p f r) -> Piece c p (fromInt (8 - toInt f)) (11 - r)) board

initialBoard : List Piece
initialBoard =
  -- Black
  -- Back row
  [ Piece Black Chariot 'a' 10
  , Piece Black Horse 'b' 10
  , Piece Black Elephant 'c' 10
  , Piece Black Advisor 'd' 10
  , Piece Black General 'e' 10
  , Piece Black Advisor 'f' 10
  , Piece Black Elephant 'g' 10
  , Piece Black Horse 'h' 10
  , Piece Black Chariot 'i' 10
  -- Cannons
  , Piece Black Cannon 'b' 8
  , Piece Black Cannon 'h' 8
  -- Soldiers
  , Piece Black Soldier 'a' 7
  , Piece Black Soldier 'c' 7
  , Piece Black Soldier 'e' 7
  , Piece Black Soldier 'g' 7
  , Piece Black Soldier 'i' 7

  -- Red
  -- Soldiers
  , Piece Red Soldier 'a' 4
  , Piece Red Soldier 'c' 4
  , Piece Red Soldier 'e' 4
  , Piece Red Soldier 'g' 4
  , Piece Red Soldier 'i' 4
  -- Cannons
  , Piece Red Cannon 'b' 3
  , Piece Red Cannon 'h' 3
  -- Back row
  , Piece Red Chariot 'a' 1
  , Piece Red Horse 'b' 1
  , Piece Red Elephant 'c' 1
  , Piece Red Advisor 'd' 1
  , Piece Red General 'e' 1
  , Piece Red Advisor 'f' 1
  , Piece Red Elephant 'g' 1
  , Piece Red Horse 'h' 1
  , Piece Red Chariot 'i' 1
  ]

type Msg
  = NoOp -- ILLEGAL STATE (here to circumvent the type system)
  | Select Piece
  | Deselect
  | Move Piece
  | CreateRoom
  | JoinRoom String
  | NewRoom String
  | Input String
  | Tick Time.Posix
  | UpdateBoard (Result Http.Error (List String))
  | PostSent (Result Http.Error ())

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({key, board, selected} as model) =
  case msg of
    NoOp ->
      ( model, Cmd.none )
    Select pce ->
      ( { model | selected = Just pce }, Cmd.none )
    Deselect ->
      ( { model | selected = Nothing }, Cmd.none )
    Move (Piece _ _ f r as pce_) ->
      case selected of
        Just pce ->
          -- TODO insert back into dropped spot?
          -- mildly annoying redraw bug with dom moving around
          let newBoard = List.filter (\(Piece _ _ f_ r_) -> not (f == f_ && r == r_)) board in
          let newBoard_ = List.filter (\p -> p /= pce) newBoard ++ [pce_] in
          let rBoard = case model.player of
                1 -> newBoard_
                _ -> rotateBoard newBoard_
          in
          case model.roomCode of
            Just rm ->
              let url = "/rooms/" ++ rm ++ ".json" in
              ( { model | board = newBoard_, selected = Nothing }, Http.post { url = url, body = Http.stringBody "application/json" (encodeBoard rBoard), expect = Http.expectWhatever PostSent } )
            Nothing ->
              ( { model | board = newBoard, selected = Nothing }, Cmd.none ) -- IMPOSSIBLE STATE
        Nothing ->
          ( model , Cmd.none ) -- IMPOSSIBLE STATE
    CreateRoom ->
      ( { model | showLobby = False, player = 1 }, Random.generate NewRoom uuidStringGenerator )
    JoinRoom newRoom ->
      ( { model | board = rotateBoard board, input = "", roomCode = Just newRoom, showLobby = False, player = 2 }, Nav.replaceUrl key ("#" ++ newRoom) )
    NewRoom newRoom ->
      ( { model | roomCode = Just newRoom }, Nav.replaceUrl key ("#" ++ newRoom) )
    Input npt ->
      ( { model | input = npt }, Cmd.none)
    Tick _ ->
      case model.roomCode of
        Just rm ->
          let url = "/rooms/" ++ rm ++ ".json" in
          ( model, Http.get { url = url, expect = Http.expectJson UpdateBoard decodeBoardJson } )
        Nothing ->
          ( model, Cmd.none ) -- waiting for room creation/join
    UpdateBoard (Ok brd) ->
      let newBoard = List.map decodePiece brd in
      let rBoard = case model.player of
            1 -> newBoard
            _ -> rotateBoard newBoard
      in
      ( { model | board = rBoard }, Cmd.none )
    UpdateBoard (Err httpError) -> -- TODO should do something
      ( model, Cmd.none )
    PostSent _ ->
      ( model, Cmd.none )

encodeBoard : List Piece -> String
encodeBoard board =
  E.encode 0 (E.list E.string (List.map encodePiece board))

encodePiece : Piece -> String
encodePiece (Piece color pceType file rank) =
  let color_ = case color of
        Black -> "Black"
        Red -> "Red"
  in
  color_ ++ ":" ++ encodePieceType pceType ++ ":" ++ String.fromChar file ++ ":" ++ String.fromInt rank

decodePiece : String -> Piece
decodePiece pce =
  let segs = String.split ":" pce in
  case segs of
    clr :: pceType :: file :: rank :: [] ->
      let clr_ = case clr of
            "Black" -> Black
            "Red" -> Red
            _ -> Red -- IMPOSSIBLE STATE
      in
      let file_ = case String.uncons file of
            Just (f, _) -> f
            Nothing -> 'a'
      in
      let rank_ = case String.toInt rank of
            Just r -> r
            Nothing -> 1
      in
      Piece clr_ (decodePieceType pceType) file_ rank_
    _ ->
      Piece Red Soldier 'a' 1 -- IMPOSSIBLE STATE

encodePieceType : PieceType -> String
encodePieceType pceType =
  case pceType of
    Advisor -> "Advisor"
    Cannon -> "Cannon"
    Chariot -> "Chariot"
    Elephant -> "Elephant"
    General -> "General"
    Horse -> "Horse"
    Soldier -> "Soldier"

decodePieceType : String -> PieceType
decodePieceType pceType =
  case pceType of
    "Advisor" -> Advisor
    "Cannon" -> Cannon
    "Chariot" -> Chariot
    "Elephant" -> Elephant
    "General" -> General
    "Horse" -> Horse
    "Soldier" -> Soldier
    _ -> Soldier -- IMPOSSIBLE STATE

decodeBoardJson : D.Decoder (List String)
decodeBoardJson =
  D.list D.string

subscriptions : model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick

onUrlRequest : UrlRequest -> Msg
onUrlRequest _ =
  NoOp

onUrlChange : Url -> Msg
onUrlChange _ =
  NoOp

stopPropOnClick : msg -> Attribute msg
stopPropOnClick msg =
  stopPropagationOn "click" (Decode.succeed (msg, True))

view : Model -> Document Msg
view model =
  { title = "Chinese Chess - Vaughan.Kitchen"
  , body =
    [ div
        [ style "position" "relative"
        , style "width" "521px"
        , style "height" "577px"
        , style "display" "inline-block"
        ]
        [ viewBoard model ]
    , div
        [ style "display" "inline-block"
        , style "vertical-align" "top"
        , style "width" "300px"
        , style "padding" "10px"
        ]
        [ p [] [ text "Known bugs: Generals shouldn't be able to face each other" ]
        , p [] [ text "Known bugs: No detection of game end states" ]
        , p [] [ text "Known bugs: Refreshing the page breaks things (turns you into black player)" ]
        , case model.selected of
            Just pce -> pieceInfo pce
            Nothing -> text "*No piece selected*"
        ]
    , if model.showLobby then
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
              [ p [] [ button [ onClick CreateRoom ] [ text "Create a new room" ] ]
              , p [] [ text "..OR.." ]
              , p []
                  [ input [ onInput Input ] []
                  , button [ onClick (JoinRoom model.input) ] [ text "Join Room" ]
                  ]
              ]
          ]
      else
        div [] []
    ]
  }

pieceInfo : Piece -> Html Msg
pieceInfo (Piece _ pceType _ _) =
  case pceType of
    Advisor ->
      text "Advisor (仕/士): Moves and captures one point diagonally. May not leave the palace"
    Cannon ->
      text "Cannon (炮/砲): Moves any number orthogonally. Captures by 'firing' over an intervening piece of own or enemy colour"
    Chariot ->
      text "Chariot (俥/車): Moves and captures any number orthogonally"
    Elephant ->
      text "Elephant (相/象): Moves and captures two points diagonally. The Elephant does not jump"
    General ->
      text "General (帥/將): Moves and captures one point orthogonally. May not leave the palace. And may not face the opposing general otherwise they will perform the 'flying general' (飛將) move and cross the entire board to capture the enemy general"
    Horse ->
      text "Horse (傌/馬): Moves and captures in a two step move with one point moved orthogonally and then one point diagonally. The horse does not jump. Blocking a horse is known as 'Hobbling the horse's leg' (蹩馬腿)"
    Soldier ->
      text "Soldier (兵/卒): Moves and captures by advancing one point. Upon crossing the river can move and capture horizontally one point"

viewBoard : Model -> Html Msg
viewBoard {player, board, selected} =
  div
    [ style "position" "absolute"
    , style "background-image" (imgUrl "board.jpg")
    , style "width" "521px"
    , style "height" "577px"
    , onClick Deselect
    ]
    ((List.map (viewPiece player selected) board)
    ++ viewMoves board selected
    )

viewPiece : Int -> Maybe Piece -> Piece -> Html Msg
viewPiece player selected (Piece color pceType file rank as pce) =
  let playerColor = case player of
        1 -> Red
        _ -> Black
  in
  let isSelected = selected == Just pce in
  div
    (pos 57 file rank
    ++
    [ style "position" "absolute"
    , style "width" "57px"
    , style "height" "57px"
    , style "background-image" (imgUrl (icon color pceType))
    , style "cursor" "pointer"
    , style "border-radius" "100%" -- stops cursor looking wrong when off piece
    ]
    ++
    if playerColor == color then
      [ stopPropOnClick (Select pce) ]
    else
      []
    ++
    if isSelected then
      [ style "border" "2px solid green" ]
    else []
    )
    []

viewMoves : List Piece -> Maybe Piece -> List (Html Msg)
viewMoves board mbyPce =
  case mbyPce of
    Nothing -> []
    Just pce ->
      List.map viewMove (moves board pce)

viewMove : Piece -> Html Msg
viewMove (Piece color pceType file rank as pce) =
  div
    (pos 25 file rank
    ++
    [ style "position" "absolute"
    , style "width" "25px"
    , style "height" "25px"
    , style "background-color" "green"
    , style "cursor" "pointer"
    , style "border-radius" "100%" -- stops cursor looking wrong when off piece
    , stopPropOnClick (Move pce)
    ]
    )
    []

moves : List Piece -> Piece -> List Piece
moves board (Piece color pceType file rank as pce) =
  (
  case pceType of
    Advisor ->
      [ Piece color pceType (subFile file 1) (rank + 1)
      , Piece color pceType (addFile file 1) (rank + 1)
      , Piece color pceType (addFile file 1) (rank - 1)
      , Piece color pceType (subFile file 1) (rank - 1)
      ]
    Cannon ->
      List.map (\file_ -> Piece color pceType (addFile 'a' file_) rank) (List.range 0 8)
      ++
      List.map (\rank_ -> Piece color pceType file rank_) (List.range 1 10)
    Chariot ->
      List.map (\file_ -> Piece color pceType (addFile 'a' file_) rank) (List.range 0 8)
      ++
      List.map (\rank_ -> Piece color pceType file rank_) (List.range 1 10)
    Elephant ->
      [ Piece color pceType (subFile file 2) (rank + 2)
      , Piece color pceType (addFile file 2) (rank + 2)
      , Piece color pceType (addFile file 2) (rank - 2)
      , Piece color pceType (subFile file 2) (rank - 2)
      ]
    General ->
      [ Piece color pceType file (rank + 1)
      , Piece color pceType (addFile file 1) rank
      , Piece color pceType file (rank - 1)
      , Piece color pceType (subFile file 1) rank
      ]
    Horse ->
      -- forward + diag
      [ Piece color pceType (addFile file 1) (rank + 2)
      , Piece color pceType (subFile file 1) (rank + 2)
      -- right + diag
      , Piece color pceType (addFile file 2) (rank + 1)
      , Piece color pceType (addFile file 2) (rank - 1)
      -- back + diag
      , Piece color pceType (addFile file 1) (rank - 2)
      , Piece color pceType (subFile file 1) (rank - 2)
      -- left + diag
      , Piece color pceType (subFile file 2) (rank + 1)
      , Piece color pceType (subFile file 2) (rank - 1)
      ]
    Soldier ->
      [ Piece color pceType file (rank + 1)
      , Piece color pceType (addFile file 1) rank
      , Piece color pceType (subFile file 1) rank
      ]
    )
    |> List.filterMap pruneOffBoard
    |> List.filterMap pruneOutOfPalace
    |> List.filterMap (pruneNotAcrossRiver pce)
    |> List.filterMap (pruneLandOnTeam board)
    |> List.filterMap (pruneBlocked board pce)

-- Why did I mix 0 addressing and 1 addressing?
pruneOffBoard : Piece -> Maybe Piece
pruneOffBoard (Piece color pceType file rank as pce) =
  if not (0 <= toInt file && toInt file <= 8) then
    Nothing
  else if not (1 <= rank && rank <= 10) then
    Nothing
  else
    Just pce

pruneOutOfPalace : Piece -> Maybe Piece
pruneOutOfPalace (Piece color pceType file rank as pce) =
  if pceType == Advisor || pceType == General then
    if not (3 <= toInt file && toInt file <= 5) then
      Nothing
    else if not (1 <= rank && rank <= 3) then
      Nothing
    else
      Just pce
  else
    Just pce

-- originalPiece possibleMove
pruneNotAcrossRiver : Piece -> Piece -> Maybe Piece
pruneNotAcrossRiver (Piece _ _ f r) (Piece color pceType file rank as pce) =
  case pceType of
    Soldier ->
      if f /= file && not (5 < rank) then
        Nothing
      else
        Just pce
    _ ->
      Just pce

-- removes if would land on team or self
pruneLandOnTeam : List Piece -> Piece -> Maybe Piece
pruneLandOnTeam board (Piece color pceType file rank as pce) =
  if List.isEmpty (List.filter
      (\(Piece c _ f r) -> c == color && f == file && r == rank)
      board
    )
  then
    Just pce
  else
    Nothing

-- board originalPiece possibleMove
pruneBlocked : List Piece -> Piece -> Piece -> Maybe Piece
pruneBlocked board (Piece _ pceType file rank as pce) (Piece _ _ file_ rank_ as pce_) =
  case pceType of
    Cannon ->
      if file == file_ && rank_ > rank then
        let sameFile = List.filter
              (\(Piece _ _ f r as p) -> p /= pce && f == file && r > rank) board
              |> sortByRank
        in
        case sameFile of
          (Piece _ _ _ r) :: (Piece _ _ _ r_) :: xss ->
            if r_ == rank_ then
              Just pce_
            else if r > rank_ then
              Just pce_
            else
              Nothing
          (Piece _ _ _ r) :: xs ->
            if r > rank_ then
              Just pce_
            else
              Nothing
          [] ->
            Just pce_
      else if file == file_ && rank_ < rank then
        let sameFile = List.filter
              (\(Piece _ _ f r as p) -> p /= pce && f == file && r < rank) board
              |> sortByRank
              |> List.reverse
        in
        case sameFile of
          (Piece _ _ _ r) :: (Piece _ _ _ r_) :: xss ->
            if r_ == rank_ then
              Just pce_
            else if r < rank_ then
              Just pce_
            else
              Nothing
          (Piece _ _ _ r) :: xs ->
            if r < rank_ then
              Just pce_
            else
              Nothing
          [] ->
            Just pce_
      else if rank == rank_ && toInt file_ > toInt file then
        let sameRank = List.filter
              (\(Piece _ _ f r as p) -> p /= pce && r == rank && toInt f > toInt file) board
              |> sortByFile
        in
        case sameRank of
          (Piece _ _ f _) :: (Piece _ _ f_ _) :: xss ->
            if f_ == file_ then
              Just pce_
            else if f > file_ then
              Just pce_
            else
              Nothing
          (Piece _ _ f _) :: xs ->
            if f > file_ then
              Just pce_
            else
              Nothing
          [] ->
            Just pce_
      else if rank == rank_ && toInt file_ < toInt file then
        let sameRank = List.filter
              (\(Piece _ _ f r as p) -> p /= pce && r == rank && toInt f < toInt file) board
              |> sortByFile
              |> List.reverse
        in
        case sameRank of
          (Piece _ _ f _) :: (Piece _ _ f_ _) :: xss ->
            if f_ == file_ then
              Just pce_
            else if f < file_ then
              Just pce_
            else
              Nothing
          (Piece _ _ f _) :: xs ->
            if f < file_ then
              Just pce_
            else
              Nothing
          [] ->
            Just pce_
      else
        Just pce_ -- IMPOSSIBLE STATE
    Chariot ->
      if file == file_ && rank_ > rank then
        if List.isEmpty (List.filter
          (\(Piece _ _ f r as p) -> p /= pce && f == file && r - rank > 0 && r - rank < rank_ - rank)
          board
          )
        then
          Just pce_
        else
          Nothing
      else if file == file_ && rank_ < rank then
        if List.isEmpty (List.filter
          (\(Piece _ _ f r as p) -> p /= pce && f == file && r - rank < 0 && r - rank > rank_ - rank)
          board
          )
        then
          Just pce_
        else
          Nothing
      else if rank == rank_ && toInt file_ > toInt file then
        if List.isEmpty (List.filter
          (\(Piece _ _ f r as p) -> p /= pce && r == rank && toInt f - toInt file > 0 && toInt f - toInt file < toInt file_ - toInt file)
          board
          )
        then
          Just pce_
        else
          Nothing
      else if rank == rank_ && toInt file_ < toInt file then
        let fdiff = abs (toInt file_ - toInt file) in
        if List.isEmpty (List.filter
          (\(Piece _ _ f r as p) -> p /= pce && r == rank && toInt f - toInt file < 0 && toInt f - toInt file > toInt file_ - toInt file)
          board
          )
        then
          Just pce_
        else
          Nothing
      else
        Just pce_ -- IMPOSSIBLE STATE
    Elephant ->
      -- two step blocked by first step
      let (Piece _ _ f r) = interpolateMove pce pce_ in
      if List.isEmpty (List.filter (\(Piece _ _ f_ r_) -> f_ == f && r_ == r) board) then
        Just pce_
      else
        Nothing
    Horse ->
      -- two step blocked by first step
      let (Piece _ _ f r) = interpolateMove pce pce_ in
      if List.isEmpty (List.filter (\(Piece _ _ f_ r_) -> f_ == f && r_ == r) board) then
        Just pce_
      else
        Nothing
    -- Single step pieces: Advisor, General, Soldier accounted for by pruneLandOnTeam
    _ ->
      Just pce_

-- originalPiece possibleMove -> inBetweenMove
-- Works for two step pieces only (Horse and Elephant)
interpolateMove : Piece -> Piece -> Piece
interpolateMove (Piece color pceType f r) (Piece _ _ f_ r_) =
  case pceType of
    Elephant ->
      let f__ = fromInt (toInt f + (toInt f_ - toInt f) // 2) in
      let r__ = r + (r_ - r) // 2 in
      Piece color pceType f__ r__
    Horse ->
      let fdiff = toInt f_ - toInt f in
      let rdiff = r_ - r in
      if fdiff == 2 || fdiff == -2 then
        Piece color pceType (fromInt (toInt f + fdiff // 2)) r
      else if rdiff == 2 || rdiff == -2 then
        Piece color pceType f (r + rdiff // 2)
      else
        Piece color pceType f r -- IMPOSSIBLE STATE
    _ ->
      Piece color pceType f r -- IMPOSSIBLE STATE

sortByFile : List Piece -> List Piece
sortByFile pces =
  List.sortWith (\(Piece _ _ f1 _) (Piece _ _ f2 _) -> compare f1 f2) pces

sortByRank : List Piece -> List Piece
sortByRank pces =
  List.sortWith (\(Piece _ _ _ r1) (Piece _ _ _ r2) -> compare r1 r2) pces

icon : Color -> PieceType -> String
icon color pce =
  (
  case color of
    Red -> "red"
    Black -> "black"
  ) ++ "-" ++ (
  case pce of
    Advisor -> "advisor"
    Cannon -> "cannon"
    Chariot -> "chariot"
    Elephant -> "elephant"
    General -> "general"
    Horse -> "horse"
    Soldier -> "soldier"
  ) ++ ".gif"

-- First square starts 34px from left, and 32px from top
-- Squares are 58x58px including boarder, ie corners are 58px apart
-- Pieces are 57x57px
pos : Int -> Char -> Int -> List (Attribute msg)
pos width file rank =
  let file_ = toInt file in
  let rank_ = 10 - rank in
  [ style "top" (String.fromInt (rank_ * 58 + ((57 - width) // 2)) ++ "px")
  , style "left" (String.fromInt (file_ * 58 + ((57 - width) // 2)) ++ "px")
  ]

addFile : Char -> Int -> Char
addFile c i =
  fromInt ((toInt c) + i)

subFile : Char -> Int -> Char
subFile c i =
  fromInt ((toInt c) - i)

toInt : Char -> Int
toInt c =
  Char.toCode c - Char.toCode 'a'

fromInt : Int -> Char
fromInt c =
  Char.fromCode (Char.toCode 'a' + c)

imgUrl : String -> String
imgUrl u =
  "url('" ++ imgPath ++ u ++ "')"
