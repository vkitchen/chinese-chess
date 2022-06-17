module Xiangqi exposing (Board, Model, Msg, boardFromNetwork, decoder, encoder, init, networkFromModel, update, view)

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

import Lib exposing (..)

type Color = Red | Black
type PieceType =
  Advisor | Cannon | Chariot | Elephant | General | Horse | Soldier
-- color piece file rank
type Piece = Piece Color PieceType Char Int

pieceTypeToString : PieceType -> String
pieceTypeToString pceType =
  case pceType of
    Advisor -> "Advisor"
    Cannon -> "Cannon"
    Chariot -> "Chariot"
    Elephant -> "Elephant"
    General -> "General"
    Horse -> "Horse"
    Soldier -> "Soldier"

pieceTypeFromString : String -> PieceType
pieceTypeFromString pceType =
  case pceType of
    "Advisor" -> Advisor
    "Cannon" -> Cannon
    "Chariot" -> Chariot
    "Elephant" -> Elephant
    "General" -> General
    "Horse" -> Horse
    _ -> Soldier

pieceToString : Piece -> String
pieceToString (Piece color pceType file rank) =
  let color_ = case color of
        Black -> "Black"
        Red -> "Red"
  in
  color_
  ++ ":" ++ pieceTypeToString pceType
  ++ ":" ++ String.fromChar file
  ++ ":" ++ String.fromInt rank

pieceFromString : String -> Piece
pieceFromString pce =
  case String.split ":" pce of
    clr :: pceType :: file :: rank :: [] ->
      Piece
        (case clr of
          "Black" -> Black
          _ -> Red
        )
        (pieceTypeFromString pceType)
        (case String.uncons file of
          Just (f, _) -> f
          Nothing -> 'a'
        )
        (case String.toInt rank of
            Just r -> r
            Nothing -> 1
        )
    _ ->
      Piece Red Soldier 'a' 1 -- IMPOSSIBLE

rootPath = "/games/chinese-chess/"
imgPath = "static/img/"

type alias Board = List String

decoder = D.list D.string
encoder = E.list E.string

networkFromModel model =
  List.map pieceToString model.board

boardFromNetwork model board =
  { model | board = List.map pieceFromString board }

type alias Model =
  { joinState : JoinState
  , gameType : GameType
  , whensMyTurn : Int
  , whosTurnNow : Int
  , board : List Piece
  , selected : Maybe Piece
  }

type alias GameState =
  { players: List String
  , whosTurnNow : Int
  , boardStringy: List String
  }

init : JoinState -> GameType -> Int -> Model
init joinState gameType myTurn =
  { joinState = joinState
  , gameType = gameType
  , whensMyTurn = myTurn
  , whosTurnNow = 1
  , board = initialBoard
  , selected = Nothing
  }

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
  = NoOp
  | SelectPiece Piece
  | DeselectPiece
  | MovePiece Piece

update : Msg -> Model -> ( Model, Bool )
update msg ({board, selected} as model) =
  case msg of
    NoOp ->
      ( model, False )
    SelectPiece pce ->
      ( { model | selected = Just pce }, False )
    DeselectPiece ->
      ( { model | selected = Nothing }, False )
    MovePiece (Piece _ _ f r as pce_) ->
      case selected of
        Just pce ->
          -- TODO insert back into dropped spot?
          -- mildly annoying redraw bug with dom moving around
          let newBoard = List.filter
                (\(Piece _ _ f_ r_) -> not (f == f_ && r == r_)) board
          in
          let newBoard_ = List.filter (\p -> p /= pce) newBoard ++ [pce_] in
          let nextTurn = case model.whosTurnNow of
                1 -> 2
                _ -> 1
          in
          let newModel =
                { model
                    | board = newBoard_
                    , selected = Nothing
                    , whosTurnNow = nextTurn
                }
          in
          ( newModel, True )
        Nothing ->
          ( model, False ) -- IMPOSSIBLE
{-
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
    (D.field "players" (D.list D.string))
    (D.field "whosTurnNow" D.int)
    (D.field "gameState" (D.list D.string))

encodeStateJson : Model -> E.Value
encodeStateJson {players, whosTurnNow, board} =
  E.object
    [ ( "players", E.list E.string players )
    , ( "whosTurnNow", E.int whosTurnNow )
    , ( "gameState", E.list E.string (List.map pieceToString board))
    ]
-}

-- XXX
rotateBoard : List Piece -> List Piece
rotateBoard board =
  List.map (\(Piece c p f r) -> Piece c p (fromInt (8 - toInt f)) (11 - r)) board

stopPropOnClick : msg -> Attribute msg
stopPropOnClick msg =
  stopPropagationOn "click" (Decode.succeed (msg, True))

view : Model -> Html Msg
view model =
  div []
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
      [ p [] [ text "Welcome to Chinese Chess" ]
      , p [] [ text "Known bugs: You can currently move into check" ]
      , turnInfo model
      , text "Piece info:"
      , br [] []
      , case model.selected of
          Just pce -> pieceInfo pce
          Nothing -> text "*No piece selected*"
      ]
  ]

turnInfo : Model -> Html Msg
turnInfo model =
  let color = case model.whensMyTurn of
        1 -> "Red"
        _ -> "Black"
  in
  case model.joinState of
    Playing ->
      case model.gameType of
        Network ->
          if model.whosTurnNow == model.whensMyTurn then
            p [] [ text ("You are " ++ color ++ ". It is your turn") ]
          else
            p [] [ text ("You are " ++ color ++ ". Waiting for opponent") ]
        Local ->
          case model.whosTurnNow of
            1 -> p [] [ text "Red players turn" ]
            _ -> p [] [ text "Black players turn" ]
        AI ->
          p [] [ text "Not yet implemented" ]
    _ ->
      p [] [ text "Game not in progress" ]

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

pieceInfo : Piece -> Html Msg
pieceInfo (Piece _ pceType _ _) =
  case pceType of
    Advisor ->
      text """
        Advisor (仕/士): Moves and captures one point diagonally.
        May not leave the palace
        """
    Cannon ->
      text """
        Cannon (炮/砲): Moves any number orthogonally. Captures by 'firing'
        over an intervening piece of own or enemy colour
        """
    Chariot ->
      text """
        Chariot (俥/車): Moves and captures any number orthogonally
        """
    Elephant ->
      text """
        Elephant (相/象): Moves and captures two points diagonally.
        The Elephant does not jump. The Elephant cannot cross the river
        thus it serves mainly as a defensive piece
        """
    General ->
      text """
        General (帥/將): Moves and captures one point orthogonally.
        May not leave the palace. And may not face the opposing general
        otherwise they will perform the 'flying general' (飛將) move and
        cross the entire board to capture the enemy general
        """
    Horse ->
      text """
        Horse (傌/馬): Moves and captures in a two step move with one point
        moved orthogonally and then one point diagonally. The horse does not
        jump. Blocking a horse is known as 'Hobbling the horse's leg' (蹩馬腿)
        """
    Soldier ->
      text """
        Soldier (兵/卒): Moves and captures by advancing one point. Upon
        crossing the river can move and capture horizontally one point
        """

viewBoard : Model -> Html Msg
viewBoard model =
  div
    [ style "position" "absolute"
    , style "background-image" (imgUrl "board.jpg")
    , style "width" "521px"
    , style "height" "577px"
    , onClick DeselectPiece
    ]
    ((List.map (viewPiece model) model.board)
    ++ viewMoves model.board model.selected
    )

viewPiece : Model -> Piece -> Html Msg
viewPiece model (Piece color pceType file rank as pce) =
  let allowedSelection =
        case model.gameType of
          Network ->
            if model.whensMyTurn == model.whosTurnNow then
              case model.whosTurnNow of
                1 -> Just Red
                2 -> Just Black
                _ -> Nothing
            else
              Nothing
          Local ->
            case model.whosTurnNow of
              1 -> Just Red
              2 -> Just Black
              _ -> Nothing
          AI ->
            Nothing
  in
  let isSelected = model.selected == Just pce in
  div
    (((pos 57 file rank
    ++
    [ style "position" "absolute"
    , style "width" "57px"
    , style "height" "57px"
    , style "background-image" (imgUrl (icon color pceType))
    , style "cursor" "pointer"
    , style "border-radius" "100%" -- stops cursor looking wrong when off piece
    ]
    ++
    if pceType == General && isInCheck color model.board then
      [ style "border" "2px solid red" ]
    else
      []
    )
    ++
    if isSelected then
      [ style "border" "2px solid green" ]
    else
      []
    ) -- XXX extra parenthesis due to apparent bug in compiler argument order?
    ++
    if Just color == allowedSelection then
      [ stopPropOnClick (SelectPiece pce) ]
    else
      []
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
    , style "border-radius" "100%" -- fix cursor collision on corner
    , stopPropOnClick (MovePiece pce)
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
      List.map
        (\file_ -> Piece color pceType (addFile 'a' file_) rank)
        (List.range 0 8)
      ++
      List.map (\rank_ -> Piece color pceType file rank_) (List.range 1 10)
    Chariot ->
      List.map
        (\file_ -> Piece color pceType (addFile 'a' file_) rank)
        (List.range 0 8)
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
      [ case color of
          Red -> Piece color pceType file (rank + 1)
          Black -> Piece color pceType file (rank - 1)
      , Piece color pceType (addFile file 1) rank
      , Piece color pceType (subFile file 1) rank
      ]
    )
    |> List.filterMap pruneOffBoard
    |> List.filterMap prunePalace
    |> List.filterMap (pruneRiver pce)
    |> List.filterMap (pruneLandOnTeam board)
    |> List.filterMap (pruneBlocked board pce)
    |> addFlyingGeneral board pce

-- possibleMove
pruneOffBoard : Piece -> Maybe Piece
pruneOffBoard (Piece _ _ f_ r_ as p_) =
  if 0 <= toInt f_ && toInt f_ <= 8 && 1 <= r_ && r_ <= 10 then
    Just p_
  else
    Nothing

-- possibleMove
prunePalace : Piece -> Maybe Piece
prunePalace (Piece c_ pt_ f_ r_ as p_) =
  if c_ == Red && (pt_ == Advisor || pt_ == General) then
    if 3 <= toInt f_ && toInt f_ <= 5 && 1 <= r_ && r_ <= 3 then
      Just p_
    else
      Nothing
  else if c_ == Black && (pt_ == Advisor || pt_ == General) then
    if 3 <= toInt f_ && toInt f_ <= 5 && 8 <= r_ && r_ <= 10 then
      Just p_
    else
      Nothing
  else
    Just p_

-- originalPiece possibleMove
pruneRiver : Piece -> Piece -> Maybe Piece
pruneRiver (Piece c pt f r) (Piece _ _ f_ r_ as p_) =
  case (c, pt) of
    (Red, Soldier) ->
      if f == f_ || r > 5 then
        Just p_
      else
        Nothing
    (Black, Soldier) ->
      if f == f_ || r < 6 then
        Just p_
      else
        Nothing
    (Red, Elephant) ->
      if r_ <= 5 then
        Just p_
      else
        Nothing
    (Black, Elephant) ->
      if r_ >= 6 then
        Just p_
      else
        Nothing
    _ ->
      Just p_

-- board possibleMove
pruneLandOnTeam : List Piece -> Piece -> Maybe Piece
pruneLandOnTeam board (Piece c_ _ f_ r_ as p_) =
  if List.any (\(Piece c _ f r) -> c == c_ && f == f_ && r == r_) board then
    Nothing
  else
    Just p_

-- board originalPiece possibleMove
pruneBlocked : List Piece -> Piece -> Piece -> Maybe Piece
pruneBlocked board (Piece _ pt f r as p) (Piece _ _ f_ r_ as p_) =
  let (nf_, nr_) = (subFiles f f_, r - r_) in
  let normalBoard = List.map (\(Piece c__ pt__ f__ r__) ->
          Piece c__ pt__ (subFiles f f__) (r - r__)
        ) board
  in
  let sameDirection =
        normalBoard
          -- same file or rank as possibleMove
          |> List.filter (\(Piece _ _ nf nr) ->
                sameSign (toInt nf) (toInt nf_) && sameSign nr nr_
              )
          |> sortNearest
  in
  if pt == Cannon then
    case sameDirection of
      Piece _ _ nf nr :: Piece _ _ nf__ nr__ ::  _ ->
        if nf__ == nf_ && nr__ == nr_ then
          Just p_
        else if (abs (toInt nf_) < abs (toInt nf)) || abs nr_ < abs nr then
          Just p_
        else
          Nothing
      Piece _ _ nf nr :: _ ->
        if (abs (toInt nf_) < abs (toInt nf)) || abs nr_ < abs nr then
          Just p_
        else
          Nothing
      [] ->
        Just p_
  else if pt == Chariot then
    case sameDirection of
      Piece _ _ nf nr :: _ ->
        if (abs (toInt nf_) <= abs (toInt nf)) && abs nr_ <= abs nr then
          Just p_
        else
          Nothing
      [] ->
        Just p_
  else if pt == Elephant || pt == Horse then
      if List.any (\(Piece _ _ nf nr) ->
              nf == fromInt (toInt nf_ // 2) && nr == nr_ // 2
            )
            normalBoard
      then
        Nothing
      else
        Just p_
  else
    Just p_

-- XXX quick hack until proper check/checkmate code is in place
-- board originalPiece moves
addFlyingGeneral : List Piece -> Piece -> List Piece -> List Piece
addFlyingGeneral board (Piece c pt f r as p) mvs =
  let normalizedSameFile = board
        |> List.filter (\(Piece _ pt_ f_ _) -> pt_ /= General && f_ == f)
        |> List.map (\(Piece c_ pt_ f_ r_) -> Piece c_ pt_ f_ (r_ - r))
  in
  let opposingGeneral = List.head (List.filter
        (\(Piece c_ pt_ f_ _) -> c_ /= c && pt_ == General && f_ == f) board)
  in
  (case opposingGeneral of
    Just (Piece _ _ f_ r_) ->
      if not (List.any
        (\(Piece _ _ _ nr) -> abs nr < abs (r_ - r)) normalizedSameFile)
      then
        Piece c pt f_ r_ :: mvs
      else
        mvs
    _ ->
      mvs
  )

isInCheck : Color -> List Piece -> Bool
isInCheck clr board =
  let opposition = List.filter (\(Piece c _ _ _ as p) -> c /= clr) board in
  let generalList = List.filter
        (\(Piece c pt _ _ as p) -> c == clr && pt == General) board
  in
  case generalList of
    Piece c pt f r :: [] ->
      List.any
        (\p_ ->
            List.any
              (\(Piece _ _ f_ r_) -> f_ == f && r == r_)
              (moves board p_)
        )
        opposition
    _ -> False -- How did the general die?

sameSign : Int -> Int -> Bool
sameSign a b =
  ((a == 0) == (b == 0))
  && (a < 0) == (b < 0)

sortNearest : List Piece -> List Piece
sortNearest pces =
  List.sortWith (\(Piece _ _ f1 r1) (Piece _ _ f2 r2) ->
      compare (abs (toInt f1), (abs r1)) (abs (toInt f2), (abs r2))
    )
    pces

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

addFiles : Char -> Char -> Char
addFiles a b =
  fromInt ((toInt a) + (toInt b))

subFiles : Char -> Char -> Char
subFiles a b =
  fromInt ((toInt a) - (toInt b))

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
