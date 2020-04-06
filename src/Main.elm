module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (stopPropagationOn, onClick)
import Json.Decode as Decode
import String

type Color = Red | Black
type PieceType = Advisor | Cannon | Chariot | Elephant | General | Horse | Soldier
-- color piece position file rank
type Piece = Piece Color PieceType Char Int

-- Where is the game rooted in order to load assets
path = "/static/img/"

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model =
  { board : List Piece
  , selected : Maybe Piece
  }

init : Model
init =
  { board = initialBoard, selected = Nothing }

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
  = Select Piece
  | Deselect

update : Msg -> Model -> Model
update msg ({board, selected} as model) =
  case msg of
    Select pce ->
      { board = board, selected = Just pce }
    Deselect ->
      { board = board, selected = Nothing }

stopPropOnClick : msg -> Attribute msg
stopPropOnClick msg =
  stopPropagationOn "click" (Decode.succeed (msg, True))

view : Model -> Html Msg
view model =
  viewBoard model

viewBoard : Model -> Html Msg
viewBoard { board, selected} =
  div
    [ style "position" "absolute"
    , style "background-image" (imgUrl "board.jpg")
    , style "width" "521px"
    , style "height" "577px"
    , onClick Deselect
    ]
    (List.map (piece selected) board)

piece : Maybe Piece -> Piece -> Html Msg
piece selected (Piece color pceType file rank as pce) =
  let isSelected = selected == Just pce in
  div
    (pos file rank
    ++
    [ style "position" "absolute"
    , style "width" "57px"
    , style "height" "57px"
    , style "background-image" (imgUrl (icon color pceType))
    , style "cursor" "pointer"
    , style "border-radius" "100%" -- stops cursor looking wrong when off piece
    , stopPropOnClick (Select pce)
    ]
    ++
    if isSelected then
      [ style "border-width" "2px"
      , style "border-style" "solid"
      , style "border-color" "red"
      ]
    else []
    )
    []

moves : Piece -> List Piece
moves (Piece color pce file rank) =
  case pce of
    Advisor ->
      [Piece color Advisor file rank]
    Cannon ->
      [Piece color Cannon file rank]
    Chariot ->
      [Piece color Cannon file rank]
    Elephant ->
      [Piece color Elephant file rank]
    General ->
      [Piece color General file rank]
    Horse ->
      [Piece color Horse file rank]
    Soldier ->
      [Piece color Soldier file rank]

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

-- First square starts 33px from left, and 32px from top
-- Squares are 58x58px including boarder, ie corners are 58px apart
-- Pieces are 57x57px
pos : Char -> Int -> List (Attribute msg)
pos file rank =
  let file_ = toInt file in
  let rank_ = 10 - rank in
  [ style "top" (String.fromInt (rank_ * 58) ++ "px")
  , style "left" (String.fromInt (file_ * 58) ++ "px")
  ]

toInt : Char -> Int
toInt c =
  Char.toCode c - Char.toCode 'a'

fromInt : Int -> Char
fromInt c =
  Char.fromCode (Char.toCode 'a' + c)

imgUrl : String -> String
imgUrl u =
  "url('" ++ path ++ u ++ "')"
