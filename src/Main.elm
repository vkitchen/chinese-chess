module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, div, text)
import Html.Attributes exposing (style)
import String

type Color = Red | Black
type PieceType = Advisor | Cannon | Chariot | Elephant | General | Horse | Soldier
-- color piece position file rank
type Piece = Piece Color PieceType Char Int

-- Where is the game rooted in order to load assets
path = "/static/img/"

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model = List Piece

init : Model
init =
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

type Msg = NoOp

update : Msg -> Model -> Model
update msg model =
  model

view : Model -> Html Msg
view model =
  board model

board model =
  div
    [ style "position" "absolute"
    , style "background-image" (imgUrl "board.jpg")
    , style "width" "521px"
    , style "height" "577px"
    ]
    (List.map piece model)

piece (Piece color pce file rank) =
  div
    (pos file rank ++
    [ style "position" "absolute"
    , style "width" "57px"
    , style "height" "57px"
    , style "background-image" (imgUrl (icon color pce))
    ])
    []

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
  let file_ = Char.toCode file - Char.toCode 'a' in
  let rank_ = rank - 1 in
  [ style "top" (String.fromInt (rank_ * 58) ++ "px")
  , style "left" (String.fromInt (file_ * 58) ++ "px")
  ]

imgUrl : String -> String
imgUrl u =
  "url('" ++ path ++ u ++ "')"
