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
  | Move Piece

update : Msg -> Model -> Model
update msg ({board, selected} as model) =
  case msg of
    Select pce ->
      { board = board, selected = Just pce }
    Deselect ->
      { board = board, selected = Nothing }
    Move pce_ ->
      case selected of
        Just pce ->
          -- TODO insert back into dropped spot?
          -- mildly annoying redraw bug with dom moving around
          { board = List.filter (\p -> p /= pce) board ++ [pce_], selected = Nothing }
        Nothing ->
          { board = board, selected = selected } -- IMPOSSIBLE STATE

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
    ((List.map (viewPiece selected) board)
    ++ viewMoves board selected
    )

viewPiece : Maybe Piece -> Piece -> Html Msg
viewPiece selected (Piece color pceType file rank as pce) =
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
    , stopPropOnClick (Select pce)
    ]
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
  "url('" ++ path ++ u ++ "')"
