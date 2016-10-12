import Html.App as App
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Svg exposing (svg, circle)
import Svg.Attributes exposing (width, height, viewBox, fill, stroke, cx, cy, r)
import Array exposing (..)

main =
  App.beginnerProgram
    { model = model
    , view = view
    , update = update
    }


-- MODEL

type Point = Blank | White | Black
type Player = WhitePlayer | BlackPlayer
type alias Model =
  { board : Array (Array (Int, Int, Point))
  , turn : Player
  }

size = 7
row i = initialize size (\j -> (i, j, Blank))
board = initialize size (\i -> row i)
model : Model
model = {board = board, turn = WhitePlayer}


-- UPDATE

type Msg = Update Int Int

update : Msg -> Model -> Model
update (Update x y) model =
  let
    clr = if model.turn == WhitePlayer then White else Black
    nextTurn = if model.turn == WhitePlayer then BlackPlayer else WhitePlayer
  in
    { board = set2 x y (x,y,clr) model.board
    , turn = nextTurn
    }

set2 x y val board =
  case (get x board) of
    Nothing  -> board
    Just row -> set x (set y val row) board


-- VIEW

radius = 20
space = radius*3
boardwidth = (size+1) * space
lengthstr = toString boardwidth
rr = toString radius

view : Model -> Html Msg
view model =
  div [] [ div [] [text ("Whose turn?¿: " ++ (toString model.turn))]
         , boardSvgArray model.board
         ]

boardSvgArray brd =
  Svg.svg
    [ width lengthstr, height lengthstr, viewBox ("0 0 " ++ lengthstr ++ " " ++ lengthstr) ]
    (List.concat (toList (map f brd)))

f row = toList (map g row)
g (x,y,stt) =
  let 
    xx = toString (space * (x+1))
    yy = toString (space * (y+1))
    clr = case stt of
      Blank -> "gray"
      White -> "yellow"
      Black -> "black"
  in circle [ onClick (Update x y), fill clr, stroke "black", cx xx, cy yy, r rr ] []
