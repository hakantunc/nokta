import Html.App as App
import Html exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)

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
  { board : List (Point,Int)
  , turn : Player
  }

size = 11
radius = 20
space = radius*3
boardwidth = (size+1) * space
lengthstr = toString boardwidth
states = List.repeat size Blank
positions = List.scanl (\_ acc->acc+space) space (List.repeat (size-1) 0)
model : Model
model = {board = List.map2 (,) states positions, turn = WhitePlayer}

-- UPDATE

type Msg = Update Int

update : Msg -> Model -> Model
update msg model = model

-- VIEW

view : Model -> Html Msg
view model =
  div [] [ div [] [Html.text ("Whose turn?¿: " ++ (toString model.turn))]
         , board model.board
         ]

board brd = Svg.svg
       [ width lengthstr, height lengthstr, viewBox ("0 0 " ++ lengthstr ++ " " ++ lengthstr) ]
       (List.map cc brd)

cc (stt,pos) =
  let 
    clr = case stt of
    Blank -> "gray"
    White -> "yellow"
    Black -> "black"
  in circle [ onClick (Update pos), fill clr, stroke "black", cx (toString pos), cy "60", r (toString radius) ] []
