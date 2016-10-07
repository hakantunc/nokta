import Html.App as App
import Html exposing (..)
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
type alias Model = List (Point,Int)

size = 7
space = 50
states = List.repeat size Blank
positions = List.scanl (\_ acc->acc+space) space (List.repeat (size-1) 0)
model : Model
model = List.map2 (,) states positions

-- UPDATE

type Msg = Name String

update : Msg -> Model -> Model
update msg model = model

-- VIEW

view : Model -> Html Msg
view model =
  div [] [ board ]

board = Svg.svg
       [ width "480", height "480", viewBox "0 0 480 480" ]
       (List.map cc model)

cc (stt,pos) =
  let 
    clr = case stt of
    Blank -> "gray"
    White -> "yellow"
    Black -> "black"
  in circle [ fill clr, stroke "black", cx (toString pos), cy "60", r "20" ] []
