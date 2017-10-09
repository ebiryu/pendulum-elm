import Html
import Svg
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import AnimationFrame

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

gravity = -500
length1 = 40
length2 = 60
dt = 0.0020
m1 = 10.0
m2 = 1.0
num = 50
first1 = pi * 3 / 6
first2 = pi

type alias Model =
  { angle1 : List Float
  , angVel1 : List Float
  , angle2 : List Float
  , angVel2 : List Float
  }

init : (Model, Cmd Msg)
init =
  ( { angle1 = List.map (\x -> x * 0.000000001 + first1) (iterate (\x -> x + 1) (List.repeat num 0))
    , angVel1 = List.repeat num 0
    , angle2 = List.map (\x -> x * 0.000000001 + first2) (iterate (\x -> x + 1) (List.repeat num 0))
    , angVel2 = List.repeat num 0
    }
  , Cmd.none)

iterate : (Float -> Float) -> List Float -> List Float
iterate func list =
  case list of
    [] -> []
    _ -> [func (listHead list)] ++ (iterate func (List.map func <| listTail list))

type Msg =
  Tick Time

calc_a1 : Float -> Float -> Float -> Float -> Float
calc_a1 angle1 angVel1 angle2 angVel2 =
  (0 - (m1 + m2) * gravity * sin angle1 + m2 * gravity * sin angle2 * cos (angle1 - angle2) - (length2 * angVel1 * angVel1 + length1 * angVel2 * angVel2 * cos (angle1 - angle2)) * m2 * sin (angle1 - angle2)) / ((m1 + m2) * length1 - m2 * length1 * cos (angle1 - angle2) * cos (angle1 - angle2))

calc_a2 : Float -> Float -> Float -> Float -> Float
calc_a2 angle1 angVel1 angle2 angVel2 =
  ((m1 + m2) * gravity * sin angle2 - (m1 + m2) * gravity * sin angle1 * cos (angle1 - angle2) - ((m1 + m2) * length1 * angVel1 * angVel1 + m2 * length2 * angVel2 * angVel2 * cos (angle1 - angle2)) * sin (angle1 - angle2)) / (m2 * length2 * cos (angle1 - angle2) * cos (angle1 - angle2) - (m1 + m2) * length2)

calcToList : (Float -> Float -> Float -> Float -> Float) -> List Float -> List Float -> List Float -> List Float -> List Float
calcToList calc angle1 angVel1 angle2 angVel2 =
  case angle1 of
    [] -> []
    _ -> [(calc (listHead angle1) (listHead angVel1) (listHead angle2) (listHead angVel2))]++(calcToList calc (listTail angle1) (listTail angVel1) (listTail angle2) (listTail angVel2))

addList : List Float -> List Float -> List Float
addList list1 list2 =
  case list1 of
    [] -> []
    _ -> [(listHead list1) + (listHead list2)]++(addList (listTail list1) (listTail list2))

eulerV : (Float -> Float -> Float -> Float -> Float) -> List Float -> List Float -> List Float -> List Float -> List Float -> List Float
eulerV calc angle1 angVel1 angle2 angVel2 angVel =
  let
    angAcc = calcToList calc angle1 angVel1 angle2 angVel2
  in
    addList angVel (List.map ((*) dt) angAcc)

-- rungeKuttaV : (Float -> Float -> Float -> Float -> Float) -> List Float -> List Float -> List Float -> List Float -> List Float  -> List Float
-- rungeKuttaV calc angle1 angVel1 angle2 angVel2 angVel =
--   let
--     k1_v1 = calcToList calc angle1 angVel1 angle2 angVel2
--     k2 = List.map ((*) 2) <| calcToList calc angle1 (addList angVel1 <| List.map (\x -> 0.5 * dt * x) k1) angle2 (addList angVel2 <| List.map (\x -> 0.5 * dt * x) k1)
--     k3 = List.map ((*) 2) <| calcToList calc angle1 (addList angVel1 <| List.map (\x -> 0.5 * dt * x) k2) angle2 (addList angVel2 <| List.map (\x -> 0.5 * dt * x) k2)
--     k4 = calcToList calc angle1 (addList angVel1 <| List.map (\x -> dt * x) k3) angle2 (addList angVel2 <| List.map (\x -> dt * x) k3)
--   in
--     addList angVel (List.map ((*) (dt / 6)) (List.foldl addList k1 [k2, k3, k4]))
--
-- rungeKuttaA : (Float -> Float -> Float -> Float -> Float) -> List Float -> List Float -> List Float -> List Float -> List Float  -> List Float
-- rungeKuttaA calc angle1 angVel1 angle2 angVel2 angle =
--   let
--     k1 = calcToList calc angle1 angVel1 angle2 angVel2
--     k2 = List.map ((*) 2) <| calcToList calc (addList angle1 <| List.map (\x -> 0.5 * dt * x) k1) angVel1 (addList angle2 <| List.map (\x -> 0.5 * dt * x) k1) angVel2
--     k3 = List.map ((*) 2) <| calcToList calc (addList angle1 <| List.map (\x -> 0.5 * dt * x) k2) angVel1 (addList angle2 <| List.map (\x -> 0.5 * dt * x) k2) angVel2
--     k4 = calcToList calc (addList angle1 <| List.map (\x -> dt * x) k3) angVel1 (addList angle2 <| List.map (\x -> dt * x) k3) angVel2
--   in
--     addList angle (List.map ((*) (dt / 6)) (List.foldl addList k1 [k2, k3, k4]))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  -- case msg of
    -- Tick time ->
      -- Runge Kutta
      let
        angle1 = model.angle1
        angVel1 = model.angVel1
        angle2 = model.angle2
        angVel2 = model.angVel2
        k1_a1 = angVel1
        k1_v1 = calcToList calc_a1 angle1 angVel1 angle2 angVel2
        k1_a2 = angVel2
        k1_v2 = calcToList calc_a2 angle1 angVel1 angle2 angVel2
        k2_a1 = List.map ((*) 2) <| (addList angVel1 <| List.map (\x -> 0.5 * dt * x) k1_v1)
        k2_v1 = List.map ((*) 2) <| calcToList calc_a1 (addList angle1 <| List.map (\x -> 0.5 * dt * x) k1_a1) (addList angVel1 <| List.map (\x -> 0.5 * dt * x) k1_v1) (addList angle2 <| List.map (\x -> 0.5 * dt * x) k1_a2) (addList angVel2 <| List.map (\x -> 0.5 * dt * x) k1_v2)
        k2_a2 = List.map ((*) 2) <| (addList angVel2 <| List.map (\x -> 0.5 * dt * x) k1_v2)
        k2_v2 = List.map ((*) 2) <| calcToList calc_a2 (addList angle1 <| List.map (\x -> 0.5 * dt * x) k1_a1) (addList angVel1 <| List.map (\x -> 0.5 * dt * x) k1_v1) (addList angle2 <| List.map (\x -> 0.5 * dt * x) k1_a2) (addList angVel2 <| List.map (\x -> 0.5 * dt * x) k1_v2)
        k3_a1 = List.map ((*) 2) <| (addList angVel1 <| List.map (\x -> 0.5 * dt * x) k2_v1)
        k3_v1 = List.map ((*) 2) <| calcToList calc_a1 (addList angle1 <| List.map (\x -> 0.5 * dt * x) k2_a1) (addList angVel1 <| List.map (\x -> 0.5 * dt * x) k2_v1) (addList angle2 <| List.map (\x -> 0.5 * dt * x) k2_a2) (addList angVel2 <| List.map (\x -> 0.5 * dt * x) k2_v2)
        k3_a2 = List.map ((*) 2) <| (addList angVel2 <| List.map (\x -> 0.5 * dt * x) k2_v2)
        k3_v2 = List.map ((*) 2) <| calcToList calc_a2 (addList angle1 <| List.map (\x -> 0.5 * dt * x) k2_a1) (addList angVel1 <| List.map (\x -> 0.5 * dt * x) k2_v1) (addList angle2 <| List.map (\x -> 0.5 * dt * x) k2_a2) (addList angVel2 <| List.map (\x -> 0.5 * dt * x) k2_v2)
        k4_a1 = (addList angVel1 <| List.map (\x -> dt * x) k3_v1)
        k4_v1 = calcToList calc_a1 (addList angle1 <| List.map (\x -> dt * x) k3_a1) (addList angVel1 <| List.map (\x -> dt * x) k3_v1) (addList angle2 <| List.map (\x -> dt * x) k3_a2) (addList angVel2 <| List.map (\x -> dt * x) k3_v2)
        k4_a2 = (addList angVel2 <| List.map (\x -> dt * x) k3_v2)
        k4_v2 = calcToList calc_a2 (addList angle1 <| List.map (\x -> dt * x) k3_a1) (addList angVel1 <| List.map (\x -> dt * x) k3_v1) (addList angle2 <| List.map (\x -> dt * x) k3_a2) (addList angVel2 <| List.map (\x -> dt * x) k3_v2)

        angVel1_ = addList angVel1 <| List.map (\x -> dt * x / 6) <| List.foldl addList k1_v1 [k2_v1, k3_v1, k4_v1]
        angle1_ = addList angle1 <| List.map (\x -> dt * x / 6) <| List.foldl addList k1_a1 [k2_a1, k3_a1, k4_a1]
        -- angle1_ = addList model.angle1 (List.map ((*) dt) angVel1_)
        angVel2_ = addList angVel2 <| List.map (\x -> dt * x / 6) <| List.foldl addList k1_v2 [k2_v2, k3_v2, k4_v2]
        angle2_ = addList angle2 <| List.map (\x -> dt * x / 6) <| List.foldl addList k1_a2 [k2_a2, k3_a2, k4_a2]
        -- angle2_ = addList model.angle2 (List.map ((*) dt) angVel2_)
      in
        ( { model
            | angle1 = angle1_
            , angVel1 = angVel1_
            , angle2 = angle2_
            , angVel2 = angVel2_
          }
        , Cmd.none)

subscriptions : Model -> Sub Msg
-- subscriptions _ = AnimationFrame.diffs Tick
subscriptions _ = Time.every (dt * second) Tick

listHead : List Float -> Float
listHead list =
  case List.head list of
    Nothing -> 0
    Just a -> a

listHeadSvg : List (Svg.Svg Msg) -> Svg.Svg Msg
listHeadSvg list =
  case List.head list of
    Nothing -> Svg.circle [ cx "0", cy "0", r "0" ] []
    Just a -> a

listTail : List a -> List a
listTail list =
  case List.tail list of
    Nothing -> []
    Just a -> a

view : Model -> Html.Html Msg
view model =
  Html.div []
  [ Svg.svg
      [ viewBox "0 0 300 300", width "900px"]
      (createPendulums model.angle1 model.angle2)
  ]

-- appendSvgList : Svg.Svg Msg -> List (Svg.Svg Msg) -> List (Svg.Svg Msg)
-- appendSvgList svg list = (appendSvgList (listHeadSvg list) (listTail list))

createPendulums : List Float -> List Float -> List (Svg.Svg Msg)
createPendulums angle1 angle2 =
  case angle1 of
    [] -> []
    _ -> List.append (dualPendulum (listHead angle1) (listHead angle2)) (createPendulums (listTail angle1) (listTail angle2))

dualPendulum : Float -> Float -> List (Svg.Svg Msg)
dualPendulum angle1 angle2 =
  let
    ball1_x = toString <| 150 + length1 * sin angle1
    ball1_y = toString <| 150 - length1 * cos angle1
    ball2_x = toString <| 150 + length1 * sin angle1 + length2 * sin angle2
    ball2_y = toString <| 150 - length1 * cos angle1 - length2 * cos angle2
    rad1 = toString <| 5 * sqrt m1
    rad2 = toString <| 5 * sqrt m2
  in
    [ Svg.line [ x1 "150", y1 "150", x2 ball1_x, y2 ball1_y , stroke "#f06300", opacity "0.25"] []
    , Svg.line [ x1 ball1_x, y1 ball1_y , x2 ball2_x, y2 ball2_y , stroke "#f06300", opacity "0.25"] []
    , Svg.circle [ cx "150", cy "150", r "5", fill "#666666"] []
    , Svg.circle [ cx ball1_x, cy ball1_y, r rad1, fill "#0B79CE", opacity "0.25"] []
    , Svg.circle [ cx ball2_x, cy ball2_y, r rad2, fill "#0B79CE", opacity "0.25"] []
    ]
