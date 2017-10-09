import Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
length2 = 30
dt = 0.010
m1 = 5.0
m2 = 5.0
num = 50
first1 = pi / 2
first2 = pi

type alias Model =
  { angle1 : List Float
  , angVel1 : List Float
  , angle2 : List Float
  , angVel2 : List Float
  , calculationMethod : CalculationMethod
  , isPaused : Bool
  , initialA1 : Float
  , initialA2 : Float
  , mass1 : Float
  , mass2 : Float
  , length1 : Float
  , length2 : Float
  }

type alias FourParameters =
  { angle1 : List Float
  , angVel1 : List Float
  , angle2 : List Float
  , angVel2 : List Float
  }

type alias Valuables =
  { mass1 : Float
  , mass2 : Float
  , length1 : Float
  , length2 : Float
  }

init : (Model, Cmd Msg)
init =
  ( { angle1 = List.map (\x -> x * 0.000000001 + first1) (iterate (\x -> x + 1) (List.repeat num 0))
    , angVel1 = List.repeat num 0
    , angle2 = List.map (\x -> x * 0.000000001 + first2) (iterate (\x -> x + 1) (List.repeat num 0))
    , angVel2 = List.repeat num 0
    , isPaused = True
    , calculationMethod = RungeKutta
    , initialA1 = first1
    , initialA2 = first2
    , mass1 = m1
    , mass2 = m2
    , length1 = length1
    , length2 = length2
    }
  , Cmd.none)

iterate : (Float -> Float) -> List Float -> List Float
iterate func list =
  case list of
    [] -> []
    _ -> [func (listHead list)] ++ (iterate func (List.map func <| listTail list))

type Msg = Tick Time
         | TogglePause
         | InputInitial1 String
         | InputInitial2 String
         | InputMass1 String
         | InputMass2 String
         | InputLength1 String
         | InputLength2 String
         | ResetButton
         | SwitchTo CalculationMethod

calc_a1 : Valuables -> Float -> Float -> Float -> Float -> Float
calc_a1 vals angle1 angVel1 angle2 angVel2 =
  let
    m1 = vals.mass1
    m2 = vals.mass2
    length1 = vals.length1
    length2 = vals.length2
  in
    (0 - (m1 + m2) * gravity * sin angle1 + m2 * gravity * (sin angle2) * cos (angle1 - angle2) - (length2 * angVel2 * angVel2 + length1 * angVel1 * angVel1 * cos (angle1 - angle2)) * m2 * sin (angle1 - angle2))
      / ((m1 + m2) * length1 - m2 * length1 * cos (angle1 - angle2) * cos (angle1 - angle2))

calc_a2 : Valuables -> Float -> Float -> Float -> Float -> Float
calc_a2 vals angle1 angVel1 angle2 angVel2 =
  let
    m1 = vals.mass1
    m2 = vals.mass2
    length1 = vals.length1
    length2 = vals.length2
  in
    ((m1 + m2) * gravity * sin angle2 - (m1 + m2) * gravity * (sin angle1) * cos (angle1 - angle2) - ((m1 + m2) * length1 * angVel1 * angVel1 + m2 * length2 * angVel2 * angVel2 * cos (angle1 - angle2)) * sin (angle1 - angle2))
      / (m2 * length2 * cos (angle1 - angle2) * cos (angle1 - angle2) - (m1 + m2) * length2)

calcToList : (Valuables -> Float -> Float -> Float -> Float -> Float) -> Valuables -> List Float -> List Float -> List Float -> List Float -> List Float
calcToList calc vals angle1 angVel1 angle2 angVel2 =
  case angle1 of
    [] -> []
    _ -> [(calc vals (listHead angle1) (listHead angVel1) (listHead angle2) (listHead angVel2))]++(calcToList calc vals (listTail angle1) (listTail angVel1) (listTail angle2) (listTail angVel2))

addList : List Float -> List Float -> List Float
addList list1 list2 =
  case list1 of
    [] -> []
    _ -> [(listHead list1) + (listHead list2)]++(addList (listTail list1) (listTail list2))

eulerV : (Valuables -> Float -> Float -> Float -> Float -> Float) -> Valuables -> List Float -> List Float -> List Float -> List Float -> List Float -> List Float
eulerV calc vals angle1 angVel1 angle2 angVel2 angVel =
  let
    angAcc = calcToList calc vals angle1 angVel1 angle2 angVel2
  in
    addList angVel (List.map ((*) dt) angAcc)

rungeKutta : (Valuables -> Float -> Float -> Float -> Float -> Float) -> (Valuables -> Float -> Float -> Float -> Float -> Float) -> Valuables -> List Float -> List Float -> List Float -> List Float -> FourParameters
rungeKutta calc_a1 calc_a2 vals angle1 angVel1 angle2 angVel2 =
  let
    k1_a1 = angVel1
    k1_v1 = calcToList calc_a1 vals angle1 angVel1 angle2 angVel2
    k1_a2 = angVel2
    k1_v2 = calcToList calc_a2 vals angle1 angVel1 angle2 angVel2
    k2_a1 =
      (addList angVel1 <| List.map (\x -> 0.5 * dt * x) k1_v1)
    k2_v1 =
      calcToList calc_a1 vals
        (addList angle1 <| List.map (\x -> 0.5 * dt * x) k1_a1)
        (addList angVel1 <| List.map (\x -> 0.5 * dt * x) k1_v1)
        (addList angle2 <| List.map (\x -> 0.5 * dt * x) k1_a2)
        (addList angVel2 <| List.map (\x -> 0.5 * dt * x) k1_v2)
    k2_a2 =
      (addList angVel2 <| List.map (\x -> 0.5 * dt * x) k1_v2)
    k2_v2 =
      calcToList calc_a2 vals
        (addList angle1 <| List.map (\x -> 0.5 * dt * x) k1_a1)
        (addList angVel1 <| List.map (\x -> 0.5 * dt * x) k1_v1)
        (addList angle2 <| List.map (\x -> 0.5 * dt * x) k1_a2)
        (addList angVel2 <| List.map (\x -> 0.5 * dt * x) k1_v2)
    k3_a1 =
      (addList angVel1 <| List.map (\x -> 0.5 * dt * x) k2_v1)
    k3_v1 =
      calcToList calc_a1 vals
        (addList angle1 <| List.map (\x -> 0.5 * dt * x) k2_a1)
        (addList angVel1 <| List.map (\x -> 0.5 * dt * x) k2_v1)
        (addList angle2 <| List.map (\x -> 0.5 * dt * x) k2_a2)
        (addList angVel2 <| List.map (\x -> 0.5 * dt * x) k2_v2)
    k3_a2 =
      (addList angVel2 <| List.map (\x -> 0.5 * dt * x) k2_v2)
    k3_v2 =
      calcToList calc_a2 vals
        (addList angle1 <| List.map (\x -> 0.5 * dt * x) k2_a1)
        (addList angVel1 <| List.map (\x -> 0.5 * dt * x) k2_v1)
        (addList angle2 <| List.map (\x -> 0.5 * dt * x) k2_a2)
        (addList angVel2 <| List.map (\x -> 0.5 * dt * x) k2_v2)
    k4_a1 =
      (addList angVel1 <| List.map (\x -> dt * x) k3_v1)
    k4_v1 =
      calcToList calc_a1 vals
        (addList angle1 <| List.map (\x -> dt * x) k3_a1)
        (addList angVel1 <| List.map (\x -> dt * x) k3_v1)
        (addList angle2 <| List.map (\x -> dt * x) k3_a2)
        (addList angVel2 <| List.map (\x -> dt * x) k3_v2)
    k4_a2 =
      (addList angVel2 <| List.map (\x -> dt * x) k3_v2)
    k4_v2 =
      calcToList calc_a2 vals
        (addList angle1 <| List.map (\x -> dt * x) k3_a1)
        (addList angVel1 <| List.map (\x -> dt * x) k3_v1)
        (addList angle2 <| List.map (\x -> dt * x) k3_a2)
        (addList angVel2 <| List.map (\x -> dt * x) k3_v2)

    angVel1_ = addList angVel1 <| List.map (\x -> dt * x / 6) <| List.foldr addList k1_v1 [k2_v1, k2_v1, k3_v1, k3_v1, k4_v1]
    angle1_ = addList angle1 <| List.map (\x -> dt * x / 6) <| List.foldr addList k1_a1 [k2_a1, k2_a1, k3_a1, k3_a1, k4_a1]
    angVel2_ = addList angVel2 <| List.map (\x -> dt * x / 6) <| List.foldr addList k1_v2 [k2_v2, k2_v2, k3_v2, k3_v2, k4_v2]
    angle2_ = addList angle2 <| List.map (\x -> dt * x / 6) <| List.foldr addList k1_a2 [k2_a2, k2_a2, k3_a2, k3_a2, k4_a2]
  in
    { angVel1 = angVel1_
    , angle1 = angle1_
    , angVel2 = angVel2_
    , angle2 = angle2_
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time ->
      case model.calculationMethod of
        RungeKutta ->
          -- Runge Kutta
          let
            vals = { mass1 = model.mass1, mass2 = model.mass2, length1 = model.length1, length2 = model.length2 }
            fourParams = rungeKutta calc_a1 calc_a2 vals model.angle1 model.angVel1 model.angle2 model.angVel2
          in
            ( { model
                | angVel1 = fourParams.angVel1
                , angle1 = fourParams.angle1
                , angVel2 = fourParams.angVel2
                , angle2 = fourParams.angle2
              }
            , Cmd.none)

        Euler ->
          -- Euler
          let
            vals = { mass1 = model.mass1, mass2 = model.mass2, length1 = model.length1, length2 = model.length2 }
            angVel1_ = eulerV calc_a1 vals model.angle1 model.angVel1 model.angle2 model.angVel2 model.angVel1
            angle1_ = addList model.angle1 (List.map ((*) dt) angVel1_)
            angVel2_ = eulerV calc_a2 vals model.angle1 model.angVel1 model.angle2 model.angVel2 model.angVel2
            angle2_ = addList model.angle2 (List.map ((*) dt) angVel2_)
          in
            ( { model
                | angle1 = angle1_
                , angVel1 = angVel1_
                , angle2 = angle2_
                , angVel2 = angVel2_
              }
            , Cmd.none)

    TogglePause ->
      ( { model | isPaused = not model.isPaused } , Cmd.none)

    InputInitial1 initial ->
      case (String.toFloat initial) of
        Ok ini ->
          ( { model
            | initialA1 = ini
            , angle1 = List.map (\x -> x * 0.000000001 + ini) (iterate (\x -> x + 1) (List.repeat num 0))
            }
          , Cmd.none )
        Err _ ->
          ( model , Cmd.none )

    InputInitial2 initial ->
      case (String.toFloat initial) of
        Ok ini ->
          ( { model
            | initialA2 = ini
            , angle2 = List.map (\x -> x * 0.000000001 + ini) (iterate (\x -> x + 1) (List.repeat num 0))
            }
          , Cmd.none )
        Err _ ->
          ( model , Cmd.none )

    InputMass1 mass ->
      case (String.toFloat mass) of
        Ok mass ->
          ( { model | mass1 = mass }, Cmd.none )
        Err _ ->
          ( model , Cmd.none )

    InputMass2 mass ->
      case (String.toFloat mass) of
        Ok mass ->
          ( { model | mass2 = mass }, Cmd.none )
        Err _ ->
          ( model , Cmd.none )

    InputLength1 l1 ->
      case (String.toFloat l1) of
        Ok l1 ->
          ( { model | length1 = l1 }, Cmd.none )
        Err _ ->
          ( model , Cmd.none )

    InputLength2 l2 ->
      case (String.toFloat l2) of
        Ok l2 ->
          ( { model | length2 = l2 }, Cmd.none )
        Err _ ->
          ( model , Cmd.none )

    SwitchTo method ->
      ( { model | calculationMethod = method } , Cmd.none)

    ResetButton ->
      ( { model
        | angle1 = List.map (\x -> x * 0.000000001 + first1) (iterate (\x -> x + 1) (List.repeat num 0))
        , angVel1 = List.repeat num 0
        , angle2 = List.map (\x -> x * 0.000000001 + first2) (iterate (\x -> x + 1) (List.repeat num 0))
        , angVel2 = List.repeat num 0
        , calculationMethod = RungeKutta
        , isPaused = True
        , initialA1 = first1
        , initialA2 = first2
        }
      , Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.isPaused of
    -- False -> Time.every (dt * second) Tick
    False -> AnimationFrame.diffs Tick
    True -> Sub.none

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
  [ Html.div
      [ Html.Attributes.style [("float", "left")]
      ]
      [ Svg.svg
          [ viewBox "0 0 300 300", Svg.Attributes.width "900px"]
          ( let
              vals = { mass1 = model.mass1, mass2 = model.mass2, length1 = model.length1, length2 = model.length2 }
            in
              (createPendulums vals model.angle1 model.angle2)
          )
      ]
  , Html.fieldset
      [ Html.Attributes.style [("float", "left"), ("width", "250px")]
      ]
      [ Html.legend [] [ Html.text "Manupulations" ]
      , Html.table [ Html.Attributes.style [("padding-left", "20px"), ("padding-right", "20px")] ]
        [ rangeBarInitial InputInitial1 model.initialA1 "Angle 1 "
        , rangeBarInitial InputInitial2 model.initialA2 "Angle 2 "
        , rangeBarMass InputMass1 model.mass1 "Mass 1 "
        , rangeBarMass InputMass2 model.mass2 "Mass 2 "
        , rangeBarLength InputLength1 model.length1 "Length 1 "
        , rangeBarLength InputLength2 model.length2 "Length 2 "
        ]
      , methodPicker "calcMethod" model.calculationMethod
      , Html.br [] []
      , pause TogglePause model.isPaused
      , Html.div [ Html.Attributes.style [("width", "80px"), ("margin-left", "auto"), ("margin-right", "auto")] ]
        [ Html.button [ onClick ResetButton, Html.Attributes.style [("width", "80px")] ] [ Html.text "Reset" ]
        ]
      ]
  -- , Html.div [] [ Html.text (toString <| totalEnergy (listHead model.angle1) (listHead model.angVel1) (listHead model.angle2) (listHead model.angVel2))]
  -- , Html.div [] [ Html.text (toString <| kineticEnergy1 (listHead model.angle1) (listHead model.angVel1) (listHead model.angle2) (listHead model.angVel2))]
  -- , Html.div [] [ Html.text (toString <| kineticEnergy2 (listHead model.angle1) (listHead model.angVel1) (listHead model.angle2) (listHead model.angVel2))]
  -- , Html.div [] [ Html.text (toString <| potentialEnergy1 (listHead model.angle1) (listHead model.angVel1) (listHead model.angle2) (listHead model.angVel2))]
  -- , Html.div [] [ Html.text (toString <| potentialEnergy2 (listHead model.angle1) (listHead model.angVel1) (listHead model.angle2) (listHead model.angVel2))]
  ]

createPendulums : Valuables -> List Float -> List Float -> List (Svg.Svg Msg)
createPendulums vals angle1 angle2 =
  case angle1 of
    [] -> []
    _ -> List.append (dualPendulum vals (listHead angle1) (listHead angle2)) (createPendulums vals (listTail angle1) (listTail angle2))

dualPendulum : Valuables -> Float -> Float -> List (Svg.Svg Msg)
dualPendulum vals angle1 angle2 =
  let
    ball1_x = toString <| 150 + vals.length1 * sin angle1
    ball1_y = toString <| 150 - vals.length1 * cos angle1
    ball2_x = toString <| 150 + vals.length1 * sin angle1 + vals.length2 * sin angle2
    ball2_y = toString <| 150 - vals.length1 * cos angle1 - vals.length2 * cos angle2
    rad1 = toString <| 5 * sqrt vals.mass1
    rad2 = toString <| 5 * sqrt vals.mass2
  in
    [ Svg.line [ x1 "150", y1 "150", x2 ball1_x, y2 ball1_y , stroke "#f06300", opacity "0.25"] []
    , Svg.line [ x1 ball1_x, y1 ball1_y , x2 ball2_x, y2 ball2_y , stroke "#f06300", opacity "0.25"] []
    , Svg.circle [ cx "150", cy "150", r "5", fill "#666666"] []
    , Svg.circle [ cx ball1_x, cy ball1_y, r rad1, fill "#0B79CE", opacity "0.25"] []
    , Svg.circle [ cx ball2_x, cy ball2_y, r rad2, fill "#0B79CE", opacity "0.25"] []
    ]

totalEnergy : Float -> Float -> Float -> Float -> Float
totalEnergy angle1 angVel1 angle2 angVel2 =
  0.5 * m1 * length1 * length1 * angVel1 * angVel1 + 0.5 * m2 * (length1 * length1 * angVel1 * angVel1 + length2 * length2 * angVel2 * angVel2 + 2 * length1 * length2 * angVel1 * angVel2 * cos (angle1 - angle2)) + m1 * gravity * length1 * (0 - cos angle1) + m2 * gravity * (length1 * (0 - cos angle1) + length2 * (0 - cos angle2))

kineticEnergy1 : Float -> Float -> Float -> Float -> Float
kineticEnergy1 angle1 angVel1 angle2 angVel2 =
  0.5 * m1 * length1 * length1 * angVel1 * angVel1

kineticEnergy2 : Float -> Float -> Float -> Float -> Float
kineticEnergy2 angle1 angVel1 angle2 angVel2 =
  0.5 * m2 * (length1 * length1 * angVel1 * angVel1 + length2 * length2 * angVel2 * angVel2 + 2 * length1 * length2 * angVel1 * angVel2 * cos (angle1 - angle2))

potentialEnergy1 : Float -> Float -> Float -> Float -> Float
potentialEnergy1 angle1 angVel1 angle2 angVel2 =
  m1 * gravity * length1 * (0 - cos angle1)

potentialEnergy2 : Float -> Float -> Float -> Float -> Float
potentialEnergy2 angle1 angVel1 angle2 angVel2 =
  m2 * gravity * (length1 * (0 - cos angle1) + length2 * (0 - cos angle2))

checkbox : msg -> String -> Bool -> Html.Html msg
checkbox msg name isTrue =
  Html.label
    [ Html.Attributes.style [("padding-left", "20px"), ("padding-right", "20px")]
    ]
    [ Html.input [ Html.Attributes.type_ "checkbox", onClick msg, Html.Attributes.checked isTrue ] []
    , Html.text name
    , Html.br [] []
    ]

type CalculationMethod = RungeKutta | Euler

methodPicker : String -> CalculationMethod -> Html.Html Msg
methodPicker name method =
  case method of
    RungeKutta ->
      Html.label [ Html.Attributes.style [("padding-left", "20px"), ("padding-right", "20px")] ]
        [ radio (SwitchTo RungeKutta) "Runge-Kutta" True name
        , radio (SwitchTo Euler) "Euler" False name
        ]
    Euler ->
      Html.label [ Html.Attributes.style [("padding-left", "20px"), ("padding-right", "20px")] ]
        [ radio (SwitchTo RungeKutta) "Runge-Kutta" False name
        , radio (SwitchTo Euler) "Euler" True name
        ]

radio : msg -> String -> Bool -> String -> Html.Html msg
radio msg value bool name =
  Html.label []
    [ Html.input [ Html.Attributes.type_ "radio", Html.Attributes.name name, onClick msg, Html.Attributes.checked bool ] []
    , Html.text value
    ]

pause : msg -> Bool -> Html.Html msg
pause msg isPaused =
  Html.div [ Html.Attributes.style [("width", "80px"), ("margin-left", "auto"), ("margin-right", "auto")] ]
    [ Html.button [ onClick msg,
        Html.Attributes.style
        [ ("font-size", "1.4em")
        , ("border-radius", "5px")
        , ("color", "#fff")
        , ("background-color", "#248")
        , ("border-style", "none")
        , ("width", "80px")
        ]
      ]
      ( case isPaused of
          True -> [ Html.text "Start" ]
          False -> [ Html.text "Pause" ]
      )
    ]

rangeBarInitial : (String -> msg) -> Float -> String -> Html.Html msg
rangeBarInitial msg number name =
  Html.tr
    [ Html.Attributes.style [("padding-left", "20px"), ("padding-right", "20px")]
    ]
    [ Html.td [] [ Html.text name ]
    , Html.td [] [ Html.input
        [ Html.Attributes.type_ "range"
        , onInput msg
        , Html.Attributes.value (toString number)
        , Html.Attributes.max (toString pi)
        , Html.Attributes.min (toString <| (-1) * pi)
        , Html.Attributes.step (toString <| 0.01 * pi)
        ] []
      ]
    -- , Html.text (toString number)
    -- , Html.br [] []
    ]

rangeBarMass : (String -> msg) -> Float -> String -> Html.Html msg
rangeBarMass msg number name =
  Html.tr
    [ Html.Attributes.style [("padding-left", "20px"), ("padding-right", "20px")]
    ]
    [ Html.td [] [ Html.text name ]
    , Html.td [] [ Html.input
      [ Html.Attributes.type_ "range"
      , onInput msg
      , Html.Attributes.value (toString number)
      , Html.Attributes.max (toString 10)
      , Html.Attributes.min (toString 0.5)
      , Html.Attributes.step (toString 0.1)
      ] []
    ]
    -- , Html.text (toString number)
    -- , Html.br [] []
    ]

rangeBarLength : (String -> msg) -> Float -> String -> Html.Html msg
rangeBarLength msg number name =
  Html.tr
    [ Html.Attributes.style [("padding-left", "20px"), ("padding-right", "20px")]
    ]
    [ Html.td [] [ Html.text name ]
    , Html.td [] [ Html.input
      [ Html.Attributes.type_ "range"
      , onInput msg
      , Html.Attributes.value (toString number)
      , Html.Attributes.max (toString 50)
      , Html.Attributes.min (toString 1)
      , Html.Attributes.step (toString 1)
      ] []
    ]
    -- , Html.text (toString number)
    -- , Html.br [] []
    ]
