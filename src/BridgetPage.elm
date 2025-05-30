module BridgetPage exposing (main)

import Angle
import Block3d
import Browser
import Browser.Events
import Camera3d
import Color
import Direction3d
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode
import Length
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material
import Viewpoint3d

-- MODEL
type alias Model =
    { azimuth : Float  -- Horizontal rotation around the board
    , elevation : Float  -- Vertical angle
    , isMouseDown : Bool
    , lastMousePos : Maybe (Float, Float)
    }

type Msg
    = MouseDown Float Float
    | MouseUp
    | MouseMove Float Float
    | NoOp

-- INIT
init : () -> (Model, Cmd Msg)
init _ =
    ( { azimuth = 45  -- degrees
      , elevation = 30  -- degrees  
      , isMouseDown = False
      , lastMousePos = Nothing
      }
    , Cmd.none
    )

-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MouseDown x y ->
            ( { model 
              | isMouseDown = True
              , lastMousePos = Just (x, y)
              }
            , Cmd.none
            )
            
        MouseUp ->
            ( { model 
              | isMouseDown = False
              , lastMousePos = Nothing
              }
            , Cmd.none
            )
            
        MouseMove x y ->
            case (model.isMouseDown, model.lastMousePos) of
                (True, Just (lastX, lastY)) ->
                    let
                        deltaX = x - lastX
                        deltaY = y - lastY
                        sensitivity = 0.5
                        newAzimuth = model.azimuth + deltaX * sensitivity
                        newElevation = 
                            model.elevation - deltaY * sensitivity
                            |> clamp -80 80  -- Limit vertical rotation
                    in
                    ( { model 
                      | azimuth = newAzimuth
                      , elevation = newElevation
                      , lastMousePos = Just (x, y)
                      }
                    , Cmd.none
                    )
                    
                _ ->
                    (model, Cmd.none)
                    
        NoOp ->
            (model, Cmd.none)

-- VIEW
boardSize : Int
boardSize = 8

squareSize : Float
squareSize = 1

-- Create the chessboard
chessboard : List (Scene3d.Entity ())
chessboard =
    List.concatMap (\y ->
        List.map (\x ->
            let
                isWhite = modBy 2 (x + y) == 0
                color = if isWhite then Color.white else Color.darkGray
                centerX = toFloat x - 3.5  -- Center the board
                centerY = toFloat y - 3.5
                frame = Frame3d.atPoint (Point3d.meters centerX centerY 0)
                size = ( Length.meters squareSize
                       , Length.meters squareSize
                       , Length.meters 0.1  -- Thin board
                       )
            in
            Block3d.centeredOn frame size
                |> Scene3d.blockWithShadow (Material.matte color)
        ) (List.range 0 (boardSize - 1))
    ) (List.range 0 (boardSize - 1))

-- Create camera based on current rotation
createCamera : Model -> Camera3d.Camera3d Length.Meters ()
createCamera model =
    let
        distance = 12
        azimuthRad = degrees model.azimuth
        elevationRad = degrees model.elevation
        
        eyeX = distance * cos elevationRad * cos azimuthRad
        eyeY = distance * cos elevationRad * sin azimuthRad
        eyeZ = distance * sin elevationRad
    in
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.meters eyeX eyeY eyeZ
                , focalPoint = Point3d.origin  -- Look at center of board
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 45
        }

view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "margin" "0"
        , Html.Attributes.style "padding" "0"
        , Html.Attributes.style "overflow" "hidden"
        , Html.Attributes.style "user-select" "none"
        ]
        [ Scene3d.sunny
            { camera = createCamera model
            , entities = chessboard
            , background = Scene3d.backgroundColor (Color.rgb255 232 206 235)
            , clipDepth = Length.meters 0.1
            , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
            , shadows = True
            , sunlightDirection = Direction3d.xyZ (Angle.degrees -120) (Angle.degrees -45)
            , upDirection = Direction3d.positiveZ
            }
            |> Html.map (\_ -> NoOp)  -- Scene3d doesn't produce messages
        ]

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onMouseDown (mouseDecoder MouseDown)
        , Browser.Events.onMouseUp (Decode.succeed MouseUp)
        , if model.isMouseDown then
            Browser.Events.onMouseMove (mouseDecoder MouseMove)
          else
            Sub.none
        ]

mouseDecoder : (Float -> Float -> Msg) -> Decode.Decoder Msg
mouseDecoder msg =
    Decode.map2 msg
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


-- Update camera to see full board
camera : Camera3d.Camera3d Length.Meters ()
camera =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.meters 7.5 15 20  -- Elevated view
                , focalPoint = Point3d.meters 7.5 7.5 0  -- Board center
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 45
        }


-- MAIN
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
