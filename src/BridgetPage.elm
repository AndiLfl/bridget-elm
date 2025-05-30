module BridgetPage exposing (main)

import Angle
import Axis3d
import Block3d
import Browser
import Browser.Dom as Dom
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
import Task
import Vector3d
import Viewpoint3d
import Html.Events

-- MODEL


type PieceType
    = LShape
    | TShape
    | ZShape
    | OShape


type alias Model =
    { azimuth : Float
    , elevation : Float
    , isMouseDown : Bool
    , lastMousePos : Maybe (Float, Float)
    , windowWidth : Float
    , windowHeight : Float
    , pieceRotX : Float
    , pieceRotY : Float
    , pieceRotZ : Float
    , shiftKeyPressed : Bool
    , pieceX : Int
    , pieceY : Int
    , pieceCenterX : Int
    , pieceCenterY : Int
    , pieceType : PieceType      -- NEW
    }

type Msg
    = MouseDown Float Float
    | MouseUp
    | MouseMove Float Float
    | NoOp
    | WindowResize Float Float
    | KeyChanged Bool
    | MovePiece Int Int         -- NEW
    | RotatePiece Axis Int      -- NEW
    | SwitchPieceType

type Axis = X | Y | Z          -- NEW



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { azimuth = 45
      , elevation = 30
      , isMouseDown = False
      , lastMousePos = Nothing
      , windowWidth = 0
      , windowHeight = 0
      , shiftKeyPressed = False
      , pieceRotX = 0
      , pieceRotY = 0
      , pieceRotZ = 0
      , pieceX = 3
      , pieceY = 3
      , pieceCenterX = 4      -- Center of 8x8 board
      , pieceCenterY = 4      -- Center of 8x8 board
      , pieceType = LShape
      }
    , Task.perform
        (\vp -> WindowResize vp.scene.width vp.scene.height)
        Dom.getViewport
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
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
            case ( model.isMouseDown, model.lastMousePos ) of
                ( True, Just ( lastX, lastY ) ) ->
                    let
                        deltaX =
                            x - lastX

                        deltaY =
                            y - lastY

                        sensitivity =
                            0.3
                    in
                    if model.shiftKeyPressed then
                        let
                            deltaRotX = deltaY * sensitivity
                            deltaRotY = deltaX * sensitivity
                            snappedX = toFloat (round (model.pieceRotX + deltaRotX / 90) * 90)
                            snappedY = toFloat (round (model.pieceRotY + deltaRotY / 90) * 90)
                        in
                        ( { model 
                            | pieceRotX = snappedX
                            , pieceRotY = snappedY
                            , lastMousePos = Just (x, y)
                        }
                        , Cmd.none
                        )

                    else
                        let
                            newAzimuth =
                                model.azimuth + deltaX * sensitivity

                            newElevation =
                                model.elevation - deltaY * sensitivity
                                    |> clamp -80 80
                        in
                        ( { model
                            | azimuth = newAzimuth
                            , elevation = newElevation
                            , lastMousePos = Just ( x, y )
                          }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        WindowResize w h ->
            ( { model
                | windowWidth = w
                , windowHeight = h
              }
            , Cmd.none
            )

        KeyChanged isPressed ->
            ( { model | shiftKeyPressed = isPressed }, Cmd.none )

        MovePiece dx dy ->
            let
                -- Get all cube positions for the current piece and rotation
                cubeOffsets = getCubeOffsets model.pieceType model.pieceRotZ
                minDx = List.minimum (List.map (\(offsetX, _, _) -> offsetX) cubeOffsets) |> Maybe.withDefault 0
                maxDx = List.maximum (List.map (\(offsetX, _, _) -> offsetX) cubeOffsets) |> Maybe.withDefault 0
                minDy = List.minimum (List.map (\(_, offsetY, _) -> offsetY) cubeOffsets) |> Maybe.withDefault 0
                maxDy = List.maximum (List.map (\(_, offsetY, _) -> offsetY) cubeOffsets) |> Maybe.withDefault 0

                minX = 0 - minDx
                maxX = boardSize - 1 - maxDx
                minY = 0 - minDy
                maxY = boardSize - 1 - maxDy

                newCenterX = clamp minX maxX (model.pieceCenterX + dx)
                newCenterY = clamp minY maxY (model.pieceCenterY + dy)
            in
            ( { model | pieceCenterX = newCenterX, pieceCenterY = newCenterY }, Cmd.none )

        RotatePiece axis dir ->
            case model.pieceType of
                LShape ->
                    let
                        snap angle =
                            let
                                newAngle = angle + 90 * toFloat dir
                                snapped = round (newAngle / 90) * 90
                            in
                            toFloat (modBy 360 snapped)
                        (newRotX, newRotY, newRotZ) =
                            case axis of
                                X -> (snap model.pieceRotX, model.pieceRotY, model.pieceRotZ)
                                Y -> (model.pieceRotX, snap model.pieceRotY, model.pieceRotZ)
                                Z -> (model.pieceRotX, model.pieceRotY, snap model.pieceRotZ)
                    in
                    ( { model
                        | pieceRotX = newRotX
                        , pieceRotY = newRotY
                        , pieceRotZ = newRotZ
                      }
                    , Cmd.none
                    )
                _ ->
                    let
                        snap angle =
                            let
                                newAngle = angle + 90 * toFloat dir
                                snapped = round (newAngle / 90) * 90
                            in
                            toFloat (modBy 360 snapped)
                        rotX = modBy 360 (round model.pieceRotX)
                        rotY = modBy 360 (round model.pieceRotY)
                        upright = rotX == 90 || rotX == 270
                        sideways = rotY == 90 || rotY == 270
                        (newRotX, newRotY, newRotZ) =
                            case axis of
                                X ->
                                    if sideways then
                                        (model.pieceRotX, model.pieceRotY, snap model.pieceRotZ)
                                    else
                                        (snap model.pieceRotX, model.pieceRotY, model.pieceRotZ)
                                Y ->
                                    if upright then
                                        (model.pieceRotX, model.pieceRotY, snap model.pieceRotZ)
                                    else
                                        (model.pieceRotX, snap model.pieceRotY, model.pieceRotZ)
                                Z ->
                                    (model.pieceRotX, model.pieceRotY, snap model.pieceRotZ)

                        -- Compute new footprint
                        newRotXInt = modBy 360 (round newRotX)
                        newRotYInt = modBy 360 (round newRotY)
                        isStandingX = newRotXInt == 90 || newRotXInt == 270
                        isStandingY = newRotYInt == 90 || newRotYInt == 270

                        footprintX = if isStandingY then 1 else 2
                        footprintY = if isStandingX then 1 else 2

                        maxX = boardSize - footprintX
                        maxY = boardSize - footprintY

                        clampedX = clamp 0 maxX model.pieceX
                        clampedY = clamp 0 maxY model.pieceY

                        -- Remove or rename adjustX/adjustY if not used

                    in
                    let
                        lRotAfter =
                            case model.pieceType of
                                OShape -> 0
                                LShape -> modBy 360 (round newRotZ)
                                TShape -> Debug.todo "Implement lRotAfter for TShape"
                                ZShape -> Debug.todo "Implement lRotAfter for ZShape"

                        (footprintXAfter, footprintYAfter) =
                            case model.pieceType of
                                OShape ->
                                    let
                                        newRotXIntAfter = modBy 360 (round newRotX)
                                        newRotYIntAfter = modBy 360 (round newRotY)
                                        isStandingXAfter = newRotXIntAfter == 90 || newRotXIntAfter == 270
                                        isStandingYAfter = newRotYIntAfter == 90 || newRotYIntAfter == 270
                                    in
                                    ( if isStandingYAfter then 1 else 2
                                    , if isStandingXAfter then 1 else 2
                                    )
                                LShape ->
                                    if lRotAfter == 0 || lRotAfter == 180 then
                                        (2, 3)
                                    else
                                        (3, 2)
                                TShape ->
                                    Debug.todo "Implement footprintAfter for TShape"
                                ZShape ->
                                    Debug.todo "Implement footprintAfter for ZShape"

                        maxXAfter = boardSize - footprintXAfter
                        maxYAfter = boardSize - footprintYAfter

                        clampedXAfter = clamp 0 maxXAfter model.pieceX
                        clampedYAfter = clamp 0 maxYAfter model.pieceY
                    in
                    ( { model
                        | pieceRotX = newRotX
                        , pieceRotY = newRotY
                        , pieceRotZ = newRotZ
                        , pieceX = clampedXAfter
                        , pieceY = clampedYAfter
                      }
                    , Cmd.none
                    )

        SwitchPieceType ->
            let
                newType =
                    case model.pieceType of
                        LShape -> TShape
                        TShape -> ZShape
                        ZShape -> OShape
                        OShape -> LShape
            in
            ( { model | pieceType = newType }, Cmd.none )



-- VIEW


boardSize : Int
boardSize =
    8


squareSize : Float
squareSize =
    1


chessboard : List (Scene3d.Entity ())
chessboard =
    List.concatMap
        (\y ->
            List.map
                (\x ->
                    let
                        isWhite =
                            modBy 2 (x + y) == 0

                        color =
                            if isWhite then
                                Color.white

                            else
                                Color.darkGray

                        centerX =
                            toFloat x - 3
                        centerY =
                            toFloat y - 3
                    in
                    Block3d.centeredOn
                        (Frame3d.atPoint (Point3d.meters centerX centerY 0))
                        ( Length.meters squareSize
                        , Length.meters squareSize
                        , Length.meters 0.1
                        )
                        |> Scene3d.blockWithShadow (Material.matte color)
                )
                (List.range 0 (boardSize - 1))
        )
        (List.range 0 (boardSize - 1))

gamePiece : Model -> Scene3d.Entity ()
gamePiece model =
    let
        centerColor = Color.purple
        normalColor = case model.pieceType of
            OShape -> Color.yellow
            TShape -> Color.red
            ZShape -> Color.green
            LShape -> Color.orange
    in
    case model.pieceType of
        LShape ->
            -- L piece: x, x, Xx (center at (0,0))
            let
                cubes =
                    [ (0,-2,False), (0,-1,False), (0,0,True), (1,0,False) ]
                cubeEntities =
                    List.map
                        (\(dx,dy,isCenter) ->
                            Block3d.centeredOn
                                (Frame3d.atPoint
                                    (Point3d.meters
                                        (toFloat model.pieceCenterX + toFloat dx - (toFloat boardSize / 2))
                                        (toFloat model.pieceCenterY + toFloat dy - (toFloat boardSize / 2))
                                        0
                                    )
                                )
                                ( Length.meters 1, Length.meters 1, Length.meters 1 )
                                |> Scene3d.blockWithShadow (Material.matte (if isCenter then centerColor else normalColor))
                        )
                        cubes
                group =
                    Scene3d.group cubeEntities
                        |> Scene3d.rotateAround Axis3d.x (Angle.degrees model.pieceRotX)
                        |> Scene3d.rotateAround Axis3d.y (Angle.degrees model.pieceRotY)
                        |> Scene3d.rotateAround Axis3d.z (Angle.degrees model.pieceRotZ)
                        |> Scene3d.translateBy (Vector3d.meters 0 0 0.5)
                in
                group
                
        OShape ->
            -- O piece: xx, Xx (center at (0,1))
            let
                cubes =
                    [ (0,0,False), (1,0,False), (0,1,True), (1,1,False) ]
                cubeEntities =
                    List.map
                        (\(dx,dy,isCenter) ->
                            Block3d.centeredOn
                                (Frame3d.atPoint
                                    (Point3d.meters
                                        (toFloat model.pieceX + toFloat dx - 3.5 + 0.5)
                                        (toFloat model.pieceY + toFloat dy - 3.5 + 0.5)
                                        0.5
                                    )
                                )
                                ( Length.meters 1, Length.meters 1, Length.meters 1 )
                                |> Scene3d.blockWithShadow (Material.matte (if isCenter then centerColor else normalColor))
                        )
                        cubes
                group =
                    Scene3d.group cubeEntities
                        |> Scene3d.rotateAround Axis3d.z (Angle.degrees model.pieceRotZ)
                in
                group

        TShape ->
            -- T piece:   x, xXx (center at (1,1))
            let
                cubes =
                    [ (1,0,False), (0,1,False), (1,1,True), (2,1,False) ]
                cubeEntities =
                    List.map
                        (\(dx,dy,isCenter) ->
                            Block3d.centeredOn
                                (Frame3d.atPoint
                                    (Point3d.meters
                                        (toFloat model.pieceX + toFloat dx - 3.5 + 0.5)
                                        (toFloat model.pieceCenterY + toFloat dy - 3.5 + 0.5)
                                        0.5
                                    )
                                )
                                ( Length.meters 1, Length.meters 1, Length.meters 1 )
                                |> Scene3d.blockWithShadow (Material.matte (if isCenter then centerColor else normalColor))
                        )
                        cubes
                group =
                    Scene3d.group cubeEntities
                        |> Scene3d.rotateAround Axis3d.z (Angle.degrees model.pieceRotZ)
                in
                group

        ZShape ->
            -- Z piece:   xx, xX (center at (1,1))
            let
                cubes =
                    [ (1,0,False), (2,0,False), (0,1,False), (1,1,True) ]
                cubeEntities =
                    List.map
                        (\(dx,dy,isCenter) ->
                            Block3d.centeredOn
                                (Frame3d.atPoint
                                    (Point3d.meters
                                        (toFloat model.pieceX + toFloat dx - 3.5 + 0.5)
                                        (toFloat model.pieceCenterY + toFloat dy - 3.5 + 0.5)
                                        0.5
                                    )
                                )
                                ( Length.meters 1, Length.meters 1, Length.meters 1 )
                                |> Scene3d.blockWithShadow (Material.matte (if isCenter then centerColor else normalColor))
                        )
                        cubes
                group =
                    Scene3d.group cubeEntities
                        |> Scene3d.rotateAround Axis3d.z (Angle.degrees model.pieceRotZ)
                in
                group


createCamera : Model -> Camera3d.Camera3d Length.Meters ()
createCamera model =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint =
                    Point3d.meters
                        (16 * cos (degrees model.elevation) * cos (degrees model.azimuth))
                        (16 * cos (degrees model.elevation) * sin (degrees model.azimuth))
                        (16 * sin (degrees model.elevation))
                , focalPoint = Point3d.origin
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 45
        }


view : Model -> Html Msg
view model =
    let
        entities =
            chessboard ++ [ gamePiece model ] ++ compassEntities
    in
    Html.div
        [ Html.Attributes.style "position" "fixed"
        , Html.Attributes.style "top" "0"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100%"
        , Html.Attributes.style "margin" "0"
        , Html.Attributes.style "padding" "0"
        , Html.Attributes.style "overflow" "hidden"
        , Html.Attributes.style "user-select" "none"
        ]
        [ Html.button
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "z-index" "10"
            , Html.Attributes.style "top" "20px"
            , Html.Attributes.style "left" "20px"
            , Html.Events.onClick SwitchPieceType
            ]
            [ Html.text "Switch Piece" ]
        , Scene3d.sunny
            { camera = createCamera model
            , entities = entities
            , background = Scene3d.backgroundColor (Color.rgb255 232 206 235)
            , clipDepth = Length.meters 0.1
            , dimensions =
                ( Pixels.pixels (round model.windowWidth)
                , Pixels.pixels (round model.windowHeight)
                )
            , shadows = True
            , sunlightDirection = Direction3d.xyZ (Angle.degrees -120) (Angle.degrees -45)
            , upDirection = Direction3d.positiveZ
            }
            |> Html.map (\_ -> NoOp)
        ]


compassEntities : List (Scene3d.Entity ())
compassEntities =
    let
        labelSize = ( Length.meters 0.8, Length.meters 0.8, Length.meters 0.05 )
        z = Length.meters 0.15
        n = Block3d.centeredOn (Frame3d.atPoint (Point3d.meters 0 -4 0.15)) labelSize
                |> Scene3d.blockWithShadow (Material.matte (Color.rgb255 200 0 0))
        s = Block3d.centeredOn (Frame3d.atPoint (Point3d.meters 0 5 0.15)) labelSize
                |> Scene3d.blockWithShadow (Material.matte (Color.rgb255 0 120 0))
        w = Block3d.centeredOn (Frame3d.atPoint (Point3d.meters -4 0 0.15)) labelSize
                |> Scene3d.blockWithShadow (Material.matte (Color.rgb255 0 0 200))
        e = Block3d.centeredOn (Frame3d.atPoint (Point3d.meters 5 0 0.15)) labelSize
                |> Scene3d.blockWithShadow (Material.matte (Color.rgb255 200 200 0))
    in
    [ n, e, s, w ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onMouseDown (mouseDecoder MouseDown)
        , Browser.Events.onMouseUp (Decode.succeed MouseUp)
        , Browser.Events.onResize (\w h -> WindowResize (toFloat w) (toFloat h))
        , Browser.Events.onKeyDown (Decode.map keyToMsg Decode.value)
        , Browser.Events.onKeyUp (Decode.map (keyCheck False) Decode.value)
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


keyCheck : Bool -> Decode.Value -> Msg
keyCheck isDown json =
    case Decode.decodeValue (Decode.field "key" Decode.string) json of
        Ok "Shift" ->
            KeyChanged isDown

        _ ->
            NoOp


keyToMsg : Decode.Value -> Msg
keyToMsg json =
    case Decode.decodeValue (Decode.field "key" Decode.string) json of
        Ok "ArrowUp"    -> MovePiece 0 -1
        Ok "ArrowDown"  -> MovePiece 0 1
        Ok "ArrowLeft"  -> MovePiece -1 0
        Ok "ArrowRight" -> MovePiece 1 0
        Ok "q"          -> RotatePiece Z 1
        Ok "e"          -> RotatePiece Z -1
        Ok "s"          -> RotatePiece X -1
        Ok "w"          -> RotatePiece X 1
        Ok "a"          -> RotatePiece Y 1
        Ok "d"          -> RotatePiece Y -1
        _               -> NoOp


getCubeOffsets : PieceType -> Float -> List (Int, Int, Bool)
getCubeOffsets pieceType rotZ =
    case pieceType of
        LShape ->
            case modBy 360 (round rotZ) of
                -- Default: standing on Xx (center at bottom right of L)
                0   -> [ (0, 0, True), (0, -1, False), (0, -2, False), (-1, 0, False) ]
                -- Rotated: lying on side, standing on x (center at bottom left of L)
                90  -> [ (0, 0, True), (1, 0, False), (2, 0, False), (0, -1, False) ]
                -- Upside down: standing on x (center at top left of L)
                180 -> [ (0, 0, True), (0, 1, False), (0, 2, False), (1, 0, False) ]
                -- Rotated: lying on other side, standing on x (center at top right of L)
                270 -> [ (0, 0, True), (-1, 0, False), (-2, 0, False), (0, 1, False) ]
                _   -> [ (0, 0, True), (0, -1, False), (0, -2, False), (-1, 0, False) ]
        -- Add similar logic for other pieces
        _ -> []


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
