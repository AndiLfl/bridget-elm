module Main exposing (main)

-- Previous imports and model definitions...


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
    { camera : Camera3d.Camera3d Length.Meters {}
    , pieces : List Piece
    , selectedPiece : Maybe Piece
    , initialPosition : Maybe ( Float, Float )
    , currentPlayer : Player
    }

type Player = White | Black

type alias Piece =
    { id : Int
    , shape : List ( Float, Float )
    , color : Color.Color
    , position : ( Float, Float )
    , rotation : Float
    , owner : Player
    }

-- MSG
type Msg
    = PickUpPiece Int ( Float, Float )
    | MovePiece ( Float, Float )
    | DropPiece
    | RotatePiece Int
    | EndTurn
    | ResetPiece

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PickUpPiece id (x, y) ->
            case findPieceById id model.pieces of
                Just piece ->
                    if piece.owner == model.currentPlayer then
                        ( { model 
                          | selectedPiece = Just piece
                          , initialPosition = Just (x, y)
                          }
                        , Cmd.none
                        )
                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        MovePiece (x, y) ->
            case ( model.selectedPiece, model.initialPosition ) of
                ( Just piece, Just (initialX, initialY) ) ->
                    let
                        dx = (x - initialX) / 50  -- Scale mouse movement
                        dy = (y - initialY) / 50
                        newX = Tuple.first piece.position + dx
                        newY = Tuple.second piece.position + dy
                        newPieces = updatePiecePosition piece.id ( newX, newY ) model.pieces
                    in
                    ( { model | pieces = newPieces }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        DropPiece ->
            let
                validatedPieces = 
                    model.pieces 
                        |> List.map (validatePosition model.pieces)
            in
            ( { model 
              | selectedPiece = Nothing
              , initialPosition = Nothing
              , pieces = validatedPieces
              }
            , Cmd.none
            )

        RotatePiece id ->
            ( { model 
              | pieces = rotatePieceById id model.pieces 
              }
            , Cmd.none
            )

        EndTurn ->
            ( { model 
              | currentPlayer = 
                    if model.currentPlayer == White then 
                        Black 
                    else 
                        White
              }
            , Cmd.none
            )

        ResetPiece ->
            ( { model 
              | pieces = resetInvalidPieces model.pieces 
              , selectedPiece = Nothing
              , initialPosition = Nothing
              }
            , Cmd.none
            )

-- HELPER FUNCTIONS
findPieceById : Int -> List Piece -> Maybe Piece
findPieceById id pieces =
    List.filter (\p -> p.id == id) pieces |> List.head

updatePiecePosition : Int -> ( Float, Float ) -> List Piece -> List Piece
updatePiecePosition id newPos pieces =
    List.map
        (\p ->
            if p.id == id then
                { p | position = newPos }
            else
                p
        )
        pieces

validatePosition : List Piece -> Piece -> Piece
validatePosition allPieces piece =
    let
        (x, y) = piece.position
        isValid = 
            not (anyCollisions piece allPieces)
            && x >= 0 && x < 8
            && y >= 0 && y < 8
    in
    if isValid then
        piece
    else
        { piece | position = piece.position }  -- Keep original position

anyCollisions : Piece -> List Piece -> Bool
anyCollisions target pieces =
    List.any
        (\p ->
            p.id /= target.id
            && distance p.position target.position < 1
        )
        pieces

distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance (x1, y1) (x2, y2) =
    sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- VIEW
view : Model -> Html Msg
view model =
    Html.div 
        [ Html.Attributes.style "height" "100vh"
        , Html.Events.onMouseUp DropPiece
        ]
        [ Scene3d.sunny
            { camera = createCamera model.camera
            , entities = 
                renderBoard 
                ++ List.concatMap renderPiece model.pieces
                ++ renderControlPanel model
            , background = Scene3d.backgroundColor Color.lightGray
            , clipDepth = Length.meters 0.1
            , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
            , shadows = True
            , sunlightDirection = Direction3d.xyZ (Angle.degrees -120) (Angle.degrees -45)
            , upDirection = Direction3d.positiveZ
            }
        , Html.div
            [ Html.Attributes.style "position" "fixed"
            , Html.Attributes.style "bottom" "20px"
            , Html.Attributes.style "left" "50%"
            , Html.Attributes.style "transform" "translateX(-50%)"
            ]
            [ Html.button
                [ Html.Events.onClick EndTurn
                , Html.Attributes.disabled (model.selectedPiece /= Nothing)
                ]
                [ Html.text "Done" ]
            ]
        ]

renderPiece : Piece -> List (Scene3d.Entity Msg)
renderPiece piece =
    let
        (x, y) = piece.position
        isSelected = Maybe.map (.id >> (==) piece.id) model.selectedPiece |> Maybe.withDefault False
    in
    List.map (\(dx, dy) ->
        Scene3d.blockWithShadow (Material.matte piece.color)
            (Block3d.centeredOn 
                (Frame3d.atPoint (Point3d.meters (x + dx) (y + dy) (if isSelected then 0.2 else 0.1)))
                (Length.meters 0.9, Length.meters 0.9, Length.meters 0.8)
            )
            |> Scene3d.onMouseDown (PickUpPiece piece.id)
            |> Scene3d.onMouseMove MovePiece
    ) piece.shape

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onMouseMove (Decode.map MovePointer mousePositionDecoder)
        , Browser.Events.onMouseUp (Decode.succeed DropPiece)
        ]

mousePositionDecoder : Decode.Decoder ( Float, Float )
mousePositionDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)

-- INITIAL MODEL
initialModel : Model
initialModel =
    { camera =
        Camera3d.perspective
            { viewpoint =
                Viewpoint3d.lookAt
                    { eyePoint = Point3d.meters 8 8 12
                    , focalPoint = Point3d.meters 4 4 0
                    , upDirection = Direction3d.positiveZ
                    }
            , verticalFieldOfView = Angle.degrees 30
            }
    , pieces = initialPieces
    , selectedPiece = Nothing
    , initialPosition = Nothing
    , currentPlayer = White
    }

initialPieces : List Piece
initialPieces =
    -- Implement your piece initialization logic here
    -- Create 14 white and 14 black pieces with unique IDs
    []

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }