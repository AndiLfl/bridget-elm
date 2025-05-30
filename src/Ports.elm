-- Ports.elm
port module Ports exposing (..)

-- Outgoing Ports (Elm -> JavaScript -> Java)
port validateMove : BridgeMove -> Cmd msg
port requestAIMove : GameStateJson -> Cmd msg
port saveBridgeGame : GameStateJson -> Cmd msg

-- Incoming Ports (Java -> JavaScript -> Elm)
port moveValidated : (MoveValidationResult -> msg) -> Sub msg
port aiMoveCalculated : (AIMove -> msg) -> Sub msg
port gameLoaded : (GameStateJson -> msg) -> Sub msg

-- Data Types für JSON-Übertragung
type alias BridgeMove =
    { card : { suit : String, rank : String }
    , player : Int
    , trickPosition : Int
    }

type alias MoveValidationResult =
    { isValid : Bool
    , reason : Maybe String
    , updatedGameState : Maybe GameStateJson
    }

type alias AIMove =
    { selectedCard : { suit : String, rank : String }
    , confidence : Float
    , reasoning : String
    }