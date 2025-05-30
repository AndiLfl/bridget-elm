
-- Move.elm
module Move exposing (Move, MoveValidationResponse, validateMove, undoMove)

import Http
import Json.Decode as Decode
import Json.Encode as Encode

type alias Move =
    { rotationIndex : Int
    , shape : List (List Int)
    , rotationMatrix : List (List Int)  
    , x : Int
    , y : Int
    , player : Int
    }

type alias MoveValidationResponse =
    { isValid : Bool
    , historyKey : String
    }

-- Encode Move to JSON
encodeMoveRequest : Move -> Encode.Value
encodeMoveRequest move =
    Encode.object
        [ ( "rotationIndex", Encode.int move.rotationIndex )
        , ( "shape", Encode.list (Encode.list Encode.int) move.shape )
        , ( "rotationMatrix", Encode.list (Encode.list Encode.int) move.rotationMatrix )
        , ( "x", Encode.int move.x )
        , ( "y", Encode.int move.y )
        , ( "player", Encode.int move.player )
        ]

-- Decode validation response
decodeValidationResponse : Decode.Decoder MoveValidationResponse
decodeValidationResponse =
    Decode.map2 MoveValidationResponse
        (Decode.field "isValid" Decode.bool)
        (Decode.field "historyKey" Decode.string)

-- Validate move via HTTP
validateMove : Move -> (Result Http.Error MoveValidationResponse -> msg) -> Cmd msg
validateMove move toMsg =
    Http.post
        { url = "http://localhost:8080/api/game/validate-move"
        , body = Http.jsonBody (encodeMoveRequest move)
        , expect = Http.expectJson toMsg decodeValidationResponse
        }

-- Undo move via HTTP
undoMove : Move -> (Result Http.Error String -> msg) -> Cmd msg
undoMove move toMsg =
    Http.post
        { url = "http://localhost:8080/api/game/undo-move"
        , body = Http.jsonBody (encodeMoveRequest move)
        , expect = Http.expectString toMsg
        }
