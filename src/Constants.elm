module Constants exposing (..)


canvasS =
    { w = 120, sw = "120", h = 280, sh = "280" }


plrS =
    { gravityStrength = 650
    , jumpStrength = 248
    , maxDy = 480
    , jumpCheckBuffer = 8
    , frictionStrength = 2000
    , leftRightStrength = 144
    , w = 9
    , h = 12
    }


lavaS =
    { startingY = canvasS.h + 1 }


platformS =
    { w = 24
    , h = 6
    , newYA = 50
    , newYB = 55
    , speed = 42
    }


rightKeys =
    [ "ArrowRight", "d", "D" ]


leftKeys =
    [ "ArrowLeft", "a", "A" ]


upKeys =
    [ "ArrowUp", " ", "w", "W" ]
