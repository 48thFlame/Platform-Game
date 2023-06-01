module Constants exposing (..)


canvasS =
    { w = 120, sw = "120", h = 280, sh = "280" }


plrS =
    { gravityStrength = 650
    , jumpStrength = 273
    , maxDy = 480
    , jumpCheckBuffer = 8
    , frictionStrength = 1800
    , leftRightStrength = 120
    , w = 9
    , h = 12
    }


lavaS =
    { startingY = canvasS.h + 21 }


platformS =
    { w = 24
    , h = 6

    -- , newYA = 55
    -- , newYB = 71
    , newYA = 67
    , newYB = 80
    , speed = 25
    }


rightKeys =
    [ "ArrowRight", "d", "D" ]


leftKeys =
    [ "ArrowLeft", "a", "A" ]


upKeys =
    [ "ArrowUp", " ", "w", "W" ]
