module Constants exposing (..)


canvasS =
    -- { w = 780, sw = "780", h = 640, sh = "640" }
    { w = 120, sw = "120", h = 280, sh = "280" }


plrS =
    { gravityStrength = 650
    , jumpStrength = 280 -- can jump up to comfortably 60 to a max of 64
    , maxDy = 480
    , frictionStrength = 1800
    , leftRightStrength = 120
    , w = 9
    , h = 12
    }


platformS =
    { w = 24, h = 6 }


rightKeys =
    [ "ArrowRight", "d", "D" ]


leftKeys =
    [ "ArrowLeft", "a", "A" ]


upKeys =
    [ "ArrowUp", " ", "w", "W" ]
