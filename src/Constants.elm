module Constants exposing (..)


canvasS =
    { w = 120, sw = "120", h = 280, sh = "280" }


plrS =
    { gravityStrength = 970
    , jumpStrength = 310
    , frictionStrength = 2000
    , leftRightStrength = 144
    , w = 9
    , h = 12
    , maxSel = 5
    }


lavaS =
    { startingY = canvasS.h + 1 }


platformS =
    { w = 24
    , h = 6
    , newYA = 50
    , newYB = 55
    , speed = 50
    , divToNew = 22
    }


rightKeys =
    [ "ArrowRight", "d", "D", "ג" ]


leftKeys =
    [ "ArrowLeft", "a", "A", "ש" ]
