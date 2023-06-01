module Lava exposing (..)

import Constants exposing (..)
import Engine exposing (..)


newLava : EntityBase
newLava =
    { pos = newPosition 0 0
    , dim = newDimension canvasS.w canvasS.h
    , rot = initialRotation
    }
