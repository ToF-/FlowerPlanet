
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector

diagram :: Diagram B
diagram = pentagon 1
    `atop`  penta # translateX 1 # rotateAround p  (a@@rad)
    where
    penta = pentagon 1
    p = (trailVertices penta)!!0
    a = -pi/5

main = mainWith $ diagram 


