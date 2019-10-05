
 {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector

diagram :: Diagram B
diagram = pentagon 1 # lc black # showOrigin
    `atop` (atPoints [pts!!0,pts!!1] (repeat (circle 0.05)) # lw none # fc red)
    `atop` fromOffsets [v] 
    `atop`  (penta # rotate ((72+72/2)@@deg) # translate v # lc purple # showOrigin)
    where
    penta = pentagon 1
    pts = trailVertices penta
    p,q :: (Double,Double)
    p = unp2 $ pts!!0 
    q = unp2 $ pts!!1
    v :: V2 Double
    v = (r2 p) ^+^ (r2 q)
    a = direction v

main = mainWith $ diagram 


