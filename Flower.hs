
 {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector

diagram :: Diagram B
diagram = pentagon 1 # lc black 
    `atop`  (penta # rotate ((72+72/2)@@deg) # translate (vs!!0) # lc purple)
    `atop`  (penta # rotate ((72+72/2)@@deg) # translate (vs!!1) # lc green)
    `atop`  (penta # rotate ((72+72/2)@@deg) # translate (vs!!2) # lc blue)
    `atop`  (penta # rotate ((72+72/2)@@deg) # translate (vs!!3) # lc red)
    `atop`  (penta # rotate ((72+72/2)@@deg) # translate (vs!!4) # lc orange)
    where
    penta = pentagon 1 `atop` (atPoints (trailVertices (pentagon 1)) (map (\n -> text (show n) # fontSizeL 0.2) [0..4]))
    pts = trailVertices (pentagon 1)
    vs :: [V2 Double]
    vs = zipWith (\p q -> (r2 (unp2 p)) ^+^ (r2 (unp2 q))) pts (tail pts ++ [head pts])

main = mainWith $ diagram 


