{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector

pentaVertices = trailVertices (pentagon 1)

middleVectors = zipWith middle pentaVertices (tail pentaVertices ++ [head pentaVertices])
    where
    middle p q = (vect p) ^+^ (vect q)
    vect = r2 . unp2
    
flourish :: Diagram B -> V2 Double -> Diagram B -> Diagram B
flourish pattern v center = center `atop` pattern # rotate r # translate v 
    where
    r= signedAngleBetweenDirs (direction v) yDir

flower :: Int -> Diagram B
flower 2 = foldr (flourish (flower 1)) (flower 0) middleVectors
flower 1 = foldr (flourish (flower 0)) (flower 0) [middleVectors!!1,middleVectors!!2]
flower 0 = flowerArcs 

flowerArcs :: Diagram B
flowerArcs = atPoints pentaVertices 
    [(arc (direction (1 *^ e (d@@rad))) (a@@rad)) 
    | d <- [p,2*p..]]  
    where
    a = 3*pi/5
    p = tau / 5

diagram :: Diagram B
diagram = flower 2 # lw 0.5 <> circle 3.45 # lw 0.5 

main = mainWith $ diagram 


