{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector
import Data.Colour (withOpacity)

palettes :: [AlphaColour Double]
palettes = map (purple `withOpacity`) [0.5,0.75,1]

pentaVertices :: [Point V2 Double]
pentaVertices = trailVertices (pentagon 1)

middleVectors :: [V2 Double]
middleVectors = zipWith middle pentaVertices (tail pentaVertices ++ [head pentaVertices])
    where
    middle p q = (vect p) ^+^ (vect q)
    vect = r2 . unp2

flourish :: Diagram B -> V2 Double -> Diagram B -> Diagram B
flourish pattern v center = center `atop` pattern # rotate r # translate v
    where
    r= signedAngleBetweenDirs (direction v) yDir

flower :: Int -> Diagram B
flower 2 = foldr (flourish (flower 1 # lcA (palettes!!1))) (flower 0 # lcA (palettes!!2)) middleVectors
flower 1 = foldr (flourish (flower 0 # lcA (palettes!!0))) (flower 0) [middleVectors!!1,middleVectors!!2]
flower 0 = flowerArcs

flowerArcs :: Diagram B
flowerArcs = atPoints pentaVertices
    [(arc (direction (1 *^ e (d@@rad))) (a@@rad))
    | d <- [p,2*p..]]
    where
    a = 3*pi/5
    p = tau / 5

diagram :: Diagram B
diagram = flower 2 # bg black

main = mainWith $ diagram


