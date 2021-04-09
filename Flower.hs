{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude (
                        (#)                     -- x # f = f x
                        , (*^)                  -- multiply a vector by a scalar
                        , (@@)                  -- convert an angle tau@@rad == 360@@deg
                        , (^*)                  -- multiply an angle by a constant factor
                        , (^+^)                 -- add two vectors
                        , AlphaColour           -- type of colours that maybe semi-transparent
                        , Diagram               -- type of diagrams for a given backend
                        , Point                 -- type of points
                        , V2                    -- type of vectors of a given dimension and scalar type
                        , arc                   -- arc d a constructs an arc of radius 1 starting at direction d and extending to angle a
                        , atPoints              -- atPoints pts diags  constructs diags at points pts
                        , atop                  -- d1 `atop` d2 places d1 on top of d2
                        , bg                    -- superimpose a diagram on top of a rectangle of color 
                        , black                 -- a color name
                        , direction             -- convert an angle into a direction
                        , lcA                   -- a synonym for lineColor specialized to AlphaColour
                        , pentagon              -- construct a pentagon
                        , purple                -- a color name
                        , r2                    -- construct a 2D vector from a pair of components
                        , rad                   -- convert an angle in radians
                        , rotate                -- rotate a diagram
                        , tau                   -- 2 times pi
                        , trailVertices         -- extract the vertices of a trail
                        , translate             -- translate a diagram
                        , unp2                  -- convert a point into a pair of coordinates
                        )
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector
import Data.Colour (withOpacity)
import Data.Fixed

palette :: [AlphaColour Double]
palette = map (purple `withOpacity`) [0.5, 0.75, 1]

pentagonVertices :: [Point V2 Double]
pentagonVertices = trailVertices (pentagon 1)

middleVectors :: [V2 Double]
middleVectors = zipWith middle pentagonVertices (tail pentagonVertices ++ [head pentagonVertices])
    where
    middle p q = (vect p) ^+^ (vect q)
    vect = r2 . unp2

flourish :: Diagram B -> V2 Double -> Diagram B -> Diagram B
flourish pattern v center = center `atop` pattern # rotate r # translate v
    where
    r= signedAngleBetweenDirs (direction v) yDir

flowerArcs :: Diagram B
flowerArcs = atPoints pentagonVertices
    [(arc (direction (1 *^ e (d@@rad))) (a@@rad))
    | d <- [p,2*p..]]
    where
    a = 3*pi/5
    p = tau / 5

flower :: Int -> Diagram B
flower 2 = foldr (flourish (flower 1 # lcA (palette!!1))) (flower 0 # lcA (palette!!2)) middleVectors
flower 1 = foldr (flourish (flower 0 # lcA (palette!!0))) (flower 0) [middleVectors!!1,middleVectors!!2]
flower 0 = flowerArcs

diagram :: Diagram B
diagram = flower 2 # bg black

main = do
    mainWith $ diagram


