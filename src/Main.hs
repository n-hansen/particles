module Main where

import ClassyPrelude
import Control.Lens
import Graphics.Gloss
import Linear.Metric
import Linear.V3
import Linear.Vector

import Render
import Physics

main :: IO ()
main = simulate display black 60 system render update
  where
    display = InWindow "demo" (400,400) (10,10)
    system =
      System
      [ Particle (V3 80 80 0) (V3 (-4) 0 3) 300 [] (1,0.2,0,1)
      , Particle (V3 40 0 40) (V3 0 5 0) 500 [] (0,1,0.2,1) ]
      [ Force
        (\f p -> let dx = (f^.forceLocation - p^.particleLocation)
                 in (p^.particleInvInertia/norm dx) *^
                    (signorm.negated$p^.particleLocation) )
        (V3 0 0 0)
        (0.2,0.3,0.9,0.6)
      ]
      0
    render = drawSystemGloss 10 projM (400,400)                          
    projM = projectionMatrix
            (V3 20 0 100)
            (V3 0 0 0)
            (V3 30 0 100)
            (pi/1.5)
            1
    update _ = (trimParticleHistories 1500 <$>) <$> eulerStepSystem

