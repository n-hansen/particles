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
main = simulate display black 100 system render update
  where
    display = InWindow "demo" (400,400) (10,10)
    system =
      System
      [ Particle (V3 100 100 0) (V3 (-2) 0 0) 8000 []
      , Particle (V3 40 0 40) (V3 0 10 0) 5600 [] ]
      [force]
      0
    force :: Force Float
    force t p =
      ((p^.particleInvInertia)/(quadrance$p^.particleLocation)) *^
      (signorm.negated$p^.particleLocation)
    render (System [p1,p2] _ _ ) = pictures
                                   [ drawPathGloss
                                     projM
                                     (400,400)
                                     (1,0,0,1)
                                     (p1^.particleHistory)
                                   , drawPathGloss
                                     projM
                                     (400,400)
                                     (0,1,0,1)
                                     (p2^.particleHistory) ]
    projM = projectionMatrix
            (V3 20 0 100)
            (V3 0 0 0)
            (V3 30 0 100)
            (pi/1.5)
            1
    update _ = (trimParticleHistories 400 <$>) <$> eulerStepSystem
    
    


    
