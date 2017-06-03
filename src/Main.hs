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
      [ Particle (V3 80 80 0) (V3 (-4) 0 3) 300 []
      , Particle (V3 40 0 40) (V3 0 5 0) 500 [] ]
      [force]
      0
    force :: Force Float
    force t p =
      ((p^.particleInvInertia)/(norm$p^.particleLocation)) *^
      (signorm.negated$p^.particleLocation)
    render (System [p1,p2] _ _ ) = drawManyGloss 100 projM (400,400) 
                                   [ Path
                                     (1,0,0,1)
                                     (p1^.particleHistory) 
                                   , Path
                                     (0,1,0,1)
                                     (p2^.particleHistory)
                                   , Node
                                     (0.2,0.2,0.9,0.5)
                                     (V3 0 0 0) ]
    projM = projectionMatrix
            (V3 20 0 100)
            (V3 0 0 0)
            (V3 30 0 100)
            (pi/1.5)
            1
    update _ = (trimParticleHistories 1500 <$>) <$> eulerStepSystem
    
    


    
