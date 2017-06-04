module Main where

import ClassyPrelude
import Control.Lens
import Control.Monad.Random
import Graphics.Gloss
import Linear.Metric
import Linear.V3
import Linear.Vector
import System.Random.Mersenne.Pure64

import Physics
import Render
import Systems

main :: IO ()
main = simulate display black 60 system render update
  where
    display = InWindow "demo" (400,400) (10,10)
    system =
      makeSystem []
      [ inversePowerForce 3 1000 (0,0,0) (0.2,0.3,0.9,0.7) ]
      (pureMT 2)
      ( generateParticle 4
        ((\x->(x,70,0)) <$> getRandomR (-60,60))
        ( liftM3 (,,)
          (getRandomR (-3,3))
          (getRandomR (-3,3))
          (getRandomR (-1,1)) )
        (getRandomR (30,60))
        ( liftM3 (\r g b->(r,g,b,1))
          (getRandomR (0.3,0.9))
          (getRandomR (0.3,0.9))
          (getRandomR (0.3,0.9)) ) >>
        eulerStepSystem >>
        absorbParticles 5
        (\f p ->
          let (pr, pg, pb, _ ) = p^.particleData
          in f & forceData %~ (\ (fr, fg, fb, a) ->
                                ( (fr+pr)/2,
                                  (fg+pg)/2,
                                  (fb+pb)/2,
                                  a ))) >>
        clipParticles (100000) >>
        trimParticleHistories 2000 >>
        advanceTime )
                            
    render = drawSystemGloss 10 projM (400,400)                          
    projM = projectionMatrix
            (V3 20 0 100)
            (V3 0 0 0)
            (V3 30 0 100)
            (pi/1.5)
            1
    update _ = runSystemUpdate
