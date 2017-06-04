-- Components for building up systems
module Systems where

import ClassyPrelude
import Control.Lens
import Control.Monad.Random
import Linear.Metric
import Linear.V3
import Linear.Vector
import System.Random

import Physics


-- | Generate a new random particle with the given constraints.
generateParticle :: (RandomGen g, RealFrac a)
                 => a -- ^ Time delay between particle production
                 -> Rand g (a,a,a) -- ^ Starting position generation
                 -> Rand g (a,a,a) -- ^ Starting velocity generation
                 -> Rand g a -- ^ Starting inv inertia generation
                 -> Rand g pData -- ^ Starting data generation
                 -> SystemUpdater pData fData g a
generateParticle delay genPos genVel genInvI genData = do
  currT <- use systemTime
  nextT <- asks (+currT)
  when (floor (currT/delay) /= floor (nextT/delay)) $ do
           gen <- use systemData
           let ((pos,vel,invi,d),newGen) =
                 runRand
                 ( liftM4 (,,,)
                   genPos
                   genVel
                   genInvI
                   genData )
                 gen
           systemData .= newGen
           systemParticles %= (makeParticle pos vel invi d :)
  

-- | Force which acts according to an inverse power law.
inversePowerForce :: (Floating a,Eq a)
                  => Int -- ^ [Positive!] exponent
                  -> a -- ^ Force constant (negative for repulsive forces)
                  -> (a,a,a) -- ^ Location
                  -> fData
                  -> Force fData a
inversePowerForce expn fconst = makeForce eqn
  where
    eqn f p =
      let dx = (f^.forceLocation - p^.particleLocation)
      in if norm dx == 0
         then zero
         else (p^.particleInvInertia*fconst/norm dx^expn) *^ signorm dx
