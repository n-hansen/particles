{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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


class HasPRNGField a g where
  prngField :: Lens' a g

instance RandomGen g => HasPRNGField g g where
  prngField = id


-- | Generate a new random particle with the given constraints.
generateParticle :: (RandomGen g,
                     RealFrac a,
                     HasPRNGField sData g)
                 => a -- ^ Time delay between particle production
                 -> Rand g (a,a,a) -- ^ Starting position generation
                 -> Rand g (a,a,a) -- ^ Starting velocity generation
                 -> Rand g a -- ^ Starting inv inertia generation
                 -> Rand g pData -- ^ Starting data generation
                 -> SystemUpdater pData fData sData a
generateParticle delay genPos genVel genInvI genData = do
  currT <- use systemTime
  nextT <- asks (+currT)
  when (floor (currT/delay) /= floor (nextT/delay)) $ do
           gen <- use $ systemData . prngField
           let ((pos,vel,invi,d),newGen) =
                 runRand
                 ( liftM4 (,,,)
                   genPos
                   genVel
                   genInvI
                   genData )
                 gen
           systemData . prngField .= newGen
           systemParticles %= (makeParticle pos vel invi d :)
  

-- | Force which acts according to an inverse power law.
inversePowerForce :: (Floating a,Eq a)
                  => Int -- ^ Exponent
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
         else (p^.particleInvInertia*fconst/norm dx^^expn) *^ signorm dx


-- | Absorb any particles close enough to a force node into the node
absorbParticles :: (Floating a,Ord a)
                => a -- ^ Absorption distance
                -> ( Force fData a
                     -> Particle pData a
                     -> Force fData a ) -- ^ Merge function
                -> SystemUpdater pData fData sData a
absorbParticles absorbDist absorb = do
  forces <- use systemForces
  updatedForces <- forM forces $ \force -> do
    -- If a particle is within absorption distance, we halt it by setting
    -- velocity to zero and inertia to infinity.
    -- Once its history is fully within the absorption distance,
    -- we merge it into the node and delete it
    let checkDist x =
          distance (force^.forceLocation) x <= absorbDist
        haltIfAbsorbed particle =
          if checkDist (particle^.particleLocation)
          then particle & particleVelocity .~ zero & particleInvInertia .~ 0
          else particle
        readyToMerge = views particleHistory
                       ( maybe True (checkDist . last)
                         . fromNullable )
    (toMerge,rest) <- uses systemParticles
                      (partition readyToMerge . map haltIfAbsorbed)
    systemParticles .= rest
    return $ foldl' absorb force toMerge
  systemForces .= updatedForces


-- | Delete any particles more than a given distance from the origin
clipParticles :: (Floating a,Ord a)
              => a -- ^ Clip distance
              -> SystemUpdater pData fData sData a
clipParticles clipDist =
  systemParticles %= filter (views particleLocation ((clipDist >) . norm))
