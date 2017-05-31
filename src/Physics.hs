{-# LANGUAGE TemplateHaskell #-}
module Physics where

import ClassyPrelude
import Control.Lens
import Linear.V3
import Linear.Vector


data Particle a = Particle { _particleLocation :: V3 a
                           , _particleVelocity :: V3 a
                           , _particleInvInertia :: a
                           , _particleHistory :: [V3 a]
                           } deriving Show
makeLenses ''Particle

-- | Semi-implicit Euler stepping function
eulerStepParticle :: Floating a
                  => a -- ^ Time step
                  -> Particle a -- ^ Particle
                  -> V3 a -- ^ Accelleration
                  -> Particle a
eulerStepParticle deltaT (Particle x0 v0 i path) accel =
  Particle x1 v1 i (x0:path)
  where
    v1 = v0 ^+^ (accel ^* deltaT)
    x1 = x0 ^+^ (v1 ^* deltaT)

-- | A force takes a time and a particle and returns an accelleration
type Force a = a -> Particle a -> V3 a

data System a = System { _systemParticles :: [Particle a]
                       , _systemForces :: [Force a]
                       , _systemTime :: a
                       } 
makeLenses ''System

eulerStepSystem :: Floating a
                => a -- ^ Time step
                -> System a -- ^ The system
                -> System a
eulerStepSystem deltaT system = system
                                & systemParticles . each %~ process
                                & systemTime +~ deltaT
  where
    process particle = eulerStepParticle deltaT particle 
                       ( sum
                         . map (\f -> f (system ^. systemTime) particle)
                         $ system ^. systemForces )
                       
trimParticleHistory :: Int -> Particle a -> Particle a
trimParticleHistory n = particleHistory %~ take n

trimParticleHistories :: Int -> System a -> System a
trimParticleHistories n = systemParticles . each . particleHistory %~ take n

