{-# LANGUAGE TemplateHaskell #-}
module Physics where

import ClassyPrelude
import Control.Lens
import Linear.V3
import Linear.Vector


data Particle d a = Particle { _particleLocation :: V3 a
                             , _particleVelocity :: V3 a
                             , _particleInvInertia :: a
                             , _particleHistory :: [V3 a]
                             , _particleData :: d
                             } deriving Show
makeLenses ''Particle

-- | Semi-implicit Euler stepping function
eulerStepParticle :: Floating a
                  => a -- ^ Time step
                  -> Particle d a -- ^ Particle
                  -> V3 a -- ^ Accelleration
                  -> Particle d a
eulerStepParticle deltaT p accel =
  p
  & particleLocation .~ x1
  & particleVelocity .~ v1
  & particleHistory %~ (p^.particleLocation :)
  where
    v1 = (p^.particleVelocity) ^+^ (accel ^* deltaT)
    x1 = (p^.particleLocation) ^+^ (v1 ^* deltaT)

data Force d a = Force { _forceEqn :: Force d a -> Particle d a -> V3 a
                       , _forceLocation :: V3 a
                       , _forceData :: d }
makeLenses ''Force

evalForce :: Floating a
          => Force d a
          -> Particle d a
          -> V3 a
evalForce f p = (f^.forceEqn) f p


data System d a = System { _systemParticles :: [Particle d a]
                         , _systemForces :: [Force d a]
                         , _systemTime :: a } 
makeLenses ''System

eulerStepSystem :: Floating a
                => a -- ^ Time step
                -> System d a -- ^ The system
                -> System d a
eulerStepSystem deltaT system = system
                                & systemParticles . each %~ process
                                & systemTime +~ deltaT
  where
    process particle = eulerStepParticle deltaT particle 
                       ( sum
                         . map (flip evalForce particle)
                         $ system ^. systemForces )
                       
trimParticleHistory :: Int -> Particle d a -> Particle d a
trimParticleHistory n = particleHistory %~ take n

trimParticleHistories :: Int -> System d a -> System d a
trimParticleHistories n = systemParticles . each . particleHistory %~ take n

