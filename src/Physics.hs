{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Physics where

import ClassyPrelude
import Control.Lens
import Linear.V3
import Linear.Vector


data Particle pTag a = Particle { _particleLocation :: V3 a
                                , _particleVelocity :: V3 a
                                , _particleInvInertia :: a
                                , _particleHistory :: [V3 a]
                                , _particleData :: pTag }
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

data Force fTag a =
  Force { _forceEqn :: forall pTag .
                       Force fTag a
                    -> Particle pTag a
                    -> V3 a
        , _forceLocation :: V3 a
        , _forceData :: fTag }
makeLenses ''Force

evalForce :: Floating a
          => forall pTag .
             Force fTag a
          -> Particle pTag a
          -> V3 a
evalForce f p = (f^.forceEqn) f p


data System pTag fTag a =
  System { _systemParticles :: [Particle pTag a]
         , _systemForces :: [Force fTag a]
         , _systemTime :: a } 
makeLenses ''System

eulerStepSystem :: Floating a
                => a -- ^ Time step
                -> System pTag fTag a -- ^ The system
                -> System pTag fTag a
eulerStepSystem deltaT system = system
                                & systemParticles . each %~ process
                                & systemTime +~ deltaT
  where
    process particle = eulerStepParticle deltaT particle 
                       ( sum
                         . map (flip evalForce particle)
                         $ system ^. systemForces )
                       
trimParticleHistory :: Int -> Particle pTag a -> Particle pTag a
trimParticleHistory n = particleHistory %~ take n

trimParticleHistories :: Int -> System pTag fTag a -> System pTag fTag a
trimParticleHistories n = systemParticles . each . particleHistory %~ take n

