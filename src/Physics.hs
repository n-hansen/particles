{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Physics
  ( Particle (..)
  , particleLocation
  , particleVelocity
  , particleInvInertia
  , particleHistory
  , particleData

  , Force (..)
  , forceEqn
  , forceLocation
  , forceData
  , evalForce

  , System (..)
  , systemParticles
  , systemForces
  , systemTime
  , systemUpdate
  , runSystemUpdate

  , SystemUpdater
  , eulerStepSystem
  , advanceTime
  , trimParticleHistories
  ) where

import ClassyPrelude
import Control.Lens
import Control.Monad.State.Strict
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

-- | We should be able to build up our system updating procedure from
-- composable components.
type SystemUpdater pTag fTag a = ReaderT a (State (System pTag fTag a)) ()

data System pTag fTag a =
  System { _systemParticles :: [Particle pTag a]
         , _systemForces :: [Force fTag a]
         , _systemTime :: a
         , _systemUpdate :: SystemUpdater pTag fTag a }    
makeLenses ''System

runSystemUpdate :: a -> System pTag fTag a -> System pTag fTag a
runSystemUpdate deltaT sys =
  execState (runReaderT (sys^.systemUpdate) deltaT) sys


-- Basic building blocks for systemUpdate:

-- | Perform numerical integration on all particles' velocities
-- and positions.
eulerStepSystem :: Floating a => SystemUpdater pTag fTag a
eulerStepSystem = do
  deltaT <- ask
  forces <- use systemForces
  systemParticles . each %= (\particle ->
                              eulerStepParticle deltaT particle 
                              ( sum
                                . map (flip evalForce particle)
                                $ forces ))

advanceTime :: Num a => SystemUpdater pTag fTag a
advanceTime = do
  deltaT <- ask
  systemTime += deltaT

-- | Trim all particle histories to a given length
trimParticleHistories :: Int -> SystemUpdater pTag fTag a
trimParticleHistories n =
  systemParticles . each . particleHistory %= take n

