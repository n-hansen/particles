{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Physics
  ( Particle ()
  , makeParticle
  , particleLocation
  , particleVelocity
  , particleInvInertia
  , particleHistory
  , particleData

  , Force ()
  , makeForce
  , forceEqn
  , forceLocation
  , forceData
  , evalForce

  , System ()
  , makeSystem
  , systemParticles
  , systemForces
  , systemTime
  , systemData
  , systemUpdate
  , runSystemUpdate

  , SystemUpdater
  , eulerStepSystem
  , advanceTime
  , trimParticleHistories
  ) where

import ClassyPrelude
import Control.Lens hiding (cons)
import Control.Monad.State.Strict
import Linear.V3
import Linear.Vector


data Particle pData a = Particle { _particleLocation :: V3 a
                                , _particleVelocity :: V3 a
                                , _particleInvInertia :: a
                                , _particleHistory :: Seq (V3 a)
                                , _particleData :: pData }
makeLenses ''Particle

makeParticle :: (a,a,a) -- ^ Position
             -> (a,a,a) -- ^ Velocity
             -> a -- ^ Inverse inertia
             -> pData -- ^ Data
             -> Particle pData a
makeParticle (x,y,z) (vx,vy,vz) invi d =
  Particle (V3 x y z) (V3 vx vy vz) invi mempty d

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
  & particleHistory %~ (cons $ p^.particleLocation)
  where
    v1 = (p^.particleVelocity) ^+^ (accel ^* deltaT)
    x1 = (p^.particleLocation) ^+^ (v1 ^* deltaT)

data Force fData a =
  Force { _forceEqn :: forall pData .
                       Force fData a
                    -> Particle pData a
                    -> V3 a
        , _forceLocation :: V3 a
        , _forceData :: fData }
makeLenses ''Force

makeForce :: (forall pData. Force fData a -> Particle pData a -> V3 a)
             -- ^ Force strength equation
          -> (a,a,a) -- ^ Force location
          -> fData -- ^ Data
          -> Force fData a
makeForce eqn (x,y,z) d =
  Force eqn (V3 x y z) d
             
evalForce :: Floating a
          => forall pData .
             Force fData a
          -> Particle pData a
          -> V3 a
evalForce f p = (f^.forceEqn) f p

-- | We should be able to build up our system updating procedure from
-- composable components.
type SystemUpdater pData fData sData a =
  ReaderT a (State (System pData fData sData a)) ()

data System pData fData sData a =
  System { _systemParticles :: [Particle pData a]
         , _systemForces :: [Force fData a]
         , _systemTime :: a
         , _systemData :: sData
         , _systemUpdate :: SystemUpdater pData fData sData a }    
makeLenses ''System

makeSystem :: Num a
           => [Particle pData a]
           -> [Force fData a]
           -> sData
           -> SystemUpdater pData fData sData a
           -> System pData fData sData a
makeSystem particles forces d updater =
  System particles forces 0 d updater

runSystemUpdate :: a
                -> System pData fData sData a
                -> System pData fData sData a
runSystemUpdate deltaT sys =
  execState (runReaderT (sys^.systemUpdate) deltaT) sys


-- Basic building blocks for systemUpdate:

-- | Perform numerical integration on all particles' velocities
-- and positions.
eulerStepSystem :: Floating a => SystemUpdater pData fData sData a
eulerStepSystem = do
  deltaT <- ask
  forces <- use systemForces
  systemParticles . each %= (\particle ->
                              eulerStepParticle deltaT particle 
                              ( sum
                                . map (flip evalForce particle)
                                $ forces ))

advanceTime :: Num a => SystemUpdater pData fData sData a
advanceTime = do
  deltaT <- ask
  systemTime += deltaT

-- | Trim all particle histories to a given length
trimParticleHistories :: Int -> SystemUpdater pData fData sData a
trimParticleHistories n =
  systemParticles . each . particleHistory %= take n

