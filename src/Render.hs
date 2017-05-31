module Render
  ( RGBA
  , projectionMatrix
  , projectPoint
  , drawPathCairo
  , drawPathGloss
  ) where

import ClassyPrelude hiding (point)
import Control.Lens
import GHC.Float (double2Float)
import Graphics.Gloss
import Graphics.Rendering.Cairo
import Linear.Epsilon (Epsilon)
import Linear.Matrix
import Linear.Projection
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector

--- Linear algebra stuff ---

projectionMatrix :: (Floating a, Epsilon a)
                 => V3 a -- ^ Camera location
                 -> V3 a -- ^ Camera focus
                 -> V3 a -- ^ Camera up direction
                 -> a -- ^ Field of view in rads
                 -> a -- ^ Aspect ratio
                 -> M44 a
projectionMatrix eye focus up fov aspect = perspectiveM !*! viewM
  where
    perspectiveM = infinitePerspective fov aspect 1
    viewM = lookAt eye focus up

projectPoint :: Floating a
             => M44 a -- ^ Projection matrix
             -> V3 a -- ^ Point to project
             -> V2 a
projectPoint projM = view _xy .
                     normalizePoint .
                     (projM !*) .
                     point


--- Cairo rendering ---

type RGBA a = (a,a,a,a)

drawPathCairo :: M44 Double -- ^ Projection matrix
              -> (Double,Double) -- ^ (width,height)
              -> RGBA Double -- ^ Path color
              -> [V3 Double] -- ^ 3d path
              -> Render ()
drawPathCairo _ _ _ [] = return ()
drawPathCairo projM (width,height) (colorR,colorG,colorB,colorA) path = do
  newPath
  setSourceRGBA colorR colorG colorB colorA
  setLineCap LineCapRound
  setLineJoin LineJoinRound
  let (start:rest) = map (\pt -> let pt' = projectPoint projM pt
                                 in ((view _x pt + 1) * width/2,
                                     (view _y pt + 1) * height/2))
                     path
  uncurry moveTo start
  forM_ rest $ uncurry lineTo
  stroke

cairoTest :: IO ()
cairoTest = do
  withImageSurface FormatARGB32 400 400 $ \result -> do
    renderWith result $ do
      let projM = projectionMatrix
                  (V3 0 0 10)
                  (V3 0 0 0)
                  (V3 0 1 10)
                  (pi/3)
                  1
      drawPathCairo projM (400,400) (0.1,0.8,0.4,1) $
        [ V3 0 0 0
        , V3 1 0 0
        , V3 0 1 5
        , V3 (-0.5) (-0.5) 0]
      clip
    surfaceWriteToPNG result "test.png"


--- Gloss rendering ---
drawPathGloss :: M44 Float -- ^ Projection matrix
              -> (Float,Float) -- ^ (width,height)
              -> RGBA Float -- ^ Path color
              -> [V3 Float] -- ^ 2d path
              -> Picture
drawPathGloss projM (width,height) (colorR,colorG,colorB,colorA) path =
  color myColor myPath
  where
    myColor = makeColor colorR colorG colorB colorA
    myPath = line $
             map ( (\(V2 x y) -> (x*width/2, y*height/2)) .
                   projectPoint projM )
             path

glossTest :: IO ()
glossTest = display d black p
  where
    d = InWindow "test" (400,400) (10,10)
    p = drawPathGloss projM (400,400) (0.8,0.5,0.0,1.0) $
        [ V3 0 0 0
        , V3 1 0 0
        , V3 0 1 5 ]
    projM = projectionMatrix
            (V3 0 0 10)
            (V3 0 0 0)
            (V3 0 1 10)
            (pi/3)
            1

