module Render where

import ClassyPrelude hiding (point)
import Control.Lens
import Graphics.Rendering.Cairo
import Linear.Matrix
import Linear.Projection
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector


type RGBA = (Double,Double,Double,Double)

drawPath :: RGBA -- ^ Path color
         -> [V2 Double] -- ^ 2d path
         -> Render ()
drawPath _ [] = return ()
drawPath (colorR,colorG,colorB,colorA) (start:remainingPoints) = do
  newPath
  setSourceRGBA colorR colorG colorB colorA
  setLineCap LineCapRound
  setLineJoin LineJoinRound
  moveTo (view _x start) (view _y start)
  forM_ remainingPoints $ \point -> lineTo (view _x point) (view _y point)
  stroke

projectionMatrix :: V3 Double -- ^ Camera location
                 -> V3 Double -- ^ Camera focus
                 -> V3 Double -- ^ Camera up direction
                 -> Double -- ^ Field of view in rads
                 -> Double -- ^ Aspect ratio
                 -> M44 Double
projectionMatrix eye focus up fov aspect = perspectiveM !*! viewM
  where
    perspectiveM = infinitePerspective fov aspect 1
    viewM = lookAt eye focus up

projectPoint :: M44 Double -- ^ Projection matrix
             -> Double -- ^ Canvas width
             -> Double -- ^ Canvas height
             -> V3 Double -- ^ Point to project
             -> V2 Double
projectPoint projM canvasW canvasH = scale . shift . project
  where
    project = view _xy .
              normalizePoint .
              (projM !*) .
              point
    scale = over _x (* (canvasW/2)) . over _y (* (canvasH/2))
    shift = (V2 1 1 ^+^)

projectPath projM canvasW canvasH = map (projectPoint projM canvasW canvasH)


{-
test :: IO ()
test = do
  withImageSurface FormatARGB32 400 400 $ \result -> do
    renderWith result $ do
      let projM = projectionMatrix
                  (V3 0 0 10)
                  (V3 0 0 0)
                  (V3 0 1 10)
                  (pi/3)
                  1
      drawPath (0.9,0.2,0.4,1) $
        projectPath projM 400 400 
        [ V3 0 0 0
        , V3 1 0 0
        , V3 0 1 5 ]
      clip
    surfaceWriteToPNG result "test.png"
-}
