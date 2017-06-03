module Render
  ( RGBA
  , projectionMatrix
--  , projectPoint
  , drawPathCairo
  , drawPathsGloss
  ) where

import ClassyPrelude hiding (point)
import Control.Lens
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
             -> V3 a
projectPoint projM = normalizePoint
                     . (projM !*)
                     . point                    

projectPoint' :: Floating a
              => M44 a -- ^ Projection matrix
              -> V3 a -- ^ Point to project
              -> V2 a
projectPoint' = (view _xy .) <$> projectPoint


--- Path drawing ---

-- | Divide a list into overlapping chunks
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks chunkSize xs | chunkSize < 2 = [xs]
                    | otherwise = front : chunks chunkSize back
  where
    front = take chunkSize xs
    back = drop (chunkSize-1) xs

-- | Project a bunch of paths based on a provided matrix, break them into chunks,
-- then sort them based on their z-dimension and render them with the provided
-- renderer.
renderPaths :: (Floating a, Ord a)
            => Int -- ^ Segment size (to handle overlap)
            -> (RGBA a -> [V2 a] -> r) -- ^ Rendering function
            -> M44 a -- ^ Projection matrix
            -> [(RGBA a,[V3 a])] -- ^ Paths to render
            -> [r]
renderPaths segmentSize render projM paths = map (uncurry render) orderedSegments
  where
    orderedSegments = map ( second $ map ( view _xy ) )
                      . sortBy compareSegment
                      $ unorderedSegments                    
    compareSegment (_, a) (_, b) =
      case (a, b) of
        ([], _ ) -> LT
        (_, [] ) -> GT
        ((V3 _ _ z1:_ ), (V3 _ _ z2:_ )) -> compare z1 z2
    unorderedSegments =
      concatMap
      (\(color,path) -> map ((,) color)
                        . chunks segmentSize
                        . map (projectPoint projM)
                        $ path )
      paths
            

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
  let (start:rest) = map (\pt -> let pt' = projectPoint' projM pt
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

-- | Draw a 3-d path given a projection matrix.
drawPathGloss' :: M44 Float -- ^ Projection matrix
               -> (Float,Float) -- ^ (width,height)
               -> RGBA Float -- ^ Path color
               -> [V3 Float] -- ^ 2d path
               -> Picture
drawPathGloss' projM (width,height) (colorR,colorG,colorB,colorA) path =
  color myColor myPath
  where
    myColor = makeColor colorR colorG colorB colorA
    myPath = line $
             map ( (\(V2 x y) -> (x*width/2, y*height/2)) .
                   projectPoint' projM )
             path

-- | Draw a 2-d path.
drawPathGloss :: (Float,Float) -- ^ (width,height)
              -> RGBA Float -- ^ Path color
              -> [V2 Float] -- ^ 2d path, coordinates [-1..1]
              -> Picture
drawPathGloss (width,height) (colorR,colorG,colorB,colorA) path =
  color myColor myPath
  where
    myColor = makeColor colorR colorG colorB colorA
    myPath = line $
             map (\(V2 x y) -> (x*width/2, y*height/2))
             path

-- | Draw a bunch of paths together, segmenting them so that overlaps are
-- correctly drawn.
drawPathsGloss :: Int -- ^ Segment size
               -> M44 Float -- ^ Projection matrix
               -> (Float,Float) -- ^ Width, height
               -> [(RGBA Float, [V3 Float])] -- ^ Paths to draw
               -> Picture
drawPathsGloss segmentSize projM dims =
  pictures . renderPaths segmentSize (drawPathGloss dims) projM
