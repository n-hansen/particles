{-# LANGUAGE LambdaCase #-}
module Render
  ( RGBA
  , projectionMatrix
--  , drawPathCairo
  , drawSystemGloss  
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

import Physics


--- What we can render... ---

type RGBA a = (a,a,a,a)

data Renderable v a = Path (RGBA a) [v a]
                    | Node (RGBA a) (v a)

mapRPoint :: (u a -> v a) -> Renderable u a -> Renderable v a
mapRPoint f (Path c vs) = Path c (map f vs)
mapRPoint f (Node c v) = Node c (f v)

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


--- Drawing many things at once ---

-- | Divide a list into overlapping chunks
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks chunkSize xs | chunkSize < 2 = [xs]
                    | otherwise = front : chunks chunkSize back
  where
    front = take chunkSize xs
    back = drop (chunkSize-1) xs

-- | Project a bunch of items based on a provided matrix, break paths into chunks,
-- then sort all items based on their z-dimension and render them with the provided
-- function.
renderMany :: (Floating a, Ord a)
           => Int -- ^ Segment size (to handle overlap)
           -> (Renderable V2 a -> r) -- ^ Rendering function
           -> M44 a -- ^ Projection matrix
           -> [Renderable V3 a] -- ^ Paths to render
           -> [r]
renderMany segmentSize render projM renderables =
  map render orderedRenderables
  where
    orderedRenderables = map (mapRPoint $ view _xy)
                         . sortBy zCompare
                         $ unorderedRenderables                   
    zCompare (Path _ []) (Path _ []) = EQ
    zCompare (Path _ []) _ = LT
    zCompare _ (Path _ []) = GT
    zCompare a b =
      let za = case a of
            Path _ ((V3 _ _ z): _ ) -> z
            Node _ (V3 _ _ z) -> z
          zb = case b of
            Path _ ((V3 _ _ z): _ ) -> z
            Node _ (V3 _ _ z) -> z
      in compare za zb
    unorderedRenderables =
      concatMap
      (\case
          (Path color pts) -> map (Path color)
                              . chunks segmentSize
                              $ pts
          r -> [r] )
      . map (mapRPoint (projectPoint projM))
      $ renderables

-- | Render a system which has been tagged with color data.
renderSystem :: (Floating a, Ord a)
             => Int -- ^ Path segment size
             -> (Renderable V2 a -> r) -- ^ Rendering function
             -> M44 a -- ^ Projection matrix
             -> System (RGBA a) a -- ^ System to render
             -> [r]
renderSystem segmentSize render projM system =
  renderMany segmentSize render projM renderables
  where
    renderables = mconcat [particles, forces]
    particles = map
                (\p -> Path
                       (p^.particleData)
                       (p^.particleLocation : p^.particleHistory) )
                (system^.systemParticles)
    forces = map
             (\f -> Node
                    (f^.forceData)
                    (f^.forceLocation) )
             (system^.systemForces)


--- Cairo rendering ---

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

-- | Transform to gloss screen coordinates
toGlossCoords (width,height) (V2 x y) = (x*width/2,y*height/2)

-- | Make a gloss color out of a tuple
toGlossColor (colorR,colorG,colorB,colorA) =
  makeColor colorR colorG colorB colorA

-- | Draw a thing in gloss
drawGloss :: (Float,Float) -- ^ canvas (width,height)
             -> Renderable V2 Float -- ^ Thing to draw
             -> Picture
drawGloss dims (Path c path) = color myColor myPath
  where
    myColor = toGlossColor c
    myPath = line $
             map (toGlossCoords dims)
             path
drawGloss dims (Node c loc) =
  color (toGlossColor c)
  . uncurry Graphics.Gloss.translate (toGlossCoords dims loc)
  $ circle 5

drawSystemGloss :: Int -- ^ Segment size
                -> M44 Float -- ^ Projection matrix
                -> (Float,Float) -- ^ Width, height
                -> System (RGBA Float) Float -- ^ System to draw
                -> Picture
drawSystemGloss segmentSize projM dims =
  pictures . renderSystem segmentSize (drawGloss dims) projM
