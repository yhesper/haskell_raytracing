module Scene
    ( Ray(..)
    , Camera(..)
    , Intersection(..)
    , Primitive(..)
    , Sphere(..)
    , Triangle(..)
    , Scene(..),
    render,
    rayCastPrimitive,
    cornellBox,
    v3Times,
    updatePrimitive,
    primitiveColor,
    updatePrimitiveColor,
    updatePrimitivePosition,
    ColorChannel(..),
    ) where

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}


import Linear.V3
import Linear.Matrix
import System.Random
import Data.Maybe
import Brick (clamp)

data ColorChannel = CCR | CCG | CCB

updateColorChannel :: Float -> Int -> Float
updateColorChannel c delta = clamp 0 255 (c*255 + (fromIntegral delta :: Float)) / 255

updateColor :: ColorChannel -> V3 Float -> Int -> V3 Float
updateColor CCR (V3 r g b) delta = V3 (updateColorChannel r delta) g b
updateColor CCG (V3 r g b) delta = V3 r (updateColorChannel g delta) b
updateColor CCB (V3 r g b) delta = V3 r g (updateColorChannel b delta)

v3Div :: V3 Float -> Float -> V3 Float
v3Div (V3 x y z) s = V3 (x / s) (y / s) (z / s)

v3Times (V3 x y z) s = V3 (x * s) (y * s) (z * s)

v3Dot (V3 x1 y1 z1) (V3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

v3Add (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 + x2) (y1 + y2) (z1 + z2)

v3Cross (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

v3Norm :: V3 Float -> Float
v3Norm (V3 x y z) = sqrt (x*x + y*y + z*z)

v3Normalize :: V3 Float -> V3 Float
v3Normalize v = v `v3Div` (v3Norm v)

dist :: V3 Float -> V3 Float -> Float
dist (V3 x1 y1 z1) (V3 x2 y2 z2) =
  sqrt ((x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1) + (z2 - z1)*(z2 - z1))

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f xs = map (\(index, element) -> f index element) (zip [0..] xs)

data Ray = Ray {
    origin :: V3 Float,
    direction :: V3 Float,
    max_t :: Float
} deriving (Eq, Show)

data Camera = Camera {
    position :: V3 Float,
    look_at :: V3 Float,
    up :: V3 Float,
    fov :: Float
} deriving (Eq, Show)

data Intersection = Intersection {
    t :: Float,
    prim_idx :: Int,
    normal :: V3 Float,
    color :: V3 Float
} deriving (Eq, Show)

instance Ord Intersection where
  compare i1 i2 = compare (t i1) (t i2)


class Primitive_ a where
  intersect :: Ray -> a -> Int -> Maybe Intersection
  volume :: a -> Float
  area :: a -> Float
  primitiveColor :: a -> V3 Float
  updatePrimitiveColor :: a -> ColorChannel -> Int -> a
  updatePrimitivePosition :: a -> Float -> Float -> a

data Primitive = forall a. Primitive_ a => Primitive a

instance Primitive_ Primitive where
  intersect r (Primitive p) i = intersect r p i
  volume (Primitive p) = volume p
  area (Primitive p) = area p
  primitiveColor (Primitive p) = primitiveColor p
  updatePrimitiveColor (Primitive p) c d = Primitive (updatePrimitiveColor p c d)
  updatePrimitivePosition (Primitive p) dx dy = Primitive (updatePrimitivePosition p dx dy)

data Sphere = Sphere {
    center :: V3 Float,
    radius :: Float,
    sphere_color  :: V3 Float
} deriving (Eq, Show)

instance Primitive_ Sphere where
  intersect r s i =
    let
      l = (center s) - (origin r)
      tca = l `v3Dot` (direction r)
      d2 = (l `v3Dot` l) - (tca * tca)
      r2 = (radius s) * (radius s)
    in
      if (d2 <= r2) then -- && not (tca < 0)then
        let
          thc = sqrt (r2 - d2)
          t0 = tca - thc
          t1 = tca + thc
          intersect_t = if (min t0 t1) >= 0 then min t0 t1 else max t0 t1
        in
          if intersect_t < 0 || intersect_t > max_t r then
            Nothing
          else
            Just Intersection {
              t = intersect_t,
              prim_idx = i,
              normal = v3Normalize (((origin r) + ((direction r) `v3Times` intersect_t)) - (center s)),
              color = sphere_color s
            }
      else
        Nothing
  volume s = 4/3 * pi * (radius s) * (radius s) * (radius s)
  area s = 4 * pi * (radius s) * (radius s)
  primitiveColor s = sphere_color s
  updatePrimitiveColor s chan delta = Sphere (center s) (radius s) (updateColor chan (sphere_color s) delta)
  updatePrimitivePosition s dx dy = Sphere ((center s) `v3Add` (V3 dx dy 0)) (radius s) (sphere_color s)

data Triangle = Triangle {
    v1 :: V3 Float,
    v2 :: V3 Float,
    v3 :: V3 Float,
    triangle_color :: V3 Float
} deriving (Eq, Show)

instance Primitive_ Triangle where 
  intersect r t idx =
    let
      (V3 v1x v1y v1z) = (v1 t)
      (V3 v2x v2y v2z) = (v2 t)
      (V3 v3x v3y v3z) = (v3 t)
      (V3 rdx rdy rdz) = (direction r)
      (V3 rox roy roz) = (origin r)
      a = v1x - v2x
      b = v1y - v2y
      c = v1z - v2z
      d = v1x - v3x
      e = v1y - v3y
      f = v1z - v3z
      g = rdx
      h = rdy
      i = rdz
      j = v1x - rox
      k = v1y - roy
      l = v1z - roz
      eihf = e * i - h * f
      gfdi = g * f - d * i
      dheg = d * h - e * g
      denom = a * eihf + b * gfdi + c * dheg
      beta = (j * eihf + k * gfdi + l * dheg) / denom
      akjb = a * k - j * b
      jcal = j * c - a * l
      blkc = b * l - k * c
      gamma = (i * akjb + h * jcal + g * blkc) / denom
      intersect_t = -(f * akjb + e * jcal + d * blkc) / denom
      normal = v3Normalize (v3Cross (v2 t - v1 t) (v3 t - v1 t))
    in
      if beta <= 0.0 || beta >= 1.0 || gamma <= 0.0 || beta + gamma >= 1.0 || intersect_t < 0.0 || intersect_t > max_t r then
        Nothing
      else
        Just Intersection {
          t = intersect_t,
          prim_idx = idx,
          normal = if v3Dot normal (direction r) > 0 then v3Times normal (-1) else normal,
          color = triangle_color t
        }
  volume t = undefined
  area t = undefined
  primitiveColor t = triangle_color t
  updatePrimitiveColor t _ _ = t
  updatePrimitivePosition t _ _ = t

data PointLight = PointLight
  { lightPosition :: V3 Float  -- Position of the light source
  , lightColor :: V3 Float      -- Color of the light
  , lightIntensity :: Float  -- Intensity of the light
  } deriving (Show)

data Scene = Scene {
    primitives :: [Primitive],
    lights     :: [Primitive]
}

sphere_y :: Float
sphere_y = -1.8

cornellBox :: Scene
pps = [Primitive sphereYellow, Primitive sphereGreen, Primitive spherePurple, Primitive backWallTriangle0, Primitive backWallTriangle1, Primitive topWallTriangle0, Primitive topWallTriangle1, Primitive botWallTriangle0, Primitive botWallTriangle1, Primitive leftWallTriangle0, Primitive leftWallTriangle1, Primitive rightWallTriangle0, Primitive rightWallTriangle1]
lts = [Primitive sphereWhite]
cornellBox = Scene pps lts
diffuseWhiteColor = V3 1 1 1
diffuseBlueColor = V3 0 0 0.75
diffuseRedColor = V3 0.75 0 0
tri = Triangle (V3 0 (-1) (-2)) (V3 0 (-1) 1) (V3 3 (-1) 1) (V3 0 1 0)
sphereYellow = Sphere (V3 1 (sphere_y+1) (-2)) 1 (V3 0 1 1)
sphereGreen = Sphere (V3 0 (sphere_y-1) (-2)) 1 (V3 0 1 0)
spherePurple = Sphere (V3 2.5 (sphere_y-1) (-3)) 1 (V3 0.75 0 1)
sphereWhite = Sphere (V3 0 (sphere_y+5) (-1)) 0.3 (V3 1 1 1)

backWallTriangle0 = Triangle (V3 (10) 10 (-4)) (V3 (-10) 10 (-4)) (V3 (-10) (-10) (-4)) diffuseWhiteColor
backWallTriangle1 = Triangle (V3 (10) 12 (-4)) (V3 (-10) (-8) (-4)) (V3 10 (-8) (-4)) diffuseWhiteColor
topWallTriangle0 = Triangle  (V3 (10) 4 (-4)) (V3 (-10) 4 (-4)) (V3 10 4 8) diffuseWhiteColor
topWallTriangle1 = Triangle  (V3 (10) 4 (-4)) (V3 (-10) 4 (-4)) (V3 (-10) 4 8) diffuseWhiteColor
botWallTriangle0 = Triangle  (V3 (10) (-4) (-4)) (V3 (-10) (-4) (-4)) (V3 10 (-4) 8) diffuseWhiteColor
botWallTriangle1 = Triangle  (V3 (10) (-4) (-4)) (V3 (-10) (-4) (-4)) (V3 (-10) (-4) 8) diffuseWhiteColor

rightWallTriangle0 = Triangle  (V3 (4) 10 (-10)) (V3 (4) (-10) (-10)) (V3 4 (10) (10)) diffuseRedColor
rightWallTriangle1 = Triangle  (V3 4 (12) (10)) (V3 (4) (-8) (-10)) (V3 4 (-8) (10)) diffuseRedColor

leftWallTriangle0 = Triangle  (V3 (-4) 10 (-10)) (V3 (-4) (-10) (-10)) (V3 (-4) (10) (10)) diffuseBlueColor
leftWallTriangle1 = Triangle  (V3 (-4) (12) (10)) (V3 (-4) (-8) (-10)) (V3 (-4) (-8) (10)) diffuseBlueColor


updatePrimitive :: Scene -> Int -> Primitive -> Scene
updatePrimitive s prim_id new_prim =
  let
    (left, right) = splitAt prim_id (primitives s)
  in
    Scene (left ++ [new_prim] ++ (tail right)) (lights s)

rayCastPrimitive :: Scene -> (Int, Int) -> (Int, Int) -> Maybe Int
rayCastPrimitive s (img_x, img_y) (w, h) = do
  let cameraFrame = setupCameraFrame w h
  let ray = raygen cameraFrame img_x img_y
  intersection <- closestHit ray s
  return $ prim_idx intersection

closestHit :: Ray -> Scene -> Maybe Intersection
closestHit r s =
  let
    intersections = mapWithIndex (flip (intersect r)) (primitives s)
    valid_intersections = catMaybes intersections
  in
    if not (null valid_intersections) then
      Just (minimum valid_intersections)
    else
      Nothing

anyHit :: Ray -> [Primitive] -> Bool
anyHit r ps =
  let
    intersections = mapWithIndex (flip (intersect r)) (ps)
    valid_intersections = catMaybes intersections
  in
    not (null valid_intersections)

type CameraFrame = (V3 Float, V3 Float, (Float, Float))

setupCameraFrame :: Int -> Int -> CameraFrame
setupCameraFrame w h =
  let
    ww = fromIntegral w :: Float
    hh = fromIntegral h :: Float
    aspect_ratio = ww / hh
    viewport_height = 2.0
    viewport_width = viewport_height * aspect_ratio
    focal_length = 1.0
    camera_center = V3 0 0 4
    viewport_upper_left = camera_center - V3 (viewport_width / 2) (viewport_height / 2) focal_length
    pixel00_loc = viewport_upper_left + V3 (viewport_width / ww) 0 0 + V3 0 (viewport_height / hh) 0
    pixel_delta_u = (viewport_width / ww)
    pixel_delta_v = (viewport_height / hh)
  in
    (camera_center, pixel00_loc, (pixel_delta_u, pixel_delta_v))

raygen :: CameraFrame -> Int -> Int -> Ray
raygen (camera_center, pixel00_loc, (pixel_delta_u, pixel_delta_v)) x y =
  let pixel_loc = pixel00_loc + (V3 (pixel_delta_u * (fromIntegral x + 0.5)) (pixel_delta_v * (fromIntegral y + 0.5)) 0)
      ray = Ray camera_center (v3Normalize (pixel_loc - camera_center)) 1e99
  in
    ray

maxDepth = 3

render :: Scene -> Int -> Int -> Int -> [V3 Float]
render s w h sampleIdx =
  let cameraFrame = setupCameraFrame w h
  in
    do
      y <- [0..h-1]
      x <- [0..w-1]
      let ray = raygen cameraFrame x y
      let seed = sampleIdx * 32 * w * h * maxDepth + y * w * maxDepth + x * maxDepth
      return $ traceRay ray s (maxDepth - 1) seed

calculatePointLightIntensity :: PointLight -> V3 Float -> Float
calculatePointLightIntensity light point =
  let distance = dist (lightPosition light) point
  in lightIntensity light / (distance * distance)


nee :: Scene -> PointLight -> V3 Float -> V3 Float
nee s light hitPoint = do
  let lightDirection = v3Normalize (lightPosition light - hitPoint)
  let lightDistance = dist (lightPosition light) hitPoint
  let shadowRay = Ray (hitPoint + lightDirection `v3Times` 1e-3) lightDirection lightDistance
  if anyHit shadowRay (primitives s) then
    V3 0 0 0
  else
    lightColor light `v3Times` lightIntensity light

traceRay :: Ray -> Scene -> Int -> Int -> V3 Float
traceRay r s d seed =
  if d == maxDepth - 1 && anyHit r (lights s) then do
    V3 100 100 100
  else
    case closestHit r s of
      Just intersection -> do
        let hitColor = color intersection
        let hitPoint = origin r + (direction r `v3Times` t intersection)
        let hitNormal = normal intersection
        let light = PointLight (center sphereWhite) (V3 1 1 1) 100
        let lightDirection = v3Normalize (lightPosition light - hitPoint)
        let lightDistance = dist (lightPosition light) hitPoint
        let neeColor = (hitColor / pi * (nee s light hitPoint `v3Times` max 0 (hitNormal `v3Dot` lightDirection))) `v3Times` (1.0 / (lightDistance*lightDistance))

        let brdf = hitColor
        if d == 0 then do
            neeColor
        else do
          let rm = rotationMatrix hitNormal
          let newDirection = randomDirection $ mkStdGen (seed + d)
          let newDirectionRotated = rm !* newDirection
          let newRay = Ray (hitPoint + newDirectionRotated `v3Times` 1e-3) (v3Normalize newDirectionRotated) 1e99
          brdf * traceRay newRay s (d - 1) seed + neeColor
      Nothing -> V3 0 0 0

randomDirection :: StdGen -> V3 Float
randomDirection gen =
  let
    (u, gen1) = random gen
    (v, gen2) = random gen1
    sx = 2 * u - 1
    sy = 2 * v - 1
    r = if abs sx > abs sy then sx else sy
    theta = if abs sx > abs sy then pi / 4 * (sy / sx) else pi / 2 - pi / 4 * (sx / sy)
    x = r * cos theta
    y = r * sin theta
    z = sqrt (max 0 (1 - x * x - y * y))
  in
    V3 x y z

rotationMatrix :: V3 Float -> M33 Float
rotationMatrix (V3 xx yy zz)
  | abs xx <= abs yy && abs xx <= abs zz = let
                                          z = V3 xx yy zz
                                          h = V3 1 yy zz
                                          y = v3Normalize (h `v3Cross` z)
                                          x = v3Normalize (z `v3Cross` y)
                                        in
                                          V3 x y z
  | abs yy <= abs xx && abs yy <= abs zz = let
                                          z = V3 xx yy zz
                                          h = V3 xx 1 zz
                                          y = v3Normalize (h `v3Cross` z)
                                          x = v3Normalize (z `v3Cross` y)
                                        in
                                          V3 x y z
  | otherwise = let
                  z = V3 xx yy zz
                  h = V3 xx yy 1
                  y = v3Normalize (h `v3Cross` z)
                  x = v3Normalize (z `v3Cross` y)
                in
                  V3 x y z
